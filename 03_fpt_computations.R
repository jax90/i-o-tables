x = c("tidyverse","tibble","progress","arrow","curl")
lapply(x, library,character.only = T)

# --- Initial Data Loading ---

message("Loading and aggregating IOT and Emissions data...")

if(!exists('data_directory')) data_directory = tempdir(check = T)

values_agg <- format_iot(
  folder = data_directory,
  exdir = data_directory,
  update = FALSE,
  edition = edition
) %>%
  filter(time_period <= as.integer(end_year) & time_period >= as.integer(start_year))

emissions <- format_emissions(
  folder = data_directory,
  exdir = data_directory,
  update = FALSE,
  edition = edition
) %>%
  unite(resource_id, ref_area, industry, sep = "_") |>
  group_by(resource_id, time_period) %>%
  summarise(direct_emissions = sum(obs_value, na.rm = TRUE),.groups = 'drop') %>%
  mutate(time_period = as.character(time_period)) %>%
  ungroup() %>%
  group_by(time_period) %>%
  mutate(absolute_emissions = sum(direct_emissions, na.rm = TRUE)) %>%
  ungroup()  %>%
  filter(time_period <= as.integer(end_year) & time_period >= as.integer(start_year))


# --- Core EEIO Computation Function (for a single year) ---
eeio_computations <- function(input_output,
                              emissions_year,
                              verbose = TRUE) {
  if (verbose) message("    Initializing EEIO variables...")

  # --- Input-Output Variable Definitions ---

  X <- rowSums(input_output, na.rm = TRUE) # Total Output vector

  # Emission intensity vector (direct emissions per unit of output)
  B <- emissions_year %>%
    mutate(production = X[match(resource_id, names(X))]) %>%
    reframe(value = if_else(production == 0, 0, direct_emissions / production))

  Ds <- input_output[, grepl('P3|P5', colnames(input_output))] %>% as.matrix() # Distributed Final Demand matrix
  D <- rowSums(Ds, na.rm = TRUE) # Aggregated Final Demand vector
  Z <- input_output %>% select(all_of(rownames(.))) %>% as.matrix() # Intermediate Transactions Matrix

  # In IO models, a sector cannot consume its own entire output for intermediate use.
  diag(Z)[diag(Z) == colSums(Z, na.rm = TRUE)] <- 0

  # Technical Coefficients Matrix (A = Z * (X_hat)^-1)
  A <- sweep(Z, 2, as.numeric(X), `/`)
  A[is.nan(A) | is.infinite(A)] <- 0 # Handle potential division by zero

  if (verbose) message("    Inverting Leontief matrix (I-A)...")
  L <- solve(diag(nrow = nrow(A)) - A)

  # --- Production Footprint Computation using Structural Decomposition ---
  # This section isolates the impact of specific (ICT) sectors.
  if (verbose) message("    Decomposing production to compute ICT footprint...")

  L_adjust <- L
  Int <- matrix(0, nrow = dim(L_adjust)[1], ncol = dim(L_adjust)[1])

  selected_industry_num <- grep('26|61|62|63', colnames(x = L_adjust))

  for (i in selected_industry_num) {
    Int <- Int + (1 / L_adjust[i, i] * L_adjust[, i] %*% t(L_adjust[i, ]))
  }

  L_adjust <- L - Int

  # --- Final Demand Footprint Computation ---
  if (verbose) message("    Computing final demand footprints...")
  marginal_carbon_per_demand <- t(B) %*% Int

  distributed_fd_fpt_list <- lapply(1:ncol(Ds), function(i) {
    embedded_emissions_di <- t(sweep(marginal_carbon_per_demand, 2, Ds[, i], `*`)) %>%
      as.data.frame()
    colnames(embedded_emissions_di) <- paste0("embodied_emissions_", colnames(Ds)[i])
    return(embedded_emissions_di)
  })

  distributed_fd_fpt <- do.call(cbind, distributed_fd_fpt_list) %>%
    rownames_to_column('resource_id')

  # Decomposing the ICT sector footprint (production-side)
  sub_invL <- solve(L[selected_industry_num, selected_industry_num])
  prescaling_sub_prod <- sub_invL %*% X[selected_industry_num, drop = FALSE]
  full_prod <- L[, selected_industry_num] %*% diag(x = as.numeric(prescaling_sub_prod))
  distributed_fpt <- diag(x = unlist(B)) %*% full_prod
  total_fpt <- colSums(distributed_fpt, na.rm = TRUE)

  # --- Scope 1, 2, and 3 Emissions for ICT Industries ---
  if (verbose) message("    Computing Scope 1, 2, and 3 for individual ICT industries...")
  scope1 <- list()
  scope2 <- list()
  scope3 <- list()

  for (industry in c("C26", "J61", "J62_63")) {
    scope1[[industry]] <- sum(emissions_year$direct_emissions[grepl(industry, emissions_year$resource_id)])

    scope2[[industry]] <- input_output |>
      as_tibble(rownames = "industry_ref_area") %>%
      filter(grepl("D35", industry_ref_area)) %>%
      select(matches(industry)) %>%
      mutate(across(everything(), ~ .x * B$value[grepl("D35", emissions_year$resource_id)][1])) %>%
      sum()

    # Embodied emissions calculation specific to this industry
    Int_i <- matrix(0, nrow = dim(L)[1], ncol = dim(L)[1])
    selected_industry_num_i <- grep(industry, colnames(x = L))
    for (i in selected_industry_num_i) {
      Int_i <- Int_i + (1 / L[i, i] * L[, i] %*% t(L[i, ]))
    }

    scope3[[industry]] <- t(B) %*% Int_i %*% as.matrix(D) - scope1[[industry]] - scope2[[industry]]
  }

  # --- Formatting Final Results Table ---
  if (verbose) message("    Formatting final results table...")
  results_table <- L %>%
    as.data.frame() %>%
    rownames_to_column('resource_id') %>%
    left_join(as.data.frame(Ds) %>% rownames_to_column('resource_id'), by = 'resource_id') %>%
    left_join(as.data.frame(Int) %>% `colnames<-`(paste0("Intwght_", colnames(.))) %>% rownames_to_column('resource_id'), by = 'resource_id') %>%
    mutate(
      total_final_demand = D,
      total_output = X,
      direct_emissions = emissions_year$direct_emissions[match(resource_id, emissions_year$resource_id)],
      absolute_emissions = emissions_year$absolute_emissions[1], # Total emissions for the year
      scope1_C26 = scope1[["C26"]], scope2_C26 = scope2[["C26"]], scope3_C26 = scope3[["C26"]],
      scope1_J61 = scope1[["J61"]], scope2_J61 = scope2[["J61"]], scope3_J61 = scope3[["J61"]],
      scope1_J62_63 = scope1[["J62_63"]], scope2_J62_63 = scope2[["J62_63"]], scope3_J62_63 = scope3[["J62_63"]]
    ) %>%
    left_join(distributed_fd_fpt, by = 'resource_id')

  return(results_table)
}


# --- Main Analysis Orchestrator Function (loops over all years) ---
eeio_analysis <- function(values_agg,
                          emissions,
                          file_name,
                          exdir,
                          deflate = TRUE,
                          basis = as.integer(end_year), # Base year for deflation.
                          update = TRUE,
                          verbose = TRUE) {

  output_file <- file.path(exdir, file_name)

  if (!update && file.exists(output_file)) {
    if (verbose) message("Returning cached results from: ", output_file)
    return(read_parquet(output_file))
  }
  if (verbose) message("Cache not found or update forced. Starting new analysis...")

  # --- Optional Deflation Block ---
  if (deflate) {
    if (is.null(basis) || !is.numeric(basis)) {
      stop("'basis' must be a valid numeric year when deflate = TRUE.")
    }
    if (verbose) message(paste0("Deflating economic data to constant ", basis, " prices..."))


    price_index <- get_value_added_price_index(basis, update = FALSE, verbose = verbose) |>
      rename(deflator = value, ref_area = country, time_period = year) |>
      select(-base) %>%
      filter(time_period %in% unique(values_agg$time_period))

    if("resource_id" %in% colnames(values_agg)) values_agg = values_agg %>% rename(rowLabels = resource_id)

    values_agg <- values_agg %>%
      separate(rowLabels, into = c('ref_area', 'industry'), sep = "_", remove = FALSE, extra = 'merge') %>%
      full_join(price_index, by = c("ref_area", "industry", "time_period"), relationship = "many-to-many") |>
      mutate(across(where(is.numeric), ~ .x * deflator)) |>
      select(-c("ref_area", "industry", "deflator"))

    if (verbose) message("Deflation complete.")
  } else {
    if (verbose) message("Skipping deflation. Analysis will be run on nominal (current price) data.")
  }

  time_periods <- unique(values_agg$time_period)

  results_list <- vector("list", length(time_periods))
  names(results_list) <- time_periods

  if (verbose) message("Starting EEIO analysis for each time period...")

  pb <- progress::progress_bar$new(
    format = "  Processing year :what [:bar] :percent | ETA: :eta",
    total = length(time_periods), clear = FALSE, width = 70
  )

  for (i in seq_along(time_periods)) {
    current_period <- time_periods[i]
    pb$tick(tokens = list(what = current_period))

    input_output <- values_agg %>%
      filter(time_period == current_period) |>
      select(-time_period) %>%
      filter(rowLabels %in% colnames(.)) %>%
      column_to_rownames('rowLabels')

    emissions_year <- emissions %>%
      filter(time_period == current_period, !grepl("HH", resource_id)) %>%
      arrange(resource_id)

    results <- eeio_computations(input_output, emissions_year, verbose) %>%
      mutate(time_period = current_period)

    results_list[[i]] <- results
  }

  if (verbose) message("\nAnalysis complete. Aggregating all yearly results...")
  final_results <- dplyr::bind_rows(results_list)

  if (verbose) message("Writing final results to cache: ", output_file)
  write_parquet(final_results, output_file)

  return(final_results)
}


# --- Execute the Analysis ---
# The main function call now explicitly shows the deflation choice.
eeio_analysis(
  values_agg = values_agg,
  emissions = emissions,
  file_name = paste0("footprint_results_", edition, "_data.parquet"),
  exdir = data_directory,
  deflate = TRUE,
  basis = as.integer(end_year),
  update = TRUE
)
