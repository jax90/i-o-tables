x = c('dplyr', 'purrr', 'data.table', 'arrow', 'progress', 'rvest', 'stringr','eurostat','curl')
lapply(x, library, character.only = TRUE)


# Generic Data Download Function ---


download_figaro_data <- function(folder, verbose) {

  figaro_country_list = c("AR","AT","AU","BE","BG","BR","CA","CH","CN","CY","CZ","DE","DK","EE","ES",
                          "FI","FIGW1","FR","GB","GR","HR","HU","ID","IE","IN","IT","JP","KR","LT","LU",
                          "LV","MT","MX","NL","NO","PL","PT","RO","RU","SA","SE","SI","SK","TR","US","ZA")


  if (verbose) message("Local data not found for MRIOT. Attempting to download from Eurostat...")

  # Ensure the destination directory exists
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  links =
    matrix(byrow = T,ncol = 2,
           data = c('2010', 'https://circabc.europa.eu/rest/download/fc80f855-d144-476e-b4bf-5cfba946819c',
                    '2011', 'https://circabc.europa.eu/rest/download/1bcb2624-04ed-43e1-8588-df6680ed352a',
                    '2012', 'https://circabc.europa.eu/rest/download/399671ad-cbb3-493e-ad5f-83e989f1eecc',
                    '2013', 'https://circabc.europa.eu/rest/download/a2b4746d-1d11-4a44-ab1c-50ac956f0849',
                    '2014', 'https://circabc.europa.eu/rest/download/beba57b2-2696-497a-b92f-2a5beca724c7',
                    '2015', 'https://circabc.europa.eu/rest/download/1a194b8c-6ea1-4bec-9c73-0cd599febcc3',
                    '2016', 'https://circabc.europa.eu/rest/download/2cdd74fc-0bce-4ae0-8bf2-34d34546d86d',
                    '2017', 'https://circabc.europa.eu/rest/download/4a11c796-4186-4cce-a02f-12d353fc5e59',
                    '2018', 'https://circabc.europa.eu/rest/download/7a57a374-2200-498c-bb5f-cee24202b0b8',
                    '2019', 'https://circabc.europa.eu/rest/download/c3467617-8a00-44a0-9b6b-ccad8a2ab58d',
                    '2020', 'https://circabc.europa.eu/rest/download/4df668e1-2a8a-4e84-ae57-00309d8bc760',
                    '2021', 'https://circabc.europa.eu/rest/download/6736dea8-da14-450f-b212-a791baf238c8',
                    '2022', 'https://circabc.europa.eu/rest/download/b20c339d-984f-413c-a499-54ff76beb90c'#,
                    #'2023', 'https://circabc.europa.eu/rest/download/21557f49-1e94-431c-8523-d972fec020b8')
           ))

  result = c()

  for(year in links[,1])
  {
    curl_file = curl_fetch_memory(links[links[,1] == as.character(year),2])

    header_file = rawToChar(curl_file$headers) %>%
      strsplit(.,'\\"') %>%
      unlist() %>%
      subset(.,grepl('ind-by-ind',.))

    if(verbose) message("Requested year = ",year,". Fetched file = ",header_file)

    if(verbose) message("Aggregate FIGARO ed25 to match AEA ed24 format...")

    dt = fread(rawToChar(curl_file$content))
    setnames(dt, 1, "resource_id")

    # Convert all columns except resource_id to numeric
    dt[, (2:ncol(dt)) := lapply(.SD, as.numeric), .SDcols = 2:ncol(dt)]

    # Melt data to long format
    dt_long = melt(dt, id.vars = "resource_id", variable.name = "use_id", value.name = "value")

    # Split use_id into country and industry parts
    dt_long[, use_country := sub("_.*", "", use_id)]
    dt_long[, use_industry := sub("^[^_]+_", "", use_id)]

    # Split resource_id into country and industry parts
    dt_long[, resource_country := sub("_.*", "", resource_id)]
    dt_long[, resource_industry := sub("^[^_]+_", "", resource_id)]

    # Replace unknown use_country codes with "FIGW1"
    dt_long[!use_country %in% figaro_country_list, use_country := "FIGW1"]

    # Replace unknown resource_country codes (except 'W2') with "FIGW1"
    dt_long[!resource_country %in% figaro_country_list & resource_country != "W2", resource_country := "FIGW1"]

    # Classify use_industry and resource_industry by order (P3/P5 last for use; D1/D2/OP last for resource)
    dt_long[, use_order := fifelse(grepl("P3|P5", use_industry), 2L, 1L)]
    dt_long[, resource_order := fifelse(grepl("D1|D2|OP", resource_industry), 2L, 1L)]

    # Aggregate values by country, industry, and order groups
    agg = dt_long[, .(value = sum(value)),
                  by = .(use_country, use_order, use_industry,
                         resource_country, resource_order, resource_industry)]

    # Rebuild full use_id and resource_id strings
    agg[, use_id := paste(use_country, use_industry, sep = "_")]
    agg[, resource_id := paste(resource_country, resource_industry, sep = "_")]

    # Prepare factor levels for rows
    resource_id_order = agg[order(resource_order, resource_country, resource_industry), unique(resource_id)]
    agg[, resource_id := factor(resource_id, levels = resource_id_order)]

    # Prepare factor levels for columns
    use_id_order = agg[order(use_order, use_country, use_industry), unique(use_id)]
    agg[, use_id := factor(use_id, levels = use_id_order)]

    # Cast back to wide format with resource_id as rows and use_id as columns
    result = rbind(result,
                   dcast(agg, resource_id ~ use_id, value.var = "value", fill = 0) %>% mutate(time_period = year))

    if(verbose) message("FIGARO formatting done for year ",year)
  }

  write_parquet(result, file.path(folder,"values_agg_25.parquet"))

  if(verbose) message("Online FIGARO version stored in ",file.path(folder,"values_agg_25.parquet"))

}

download_emissions_data = function(folder,verbose) {

  figaro_industry_list = c("A01","A02","A03","B","C10T12","C13T15","C16","C17","C18","C19","C20","C21","C22",
                           "C23","C24","C25","C26","C27","C28","C29","C30","C31_32","C33","D35","E36","E37T39",
                           "F","G45","G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60","J61","J62_63",
                           "K64","K65","K66","L","M69_70","M71","M72","M73","M74_75","N77","N78","N79","N80T82",
                           "O84","P85","Q86","Q87_88","R90T92","R93","S94","S95","S96","T","U")

  figaro_country_list = c("AR","AT","AU","BE","BG","BR","CA","CH","CN","CY","CZ","DE","DK","EE","ES",
                          "FI","FIGW1","FR","GB","GR","HR","HU","ID","IE","IN","IT","JP","KR","LT","LU",
                          "LV","MT","MX","NL","NO","PL","PT","RO","RU","SA","SE","SI","SK","TR","US","ZA")


  ghg_eurostat = get_eurostat("env_ac_ghgfp",
                              filters = list(na_item = "TOTAL", c_dest = "WORLD"),
                              cache   = FALSE
  ) %>%
    mutate(
      nace_r2 = case_when(
        nace_r2 == "D"     ~ "D35",
        nace_r2 == "O"     ~ "O84",
        nace_r2 == "P"     ~ "P85",
        grepl("-",nace_r2) ~ paste0(substr(nace_r2,1,3),"T",substr(nace_r2,6,7)),
        grepl("_",nace_r2) ~ paste0(substr(nace_r2,1,4),substr(nace_r2,6,7)),
        TRUE ~ nace_r2),
      c_orig  = case_when(
        c_orig  == "EL"       ~ "GR",   # Greece old code
        c_orig  == "UK"       ~ "GB",   # United Kingdom ISO‑2
        c_orig  == "WRL_REST" ~ "FIGW1",# Rest‑of‑World aggregate
        TRUE                  ~ c_orig
      )
    ) %>%
    filter(nace_r2 %in% c(figaro_industry_list,'HH') & c_orig %in% figaro_country_list) %>%
    group_by(time_period = substr(time,1,4),
             ref_area = c_orig,
             industry = nace_r2,
             counterpart_area = c_dest) %>%
    summarise(obs_value = sum(values),
              sto = 'ALL') %>%
    arrange(time_period,ref_area,industry) %>%
    ungroup()


  write_parquet(ghg_eurostat, file.path(folder,"ghgFootprint_25ed.parquet"))

  if(verbose) message("Formatted emissions data stored in ",file.path(folder,"ghgFootprint_25ed.parquet"))


}

# Main Data Formatting Functions ---

format_emissions <- function(folder, exdir, update = FALSE, verbose = TRUE, edition) {

  output_file <- file.path(exdir, paste0("co2e_emission_agg_", edition, ".parquet"))

  #if emissions data already formatted and cached

  if (!update && file.exists(output_file)) {
    if (verbose) message("Returning cached emissions data from: ", output_file)
    return(read_parquet(output_file))
  }

  # --- Download if local data is missing ---
  file_pattern_check <- paste0("ghgFootprint_", edition, "ed")
  if (length(list.files(folder, pattern = file_pattern_check)) == 0 || update) {

    unlink(list.files(folder, pattern = file_pattern_check,full.names = T))

    download_emissions_data(folder = folder,verbose = verbose)

    df = read_parquet(file.path(folder,"ghgFootprint_25ed.parquet"))

  }else{

  if (verbose) message("Processing emissions files...")

  filenames <- list.files(folder, pattern = file_pattern_check, full.names = TRUE)

  pb <- progress::progress_bar$new(
    format = "  Reading emission files [:bar] :percent", total = length(filenames), width = 60)

  if(tools::file_ext(filenames) == 'csv') df <- purrr::map_dfr(filenames, ~{pb$tick(); fread(.x)})

  if(tools::file_ext(filenames) == 'parquet') df <- read_parquet(filenames)

  }

  if (verbose) message("Imputing values for the year 2011...")

  df_2010 <- df %>% filter(time_period == 2010) %>% select(industry, ref_area, counterpart_area, sto, value_2010 = obs_value)
  df_2012 <- df %>% filter(time_period == 2012) %>% select(industry, ref_area, counterpart_area, sto, value_2012 = obs_value)

  df <- df %>%
    left_join(df_2010, by = c("industry", "ref_area", "counterpart_area", "sto")) %>%
    left_join(df_2012, by = c("industry", "ref_area", "counterpart_area", "sto")) %>%
    mutate(obs_value = if_else(time_period == 2011, 0.5 * value_2010 + 0.5 * value_2012, obs_value)) %>%
    select(-value_2010, -value_2012)

  if (verbose) message("Writing aggregated emissions data to cache: ", output_file)

  write_parquet(df, file.path(folder,"co2e_emission_agg_25.parquet"))

  if(verbose) message("Formatted emissions data stored in ",file.path(folder,"co2e_emission_agg_25.parquet"))

  return(df)

}


read_csv_with_time <- function(file_path, pb = NULL) {
  if (!is.null(pb)) pb$tick()

  # fread can read .gz files directly, no need to decompress manually
  df <- data.table::fread(file_path) %>%
    dplyr::mutate(time_period = stringr::str_extract(basename(file_path), "\\d{4}"))

  return(df)
}


format_iot <- function(folder, exdir, update = FALSE, verbose = TRUE, edition) {

  output_file = file.path(exdir, paste0("values_agg_", edition, ".parquet"))

  #If the file is already computed and cached

  if (!update && file.exists(output_file)) {
    if (verbose) message("Returning cached IOT data from: ", output_file)
    return(read_parquet(output_file))
  }

  #Else, fetch the online version

  output_file = file.path(exdir, "values_agg_25.parquet")

  if (!update && file.exists(file.path(exdir, "values_agg_25.parquet"))) {

    message("Local MRIOT edition 20",edition," not found. Returning cached 2025 edition")

    if (verbose) message("Returning cached IOT data from: ", output_file)
    return(read_parquet(output_file))
  }

  # --- Fallback Logic: Download if local data is missing ---
  file_pattern_check <- paste0("matrix_eu-ic-io_ind-by-ind_", edition, "ed")
  if (length(list.files(folder, pattern = file_pattern_check)) == 0) {

    download_figaro_data(folder = folder, verbose = verbose)

    df = list.files(folder, pattern = "values_agg_25", full.names = TRUE) %>%
      read_parquet()

    return(df)
  }

  if (verbose) message("Processing IOT files...")
  filenames <- list.files(folder, pattern = file_pattern_check, full.names = TRUE)

  pb <- progress::progress_bar$new(
    format = "  Reading IOT files [:bar] :percent", total = length(filenames), width = 60)

  df <- purrr::map_dfr(filenames, ~read_csv_with_time(.x, pb = pb))

  }

