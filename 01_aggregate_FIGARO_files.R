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
           data = c('2010', 'https://circabc.europa.eu/rest/download/810676bb-e3d8-4abe-a75e-c4fc5d059f1e',
                    '2011', 'https://circabc.europa.eu/rest/download/dcbc0884-f4e4-46af-bf3f-462937f59ee3',
                    '2012', 'https://circabc.europa.eu/rest/download/38e5a3a3-1ae1-408b-8901-153ef6652829',
                    '2013', 'https://circabc.europa.eu/rest/download/42d4f65d-3c5b-4eb3-8391-46314801b359',
                    '2014', 'https://circabc.europa.eu/rest/download/85d3f6de-3510-45ff-96c9-11da9f1e325a',
                    '2015', 'https://circabc.europa.eu/rest/download/e989cc76-52f5-4cd9-ba8c-5764fafde13a',
                    '2016', 'https://circabc.europa.eu/rest/download/2e7bc8e5-ee0c-4e57-a082-60a9fb59ee73',
                    '2017', 'https://circabc.europa.eu/rest/download/0bd4147b-d06b-4800-87e4-c479352e88ba',
                    '2018', 'https://circabc.europa.eu/rest/download/40c2349e-1ba7-48b5-b4a3-4f00b78f2283',
                    '2019', 'https://circabc.europa.eu/rest/download/ba9d7f3a-a381-49f2-998d-64fec81c3d35',
                    '2020', 'https://circabc.europa.eu/rest/download/28d77fbf-50c2-475c-8ba6-c97b02a69039',
                    '2021', 'https://circabc.europa.eu/rest/download/57fd3555-51db-4f03-b765-3cb7fcfe3007',
                    '2022', 'https://circabc.europa.eu/rest/download/f1330ee5-2841-4f3a-949c-20f36d068714',
                    '2023', 'https://circabc.europa.eu/rest/download/836a0346-bdb5-4d8a-bbea-b3bd583a53c9',
                    '2024', 'https://circabc.europa.eu/rest/download/8ac7da34-911e-4a91-ab1c-8aec1a574381'
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

    if(verbose) message("Aggregate FIGARO ed26 to match AEA ed24 format...")

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

    rm(curl_file, dt, dt_long, agg); gc() # free memory
  }

  write_parquet(result, file.path(folder,"values_agg_26.parquet"))

  if(verbose) message("Online FIGARO version stored in ",file.path(folder,"values_agg_26.parquet"))

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
        c_orig %in% c("AL","ME","MK","RS") ~ "FIGW1", # countries outside figaro_country_list merged into FIGW1
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


  write_parquet(ghg_eurostat, file.path(folder,"ghgFootprint_26ed.parquet"))

  if(verbose) message("Formatted emissions data stored in ",file.path(folder,"ghgFootprint_26ed.parquet"))


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

    df = read_parquet(file.path(folder,"ghgFootprint_26ed.parquet"))

  }else{

  if (verbose) message("Processing emissions files...")

  filenames <- list.files(folder, pattern = file_pattern_check, full.names = TRUE)

  pb <- progress::progress_bar$new(
    format = "  Reading emission files [:bar] :percent", total = length(filenames), width = 60)

  if(all(tools::file_ext(filenames) == 'csv')) df <- purrr::map_dfr(filenames, ~{pb$tick(); fread(.x)})

  if(all(tools::file_ext(filenames) == 'parquet')) df <- read_parquet(filenames)

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

  write_parquet(df, output_file)

  if(verbose) message("Formatted emissions data stored in ",output_file)

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

  output_file = file.path(exdir, "values_agg_26.parquet")

  if (!update && file.exists(file.path(exdir, "values_agg_26.parquet"))) {

    message("Local MRIOT edition 20",edition," not found. Returning cached 2026 edition")

    if (verbose) message("Returning cached IOT data from: ", output_file)
    return(read_parquet(output_file))
  }

  # --- Fallback Logic: Download if local data is missing ---
  file_pattern_check <- paste0("matrix_eu-ic-io_ind-by-ind_", edition, "ed")
  if (length(list.files(folder, pattern = file_pattern_check)) == 0) {

    download_figaro_data(folder = folder, verbose = verbose)

    df = list.files(folder, pattern = "values_agg_26", full.names = TRUE) %>%
      read_parquet()

    return(df)
  }

  if (verbose) message("Processing IOT files...")
  filenames <- list.files(folder, pattern = file_pattern_check, full.names = TRUE)

  pb <- progress::progress_bar$new(
    format = "  Reading IOT files [:bar] :percent", total = length(filenames), width = 60)

  df <- purrr::map_dfr(filenames, ~read_csv_with_time(.x, pb = pb))

  }

