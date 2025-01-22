
format_emissions = function(folder,
                            exdir,
                            update = F,
                            verbose = T)
{
  if(!update)
  {
    if(file.exists(paste0(exdir,"/co2e_emission_agg.parquet")))
    {
      if(verbose) print('Returning cached data')
      return(read_parquet(paste0(exdir,"/co2e_emission_agg.parquet")))
    }
    if(verbose) print('File does not exists yet, updating...')
  }

  filenames <- list.files(folder, pattern = "greenhouse-gas-footprints_2024-edition", full.names = TRUE)

  df <- filenames |>
    map_dfr(fread)

  write_parquet(df, paste0(exdir,"/co2e_emission_agg.parquet"))

  return(df)

}

# format_emissions(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#                  exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#                  update = T)

#ref_area refers to rows
#counterpart refers to columns

read_csv_with_time <- function(file_path) {
  # Read the CSV file
  df <- fread(file_path) %>%
    # Add 'time_period' column
    mutate(time_period = gsub(".*_(\\d{4})\\.csv", "\\1", file_path))

  return(df)
}


format_iot = function(folder, #matrix_eu-ic-io_ind-by-ind_23ed local path
                      exdir,
                      update = F,
                      verbose = T)
{

  if(!update)
  {
    if(file.exists(paste0(exdir,"/values_agg_23.parquet")))
    {
      if(verbose) print('Returning cached data')
      return(read_parquet(paste0(exdir,"/values_agg_23.parquet")))
    }
    if(verbose) print('File does not exists yet, updating...')
  }

  filenames <- list.files(folder, pattern = "matrix_eu-ic-io_ind-by-ind_23ed", full.names = TRUE)

  df <- filenames |>
    map_dfr(read_csv_with_time)
  # %>%
  #   separate(rowLabels, into = c("ref_area", "industry"), sep = "_",  extra = "merge") |>
  #   pivot_longer(
  #     cols = -c(ref_area, industry, time_period),
  #     names_to = c("counterpart_area", ".value"),
  #     names_pattern = "^(\\D+1?)_(\\w*_?)"  )

  if(verbose) print(paste0('Data cached in ',exdir,"/values_agg_23.parquet"))

  write_parquet(df, paste0(exdir,"/values_agg_23.parquet"))

  return(df)

}

# format_iot(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#            exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#            update = T)
