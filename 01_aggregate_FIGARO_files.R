
format_emissions = function(folder,
                            exdir,
                            update = F,
                            verbose = T,
                            edition = edition)
{
  if(!update)
  {
    if(file.exists(paste0(exdir,"/co2e_emission_agg_", edition,".parquet")))
    {
      if(verbose) print('Returning cached data')
      return(read_parquet(paste0(exdir,"/co2e_emission_agg_", edition,".parquet")))
    }
    if(verbose) print('File does not exists yet, updating...')
  }

  filenames <- list.files(folder, pattern = paste0("ghgFootprint_",edition,"ed"), full.names = TRUE)

  df <- filenames |>
    map_dfr(fread, sep = ",")


  df_2010 <- df |>
    select(time_period, industry, ref_area, counterpart_area, sto, obs_value) |>
    filter(time_period == 2010) |>
    mutate(value_2010 = obs_value) |>
    select(-time_period, -obs_value)

  df_2012 <- df |>
    select(time_period, industry, ref_area, counterpart_area, sto, obs_value) |>
    filter(time_period == 2012) |>
    mutate(value_2012 = obs_value) |>
    select(-time_period, -obs_value)


  df <- df |>
      left_join(df_2010, by = c("industry", "ref_area", "counterpart_area", "sto")) |>
      left_join(df_2012, by = c("industry", "ref_area", "counterpart_area", "sto")) |>
      mutate(obs_value = if_else(time_period == 2011, 0.5* value_2010 + 0.5* value_2012, obs_value)) |>
    select(-value_2010, -value_2012)
                #   group_by(industry, ref_area, counterpart_area, counterpart_industry) |>

  # df <- df |>
  #   group_by(industry, ref_area, counterpart_area, sto) |>
  #   mutate(
  #     value_2011 = obs_value[time_period == 2011],
  #     value_2012 = obs_value[time_period == 2012],
  #     obs_value = if_else(
  #       ref_area == "CN" & time_period == 2011,
  #       0.5 * value_2011 + 0.5 * value_2012,
  #       obs_value
  #     )
  #   ) |>
  #   ungroup() |>
  #   select(-value_2011, -value_2012)


  write_parquet(df, paste0(exdir,"/co2e_emission_agg_", edition,".parquet"))

  return(df)

}
#
# df |>
#   filter(ref_area == "CN") |>
#   group_by(time_period) |>
#   summarise(obs_value  = sum(obs_value), .groups = "drop" )
#


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
                      verbose = T,
                      edition = edition)
{

  if(!update)
  {
    if(file.exists(paste0(exdir,"/values_agg_",edition,".parquet")))
    {
      if(verbose) print('Returning cached data')
      return(read_parquet(paste0(exdir,"/values_agg_",edition,".parquet")))
    }
    if(verbose) print('File does not exists yet, updating...')
  }

  filenames <- list.files(folder, pattern = paste0("matrix_eu-ic-io_ind-by-ind_",edition,"ed"), full.names = TRUE)

  df <- filenames |>
    map_dfr(read_csv_with_time)

  if(edition == "24")
  {
    if(verbose) print('Impute 2011 for China...')
  if(verbose) print('Prepare frame...')

    df <- df |>
      separate(rowLabels, into = c("ref_area", "industry"), sep = "_",  extra = "merge") |>
      pivot_longer(
        cols = -c(ref_area, industry, time_period),
        names_to = c("counterpart_area", ".value"),
        names_pattern = "^(\\D+1?)_(\\w*_?)"  ) |>
      pivot_longer(
        cols = -c(ref_area, industry, time_period, counterpart_area),
        names_to = "counterpart_industry",
        values_to = "value")


    if(verbose) print('Impute...')



    df_2010 <- df |>
      select(time_period, industry, ref_area, counterpart_area, counterpart_industry, value) |>
      filter(time_period == 2010) |>
      mutate(value_2010 = value) |>
      select(-time_period, -value)

    df_2012 <- df |>
      select(time_period, industry, ref_area, counterpart_area, counterpart_industry, value) |>
      filter(time_period == 2012) |>
      mutate(value_2012 = value) |>
      select(-time_period, -value)


    df <- df |>
      left_join(df_2010, by = c("industry", "ref_area", "counterpart_area", "counterpart_industry")) |>
      left_join(df_2012, by = c("industry", "ref_area", "counterpart_area", "counterpart_industry")) |>
      mutate(value = if_else(time_period == 2011, 0.5* value_2010 + 0.5* value_2012, value)) |>
      select(-value_2010, -value_2012)


    if(verbose) print('Imputed...')

    df <- df |>
      unite(rowLabels, ref_area,industry, sep = "_") |>
      unite(colLabels, counterpart_area,counterpart_industry, sep = "_") |>
      pivot_wider(names_from = colLabels,
                  values_from = value)

      # Remove temporary columns

    if(verbose) print('Done...')

  }



  if(verbose) print(paste0('Data cached in ',exdir,paste0("/values_agg_",edition,".parquet")))

  write_parquet(df, paste0(exdir,"/values_agg_",edition,".parquet"))

  return(df)

}

# format_iot(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#            exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#            update = T)
