x = c('tidyverse','data.table','arrow')
lapply(x, library,character.only = T)

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

  filenames <- list.files(folder, pattern = "ghgFootprint", full.names = TRUE)

  df <- filenames |>
    map_dfr(fread)

  write_parquet(df, paste0(exdir,"/co2e_emission_agg.parquet"))

  return(df)

}

# format_emissions(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#                  exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#                  update = T)
