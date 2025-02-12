x = c('tidyverse','data.table','arrow','eurostat')
lapply(x, library,character.only = T)

format_emissions = function(folder,
                            exdir,
                            update = F,
                            verbose = T,
                            version = "local")
{
  if(version == "online")
  {

    if(!update)
    {
      if(file.exists(paste0(exdir,"/online_ghg/co2e_emission_agg.parquet")))
      {
        if(verbose) print('Returning cached data')
        return(read_parquet(paste0(exdir,"/online_ghg/co2e_emission_agg.parquet")))
      }
      if(verbose) print('File does not exists yet, updating...')
    }


    ghg_eurostat =
      get_eurostat("env_ac_ghgfp",filters = list(na_item = "TOTAL",c_dest = 'WORLD'),update_cache = T) %>%
      mutate(nace = case_when(
        nace_r2 == "D" ~ "D35",
        nace_r2 == "O" ~ "O84",
        nace_r2 == "P" ~ "P85",
        grepl("-",nace_r2) ~ paste0(substr(nace_r2,1,3),"T",substr(nace_r2,6,7)),
        grepl("_",nace_r2) ~ paste0(substr(nace_r2,1,4),substr(nace_r2,6,7)),
        T ~ nace_r2
      ),
             c_orig = case_when(c_orig == "EL" ~ "GR",
                                c_orig == "UK" ~ "GB",
                                c_orig == "WRL_REST" ~ "FIGW1",
                                T ~ c_orig),
             id = paste0(c_orig,"_",nace)) %>%
      reframe(time_period = substr(time,1,4),
              ref_area = c_orig,
              counterpart_area = c_dest,
              obs_value = values,
              industry = nace) %>%
      filter(nchar(ref_area) == 2 | ref_area == 'FIGW1')

      if(!dir.exists(paste0(exdir,"/online_ghg"))) dir.create(paste0(exdir,"/online_ghg"))

      write_parquet(ghg_eurostat, paste0(exdir,"/online_ghg/co2e_emission_agg.parquet"))


      return(ghg_eurostat)

  }


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

# gzip = curl::curl_download("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/env_ac_ghgfp/?format=SDMX-CSV",tempfile())
#
# file = R.utils::gunzip(gzip, destname = tempfile())
#
# ed24 = fread(file,sep = ",",header = T) %>%
#   group_by(ref_area,industry)
# ed24 %>%
#   separate(c_orig,c("country","country_lib"),sep = ":",extra = 'merge') %>%
#   separate(nace_r2,c("industry","industry_lib"),sep = ":",extra = 'merge') %>%
#   group_by()
#
#
#
# ed23 = format_emissions(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#                                            exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#                                            update = F)# %>%
#   # mutate(value_ed24 = ed24$OBS_VALUE[match(paste0(.$time_period,.$ref_area,.$industry,.$counterpart_area,.$sto),
#   #                                          paste0(.$TIME_PERIOD,.$c_orig,.$nace_r2,.$c_dest,.$na_item))])
