x = c('tidyverse','data.table','arrow','xml2','rvest')
lapply(x, library,character.only = T)

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
                      version = "local")
{

  if(version == "online")
  {

    if(!update)
    {
      if(file.exists(paste0(exdir,"/online_figaro/values_agg.parquet")))
      {
        if(verbose) print('Returning cached data')
        return(read_parquet(paste0(exdir,"/online_figaro/values_agg.parquet")))
      }
      if(verbose) print('File does not exists yet, updating...')
    }

    links = read_html("https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/database") %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      subset(grepl("matrix_eu-ic",.) & grepl("io_ind-by-ind",.))

    years = strsplit(links,"ed_") %>%
      sapply(., "[[", 2) %>%
      substr(.,1,4)

    values_agg = c()

    for(i in years)
    {
      link = links %>% subset(.,grepl(i,.))

      mriot = fread(paste0("https://ec.europa.eu/",link)) %>%
        mutate(time_period = i)

      values_agg = rbind(
        values_agg,
        mriot
      )

      if(verbose) print(paste0("FIGARO MRIOT fetched and formatted for year ",i))

    }

    if(!dir.exists(paste0(exdir,"/online_figaro"))) dir.create(paste0(exdir,"/online_figaro"))

    write_parquet(values_agg, paste0(exdir,"/online_figaro/values_agg.parquet"))

    return(values_agg)

  }


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

# ed23 =
#   format_iot(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#             exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#             update = F) %>%
#   arrange(time_period,rowLabels)
#
# ed24 =
#   format_iot(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#              exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#              update = F,
#              version = "online") %>%
#   filter(time_period != 2022) %>%
#   arrange(time_period,rowLabels)
#
# comp = (ed24 %>% select(!c(time_period,rowLabels) ) -
#           ed23 %>% select(!c(time_period,rowLabels))) %>%
#   cbind(ed24 %>% select(c(time_period,rowLabels)))
