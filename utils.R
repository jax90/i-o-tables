currency_converter_oecd = function(value,iso3_origin,iso3_destination,year,col_value,col_year,new_col)
{

  oecd_exch = read.csv(paste0("https://sdmx.oecd.org/archive/rest/data/OECD,DF_DP_LIVE,/.EXCH...A?startPeriod=",year,
                              "&endPeriod=",year,"&dimensionAtObservation=AllDimensions&format=csvfile"))

  if(inherits(value,'data.frame'))
  {

    oecd_exch = read.csv(paste0("https://sdmx.oecd.org/archive/rest/data/OECD,DF_DP_LIVE,/.EXCH...A?dimensionAtObservation=AllDimensions&format=csvfile"))

    cn = colnames(value)

    value = value %>%
      mutate(exch_origin = oecd_exch$OBS_VALUE[match(paste0(iso3_origin,unlist(.[,col_year])),paste0(oecd_exch$LOCATION,oecd_exch$TIME_PERIOD))],
             exch_destination = oecd_exch$OBS_VALUE[match(paste0(iso3_destination,unlist(.[,col_year])),paste0(oecd_exch$LOCATION,oecd_exch$TIME_PERIOD))]) %>%
      mutate("{new_col}" := case_when(!is.na(exch_origin) & !is.na(exch_destination) ~ unlist(.[,col_value]) / exch_origin * exch_destination,
                                      T ~ NA)) %>%
      select(all_of(cn))

    return(value)
  }

  exch_origin = oecd_exch$OBS_VALUE[oecd_exch$LOCATION == iso3_origin]

  exch_destination = oecd_exch$OBS_VALUE[oecd_exch$LOCATION == iso3_destination]

  if(length(exch_origin) != 1 | length(exch_destination) != 1)
  {
    stop(paste0("OECD dataset for year ",year,", lengths = ",length(exch_origin), "(",iso3_origin,") & ",length(exch_origin),"(",iso3_destination,")"))
  }

  converted_value = value / exch_origin * exch_destination

  return(converted_value)

}

rsd_z_lenzen = function(ei)
{
  return(.393 * ei^(-.302))
}

rsd_e_lenzen = function(ei)
{
  return(.486 * ei^(-.261))
}

perturb_e_lenzen = function(ei,n=1)
{
  v = rnorm(n)
  return(10^(log10(ei)+v*log10(1+rsd_e_lenzen(ei))))
}

perturb_z_lenzen = function(zij,n=1)
{
  v = rnorm(n)
  return(10^(log10(zij)+v*log10(1+rsd_z_lenzen(zij))))
}

sort_id = function(df,col_1dim = 'id',col_2dim)
{

  cn = colnames(df)

  if(missing(col_2dim))
  {

    df =
      df %>%
      separate(!!col_1dim,c("country","industry"),remove = F,extra = 'merge') %>%
      arrange(country,industry) %>%
      select(all_of(cn))

    return(df)

  }

  df =
    df %>%
    separate(!!col_1dim,c("country1","industry1"),remove = F,extra = 'merge') %>%
    separate(!!col_2dim,c("country2","industry2"),remove = F,extra = 'merge') %>%
    arrange(country1,industry1,country2,industry2) %>%
    select(all_of(cn))

  return(df)

}

#Import all economic data from FIGARO and the corresponding emissions vector from Eurostat 'env_ac_ghgfp' database (Eurostat estimates)

fetch_format_data = function(year,folder = NULL,ghg = T)
{
  ####IMPORT FIGARO####

  #Detect corresponding tables

  if(all(is.null(folder))){

  links = read_html("https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/database") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    subset(grepl("matrix_eu-ic",.) & grepl(year,.))

  linkZ = links %>%
    subset(grepl("io_ind-by-ind",.))

  #Fetch raw data

  rawZ = fread(paste0("https://ec.europa.eu/",linkZ))

  rm(links,linkZ)

  }else{

    csv_file = data.frame(file_path = list.files(folder,full.names = T) %>% subset(grepl("matrix_eu-ic-io_ind-by-ind",.))) %>%
      mutate(local_year = substr(file_path,regexpr("ed_",file_path)+3,regexpr("ed_",file_path)+6)) %>%
      filter(local_year == year)

    rawZ = fread(csv_file$file_path)
  }
  #Format tables and vectors

  transaction_flows = rawZ %>%
    column_to_rownames("rowLabels") %>%
    {.[1:2944,1:2944]}

  distributed_demand =
    rawZ %>%
    as.data.frame() %>%
    {.[1:2944,-c(2:2945)]} %>%
    pivot_longer(-1,names_to = "finaldemand") %>%
    mutate(counterpartarea = sub("_.*","",finaldemand)) %>%
    group_by(id = rowLabels,counterpartarea) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = counterpartarea)


  aggregated_demand = rawZ %>%
    as.data.frame() %>%
    column_to_rownames("rowLabels") %>%
    {.[1:2944,-c(1:2944)]} %>%
    rowSums()

  intermediate_consumption = data.frame(value = colSums(transaction_flows))

  production =  data.frame(value = colSums(rawZ[,2:2945]))

  value_added = production - intermediate_consumption

  rm(rawZ)

  if(ghg)
  {
  ###Import GHG data

  ghg_eurostat =
    get_eurostat("env_ac_ghgfp",filters = list(na_item = "TOTAL",c_dest = 'WORLD',time = year)) %>%
    mutate(nace = bir:::eurostat_data_to_figaro(nace_r2),
           c_orig = case_when(c_orig == "EL" ~ "GR",
                              c_orig == "UK" ~ "GB",
                              c_orig == "WRL_REST" ~ "FIGW1",
                              T ~ c_orig),
           id = paste0(c_orig,"_",nace)) %>%
    mutate(value = values * 1000) %>%
    filter(id %in% rownames(transaction_flows)) %>%
    arrange(country = c_orig,industry = nace) %>%
    select(id,value) %>%
    column_to_rownames("id")

  list_data = list(
    transaction_flows = transaction_flows,
    distributed_demand = distributed_demand,
    aggregated_demand = aggregated_demand,
    intermediate_consumption = intermediate_consumption,
    production = production,
    value_added = value_added,
    direct_emissions = ghg_eurostat
  )


  }else{

  list_data = list(
    transaction_flows = transaction_flows,
    distributed_demand = distributed_demand,
    aggregated_demand = aggregated_demand,
    intermediate_consumption = intermediate_consumption,
    production = production,
    value_added = value_added
  )
  }


  return(list_data)
}
