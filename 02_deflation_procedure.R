x = c('dplyr','tidyr','tibble','curl','stringr','ggplot2','eurostat','xml2','rvest','data.table','arrow','countrycode','here','readxl')

lapply(x,library,character.only = T)

source(here("utils.R"))

get_value_added_price_index = function(base,time_serie = 2010:2022,verbose = T,update = F)
{

  if(!update)
  {
    file = list.files(tempdir(),recursive = T,full.names = T) %>% subset(grepl(paste0("pivoted_indexes",base,paste0(time_serie,collapse = '')),.))

    if(length(file) == 1)
    {
      if(verbose) print('Cached data used')
      return(readRDS(file))
    }
  }

  #IMPORT UN PRICE DATA

  if(verbose) print('Downloading UN current and constant VA data...')

  # link_un_price_data_constant = "http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=group_code:204;fiscal_year:2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023&DataMartId=SNA&Format=csv&c=2,3,4,6,7,8,9,10,11,12,13,14&s=_cr_engNameOrderBy:asc,fiscal_year:desc,_grIt_code:asc"
  #
  # zip_file = curl_download(link_un_price_data_constant,tempfile(fileext = '.zip'))
  #
  # unzip_file = unzip(zip_file,exdir = tempdir())

  unzip_file = here("price_data/UNdata_Export_Constant.csv")

  un_price_data_constant = read.csv(unzip_file,check.names = F)

  # link_un_price_data_current = "http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=group_code:205;fiscal_year:2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023&DataMartId=SNA&Format=csv&c=2,3,4,6,7,8,9,10,11,12,13,14&s=_cr_engNameOrderBy:asc,fiscal_year:desc,_grIt_code:asc"
  #
  # zip_file = curl_download(link_un_price_data_current,tempfile())
  #
  # unzip_file = unzip(zip_file,exdir = tempdir())

  unzip_file =  here("price_data/UNdata_Export_Current.csv")

  un_price_data_current = read.csv(unzip_file,check.names = F)

  #GATHER DATA

  if(verbose) print('Cleaning, filtering and formatting price data...')

  pooled_price_data =
    un_price_data_constant %>%
    filter(`Base Year` >=  min(time_serie)) %>%
    select(country = `Country or Area`,
           item = Item,
           code = `SNA93 Item Code`,
           base_year = `Base Year`,
           year = `Fiscal Year`,
           constant_value = Value,
           currency = Currency,
           sna_system = `SNA System`,
           series = Series) %>%
    mutate(current_value = un_price_data_current$Value[match(paste0(year,item,country,currency,sna_system,series),
                                                             paste0(un_price_data_current$Year,
                                                                    un_price_data_current$Item,
                                                                    un_price_data_current$`Country or Area`,
                                                                    un_price_data_current$Currency,
                                                                    un_price_data_current$`SNA System`,
                                                                    un_price_data_current$Series))])

  #FILTERING
  price_data = pooled_price_data %>%
    filter(substr(code,1,1) %in% LETTERS) %>%
    group_by(country,code,base_year,currency) %>%
    mutate(last_sna = max(sna_system)) %>%
    filter(last_sna == sna_system) %>%
    mutate(last_serie = max(series)) %>%
    filter(last_serie == series) %>%
    mutate(nearest_reference = year[which.min(abs(base - year))]) %>%
    filter(nearest_reference == base) %>%
    group_by(country,base_year,currency) %>%
    mutate(num = n()) %>%
    group_by(country,currency) %>%
    filter(num == max(num)) %>%
    mutate(max_base = max(base_year)) %>%
    filter(base_year == max_base) %>%
    ungroup() %>%
    select(country,code,base_year,year,currency,constant_value,current_value)


  #DERIVE VA DEFLATORS

  value_added_indexes =  price_data %>%
    group_by(country,code,base_year,currency,year) %>%
    mutate(index = constant_value / current_value) %>%
    group_by(country,code,currency,base_year) %>%
    mutate(recalibrated_index = index / index[year == base]) %>%
    ungroup()

  #FORMAT BY 64 BY 46 PRICE TABLE, IMPUTING WHOLE ECONOMY MEAN AS DEFAULT VALUE FOR MISSINGS

  figaro_country_list = c("AR","AT","AU","BE","BG","BR","CA","CH","CN","CY","CZ","DE","DK","EE","ES",
                          "FI","FIGW1","FR","GB","GR","HR","HU","ID","IE","IN","IT","JP","KR","LT","LU",
                          "LV","MT","MX","NL","NO","PL","PT","RO","RU","SA","SE","SI","SK","TR","US","ZA")

  figaro_industry_list = c("A01","A02","A03","B","C10T12","C13T15","C16","C17","C18","C19","C20","C21","C22",
                           "C23","C24","C25","C26","C27","C28","C29","C30","C31_32","C33","D35","E36","E37T39",
                           "F","G45","G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60","J61","J62_63",
                           "K64","K65","K66","L","M69_70","M71","M72","M73","M74_75","N77","N78","N79","N80T82",
                           "O84","P85","Q86","Q87_88","R90T92","R93","S94","S95","S96","T","U")

  empty_figaro_df = data.frame(country = rep(figaro_country_list,64)) %>%
    group_by(country) %>%
    mutate(industry = figaro_industry_list) %>%
    ungroup() %>%
    arrange(country,industry)

  # oecd_exch = read.csv(paste0("https://sdmx.oecd.org/archive/rest/data/OECD,DF_DP_LIVE,/.EXCH...A?&dimensionAtObservation=AllDimensions&format=csvfile"))
  #
  # figw1_weights = price_data %>%
  #   mutate(iso2 = countrycode(country,'country.name','iso2c',warn = F),
  #          iso3 = countrycode(country,'country.name','iso3c',warn = F)) %>%
  #   filter(!iso2 %in% figaro_country_list) %>%
  #   mutate(to_convert = ifelse(currency == "US dollar",0,1)) %>%
  #   mutate(currency_to_usd = oecd_exch$OBS_VALUE[match(paste0(iso3,year),paste0(oecd_exch$LOCATION,oecd_exch$TIME_PERIOD))])
  #Maybe use https://betadata.imf.org/en/Data-Explorer?datasetUrn=IMF.RES:WEO(4.0.0). OECD database contains to much missing exchange rates

  formatted_indexes = value_added_indexes %>%
    mutate(iso2 = countrycode(country,'country.name','iso2c',warn = F),
           figaro_country = case_when(iso2 %in% figaro_country_list ~ iso2,
                                      T ~ 'FIGW1')) %>%
    group_by(figaro_country,year,code) %>%
    summarise(index = mean(recalibrated_index,na.rm=T)) %>% #Imply that price index for FIGW1 is retrieved by a simple arithmetic mean. To review
    group_by(figaro_country,code) %>%
    mutate(index = index / index[year == base]) %>%
    ungroup() %>%
    mutate(id = paste0(figaro_country,code,year))

  formatted_rates = formatted_indexes %>%
    arrange(figaro_country,year,code) %>%
    group_by(figaro_country,code) %>%
    mutate(ascending_rate = index / lag(index),
           descending_rate = index / lead(index))


  figaro_indexes = empty_figaro_df %>%
    as.data.frame() %>%
    mutate(code1 = substr(industry,1,1),
           code2 = case_when(substr(industry,1,1) %in% c('B','C','D','E') ~ 'B+C+D+E',
                             substr(industry,1,1) %in% c('G','H','I') ~ 'G+H+I',
                             substr(industry,1,1) %in% c('M','N') ~ 'M+N',
                             substr(industry,1,1) %in% c('O','P','Q') ~ 'O+P+Q',
                             substr(industry,1,1) %in% c('R','S','T') ~ 'R+S+T',
                             T ~ 'B.1g'), #B1G = the national total = default value
           code3 = 'B.1g') %>%
    ungroup() %>%
    mutate(id1 = paste0(country,code1),
           id2 = paste0(country,code2),
           id3 = paste0(country,code3))


  if(verbose) print('Missing data step 1 : assign the price index of a broader sector or at the national level ...')


  for(i in as.character(time_serie))
  {

    #FIND THE CLOSEST PRICE INDEX
    figaro_indexes[,i] = formatted_indexes$index[match(paste0(figaro_indexes$id1,i),
                                                       formatted_indexes$id)]
  }

  ####FETCH AND FORMAT EUROSTAT PPI data

  if(verbose) print("Fetching and formatting additional PPI data into the price table : Eurostat, Chinese NBS and US BLS")

  eu_ppi = read.csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/sts_inpp_a/A.PRC_PRR.B+C10-C12+C13-C15+C16+C17+C18+C19+C20+C21+C22+C23+C24+C25+C26+C27+C28+C29+C30+C31_C32+C33+D+E36.NSA.I21.?format=SDMX-CSV&startPeriod=2010") %>%
    mutate(FIGARO = case_when(nace_r2 == "C10-C12" ~ "C10T12",
                              nace_r2 == "C13-C15" ~ "C13T15",
                              nace_r2 == "D" ~ "D35",
                              T ~ nace_r2)) %>%
    group_by(country = geo, industry = FIGARO) %>%
    filter(base %in% TIME_PERIOD) %>%
    reframe(value = OBS_VALUE / OBS_VALUE[TIME_PERIOD == base],
            year = TIME_PERIOD)

  eu_sppi = read.csv("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/sts_sepp_a/A.PRC_PRR+PRC_PRR_B2B.H-N_STS+H49+H50+H51+H52+H53+I+J+J58+J59+J60+J61+J62_J63+L+M69_M702+M71+M73+M74+N77+N78+N79+N80+N81+N82.NSA.I21+I15+I10+PCH_SM.?format=SDMX-CSV&startPeriod=2010") %>%
    mutate(FIGARO = case_when(nace_r2 %in% c("J59","J60") ~ "J59_60",
                              nace_r2 == "J62_J63" ~ "J62_63",
                              nace_r2 == "M69_M702" ~ "M69_70",
                              nace_r2 == "M74" ~ "M74_75",
                              nace_r2 %in% c("N80","N81","N82") ~ "N80T82",
                              T ~ nace_r2)) %>%
    group_by(country = geo, industry = FIGARO) %>%
    filter(base %in% TIME_PERIOD) %>%
    group_by(country,industry,year = TIME_PERIOD) %>%
    summarise(value = mean(OBS_VALUE,na.rm=T)) %>%
    group_by(country, industry) %>%
    reframe(value = value / value[year == base],
            year) %>%
    filter(!is.na(value))

  ####FETCH AND FORMAT CHINESE PPI data

  cn_ppi = read.csv(here("price_data/CN_PPIs.csv"),
                    skip = 2,
                    header = T,
                    check.names = F) %>%
    filter(!is.na(.[,as.character(base)])) %>%
    mutate(Indicators = case_when(1:n() == 1 ~ "Total",
                                  T ~ substring(Indicators,68))) %>%
    pivot_longer(-1,names_to = "year") %>%
    mutate(min_year = min(year[!is.na(value)]),
           value = case_when(year == min_year ~ 100,
                             T ~ value)) %>%
    filter(!is.na(value)) %>%
    group_by(Indicators) %>%
    mutate(FIGARO = case_when(grepl('Mining|Extraction',Indicators) ~ "B",
                              Indicators == "Manufacture of Foods" ~ "C10T12",
                              Indicators == "Manufacture of Textile Wearing Apparel and Accessories" ~ "C13T15",
                              Indicators == "Manufacture of Paper and Paper Products" ~ "C17",
                              Indicators == "Printing and Reproduction of Recording Media" ~ "C18",
                              Indicators == "Processing of Petroleum Coking and Processing of Nuclear Fuel" ~ "C19",
                              Indicators == "Manufacture of Raw Chemical Materials and Chemical Products" ~ "C20",
                              Indicators == "Manufacture of Medicines" ~ "C21",
                              Indicators == "Manufacture of Rubber and Plastics Products" ~ "C22",
                              Indicators == "Manufacture of Non-metallic Mineral Products" ~ "C23",
                              grepl('Smelting',Indicators) ~ "C24",
                              Indicators == "Manufacture of Metal Products" ~ "C25",
                              Indicators == "Manufacture of Computers Communication and Other Electronic Equipment" ~ "C26",
                              Indicators == "Manufacture of Electrical Machinery and Apparatus" ~ "C27",
                              Indicators == "Other Manufacture" ~ "C28",
                              Indicators == "Manufacture of Automobiles" ~ "C29",
                              Indicators == "Manufacture of Railway Ship Aerospace and Other Transport Equipments" ~ "C30",
                              Indicators == "Repair Service of Metal Products Machinery and Equipment" ~ "C33",
                              Indicators %in% c("Production and Supply of Electric Power and Heat Power"," Production and Supply of Gas") ~ "D35",
                              Indicators == " Production and Supply of Water" ~ "E36",
                              Indicators == "Utilization of Waste Resources" ~ "E37T39",
                              Indicators == "Total" ~ "TOTAL")) %>%
    filter(!is.na(FIGARO)) %>%
    group_by(FIGARO,year) %>%
    summarise(value = mean(value),
              basis = NA) %>%
    ungroup()

  for(i in unique(cn_ppi$FIGARO))
  {
    range = which(cn_ppi$FIGARO ==i)

    for(j in range)
    {
      cn_ppi$basis[j] = 100 * prod(cn_ppi$value[range[1]:j]/100)
    }

    cn_ppi$basis[range] = cn_ppi$basis[range] / cn_ppi$basis[cn_ppi$FIGARO == i & cn_ppi$year == base]

  }

  ####FETCH AND FORMAT US C26 PPI data

  #'PCU334---334---',#Computer & electronic product mfg C26
  #'PCU5182105182104',#Data processing, hosting and related services-Data management, information transformation, and related services J62_63
  #'PCU517---517---'))#,#Telecommunications J61
  #
  # Access https://data.bls.gov/series-report, type :
  # PCU334---334---
  # PCU5182105182104
  # PCU517---517---
  # Click 'Next'
  # Choose 'Multi-series table', Specify the 2010- range, Select 'Annual Data'
  # Click 'Retrieve data'
  # Download and rename the Excel file

  us_ppi = read_xlsx(here("price_data/US_PPIs.xlsx"),skip = 2,.name_repair = 'minimal') %>%
    pivot_longer(-1,names_to = 'year') %>%
    mutate(year = gsub("Annual\n","",year),
           industry = case_when(`Series ID` == "PCU334---334---" ~ "C26",
                                `Series ID` == "PCU5182105182104" ~ "J62_63",
                                `Series ID` == "PCU517---517---" ~ "J61")) %>%
    group_by(industry) %>%
    mutate(value = value / value[year == base])


  if(verbose) print("Inserting additional PPI data into the price table...")


  for(i in 1:nrow(figaro_indexes))
  {

    if(figaro_indexes$country[i] %in% codelist$iso2c[codelist$eu28 == "EU"])
    {


      if(figaro_indexes$industry[i] %in% eu_ppi$industry)
      {
        fig_country = ifelse(figaro_indexes$country[i] %in% eu_ppi$country,figaro_indexes$country[i],"EU27_2020")

        figaro_indexes[i,as.character(time_serie)] = eu_ppi$value[match(paste0(fig_country,figaro_indexes$industry[i],time_serie),
                                                                        paste0(eu_ppi$country,eu_ppi$industry,eu_ppi$year))]
      }


      if(figaro_indexes$industry[i] %in% eu_sppi$industry)
      {

        fig_country = ifelse(figaro_indexes$country[i] %in% eu_sppi$country,figaro_indexes$country[i],"EU27_2020")

        figaro_indexes[i,as.character(time_serie)] = eu_sppi$value[match(paste0(fig_country,figaro_indexes$industry[i],time_serie),
                                                                         paste0(eu_sppi$country,eu_sppi$industry,eu_sppi$year))]

      }

    }

    if(figaro_indexes$country[i] == "CN" & figaro_indexes$industry[i] %in% cn_ppi$FIGARO) figaro_indexes[i,as.character(time_serie)] = cn_ppi$basis[match(paste0(figaro_indexes$industry[i],time_serie),
                                                                                                                                                          paste0(cn_ppi$FIGARO,cn_ppi$year))]

    if(figaro_indexes$country[i] == "US" & figaro_indexes$industry[i] %in% c("C26","J61","J62_63"))
    {
      figaro_indexes[i,as.character(time_serie)] = us_ppi$value[match(paste0(figaro_indexes$industry[i],time_serie),
                                                                      paste0(us_ppi$industry,us_ppi$year))]
    }


  }


  figaro_indexes$missing_values = rowSums(is.na(figaro_indexes))

  missing_figaro_indexes = figaro_indexes[figaro_indexes$missing_values == length(time_serie),]

  for(i in as.character(time_serie))
  {

    missing_figaro_indexes[,i] = formatted_indexes$index[match(paste0(missing_figaro_indexes$id2,i),
                                                               formatted_indexes$id)]
  }

  missing_figaro_indexes$missing_values = rowSums(is.na(missing_figaro_indexes))

  #INSERT NEW VALUE
  figaro_indexes[figaro_indexes$missing_values == length(time_serie) &
                   figaro_indexes$id2 %in% missing_figaro_indexes$id2[missing_figaro_indexes$missing_values != length(time_serie)],] =
    missing_figaro_indexes[missing_figaro_indexes$missing_values != length(time_serie),]

  missing_figaro_indexes = missing_figaro_indexes[missing_figaro_indexes$missing_values == length(time_serie),]

  for(i in as.character(time_serie))
  {

    missing_figaro_indexes[,i] = formatted_indexes$index[match(paste0(missing_figaro_indexes$id3,i),
                                                               formatted_indexes$id)]
  }

  missing_figaro_indexes$missing_values = rowSums(is.na(missing_figaro_indexes))

  #INSERT NEW VALUE
  figaro_indexes[figaro_indexes$missing_values == length(time_serie) &
                   figaro_indexes$id3 %in% missing_figaro_indexes$id3[missing_figaro_indexes$missing_values != length(time_serie)],] =
    missing_figaro_indexes[missing_figaro_indexes$missing_values != length(time_serie),]

  #When there is no data at all, take FIGW1

  figaro_indexes$missing_values = rowSums(is.na(figaro_indexes))

  empty_rows = which(figaro_indexes$missing_values == length(time_serie))

  for(i in as.character(time_serie))
  {

    figaro_indexes[empty_rows,i] = figaro_indexes[match(paste0('FIGW1',figaro_indexes$industry[empty_rows]),
                                                        paste0(figaro_indexes$country,figaro_indexes$industry)),i]

  }


  #For US in 2022, Russia starting in 2014, Argentina and Saudia Arabia (no data), fill with FIGW1 sectoral data

  #Fill gaps with ascending and descending rate loop

  if(verbose) print('Missing data step 2 : chained price index imputation procedure for residual missing years...')

  figaro_indexes$missing_values = rowSums(is.na(figaro_indexes))

  for(i in which(figaro_indexes$missing_values > 0))
  {
    for(j in which(is.na(figaro_indexes[i,])))
    {

      sector_country_specific_rates =
        formatted_rates %>%
        filter(!is.na(index)) %>%
        filter(figaro_country == figaro_indexes$country[i] & code %in% c(figaro_indexes$code1[i],
                                                                         figaro_indexes$code2[i],
                                                                         figaro_indexes$code3[i])) %>%
        group_by(code) %>%
        filter(any(year == names(figaro_indexes)[j])) %>%
        ungroup() %>%
        mutate(ind = case_when(any(code == figaro_indexes$code1[i]) ~ figaro_indexes$code1[i],
                               any(code == figaro_indexes$code2[i]) ~ figaro_indexes$code2[i],
                               any(code == figaro_indexes$code3[i]) ~ figaro_indexes$code3[i])) %>%
        filter(code == ind) %>%
        select(year,ascending_rate,descending_rate)

      if(nrow(sector_country_specific_rates) == 0)
      {
        sector_country_specific_rates =
          formatted_rates %>%
          filter(!is.na(index)) %>%
          filter(figaro_country == 'FIGW1' & code %in% c(figaro_indexes$code1[i],
                                                         figaro_indexes$code2[i],
                                                         figaro_indexes$code3[i])) %>%
          group_by(code) %>%
          filter(any(year == names(figaro_indexes)[j])) %>%
          ungroup() %>%
          mutate(ind = case_when(any(code == figaro_indexes$code1[i]) ~ figaro_indexes$code1[i],
                                 any(code == figaro_indexes$code2[i]) ~ figaro_indexes$code2[i],
                                 any(code == figaro_indexes$code3[i]) ~ figaro_indexes$code3[i])) %>%
          filter(code == ind) %>%
          select(year,ascending_rate,descending_rate)
      }

      closest_pts = sector_country_specific_rates %>%
        mutate(rate = case_when(names(figaro_indexes)[j] == 2010 ~ descending_rate,
                                T ~ ascending_rate)) %>%
        filter(!year %in% names(figaro_indexes)[which(is.na(figaro_indexes[i,]))]) %>%
        mutate(gap = year - as.numeric(names(figaro_indexes)[j])) %>%
        filter(gap == min(gap)) %>%
        select(year) %>%
        as.character()

      figaro_indexes[i,j] =
        figaro_indexes[i,closest_pts] *
        prod(sector_country_specific_rates[which(sector_country_specific_rates$year == closest_pts) : which(sector_country_specific_rates$year == names(figaro_indexes)[j]),
                                           case_when(names(figaro_indexes)[j] == 2010 ~ 'descending_rate',
                                                     T ~ 'ascending_rate')][-1,])
    }
    if(verbose) print(paste0(which(which(figaro_indexes$missing_values > 0) == i),"    /   ",length(which(figaro_indexes$missing_values > 0))))
  }


  pivoted_indexes = figaro_indexes %>%
    select(country,industry,as.character(time_serie)) %>%
    pivot_longer(-c(1:2),names_to = 'year') %>%
    mutate(base = base)


  if(verbose) print(paste0("Price table cached for basis ",base," and time serie : ",paste0(time_serie,collapse = ", ")))

  saveRDS(pivoted_indexes,tempfile(pattern = paste0("pivoted_indexes",base,paste0(time_serie,collapse = '')),fileext = '.rdata'))


  return(pivoted_indexes)
}


#Pre-load utils_sda.R, especially the fetch_format_data function

get_constant_figaro_tables = function(years = 2010:2021,base = 2021,verbose = T,folder = NULL)
{
  price_data = get_value_added_price_index(base) %>%
    mutate(id = paste0(country,"_",industry))

  list_constant = list()

  for(i in years)
  {
    if(verbose) print(paste0("FIGARO ",i," deflation procedure..."))

    formatted_figaro = fetch_format_data(i,folder = NULL)

    formatted_figaro$distributed_demand =
      formatted_figaro$distributed_demand %>%
      pivot_longer(-1,names_to = 'counterpart') %>%
      mutate(deflator = price_data$value[match(paste0(i,id),
                                               paste0(price_data$year,price_data$id))],
             value = deflator * value) %>%
      select(!deflator) %>%
      pivot_wider(names_from = "counterpart")

    formatted_figaro$transaction_flows =
      formatted_figaro$transaction_flows %>%
      rownames_to_column('resource_id') %>%
      pivot_longer(-1,names_to = 'use_id') %>%
      mutate(deflator = price_data$value[match(paste0(i,resource_id),
                                               paste0(price_data$year,price_data$id))],
             value = value * deflator) %>%
      select(!deflator) %>%
      pivot_wider(names_from = "use_id") %>%
      column_to_rownames("resource_id")

    formatted_figaro$intermediate_consumption =
      colSums(formatted_figaro$transaction_flows)

    formatted_figaro$production =
      rowSums(formatted_figaro$distributed_demand[,-1]) + rowSums(formatted_figaro$transaction_flows)

    formatted_figaro$value_added =
      formatted_figaro$production - formatted_figaro$intermediate_consumption

    formatted_figaro$aggregated_demand =
      formatted_figaro$distributed_demand %>% column_to_rownames('id') %>% rowSums()


    list_constant[[as.character(i)]] = formatted_figaro

    rm(formatted_figaro)

    if(verbose) print(paste0("FIGARO ",i," tables deflated"))

  }

  return(list_constant)
}

extract_constant_table = function(constant_tables,
                                  item,#"transaction_flows","distributed_demand","aggregated_demand","intermediate_consumption","production","value_added"
                                  year)
{

  if(!year %in% names(constant_tables)) stop(paste0("selected year are not stored in 'constant_tables', it should be either : ",paste0(names(constant_tables),collapse = ", ")))

  if(!item %in% names(constant_tables[[as.character(year)]])) stop(paste0("'item' should be one of these strings : ",paste0(names(constant_tables[[as.character(year)]]),collapse = ', ')))

  output = constant_tables[[as.character(year)]][[item]]

  return(output)
}

# test1 = get_value_added_price_index(2010) #WORKS
# test2 = get_value_added_price_index(2015) #WORKS
#
# test3 = get_constant_figaro_tables(years = 2010:2021,
#                                    folder = "./data/values/values_agg_23.rds") #WORKS
# test4 = fetch_format_data(year = 2021,
#                           folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Travaux appliqués/F Charpentier - ICT4S/Embodied footprint",
#                           ghg = F) #WORKS
#
# test5 = fetch_format_data(year = 2021,
#                           folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Travaux appliqués/F Charpentier - ICT4S/Embodied footprint",
#                           ghg = T) #WORKS
# test6 = extract_constant_table(test3,
#                                'transaction_flows',
#                                2010) #WORKS
# test7 = extract_constant_table(test3,
#                                'production',
#                                2010) #WORKS


