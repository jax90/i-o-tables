#To capture economic technology, one needs to derive technical coefficient 'a'
#We deduct 'a' being the sum of intermediate uses of products (row summation) and the vector of final demand

compute_a = function(z,d)
{
  a = sweep(
    as.matrix(z),
    2,
    rowSums(z) + d,
    `/`)

  a[is.nan(a) | is.infinite(a)] = 0

  return(a)
}

#Perform production footprint computations - main method of the paper.
#Using the second identified method, that is time-saving. Exactly equivalent to Janna's method

io_aggregation_methods = function(d, #Final demand vector
                                  c, #Impact per unit of output
                                  a, #Technical coefficient
                                  selected_industry) #An array that contains target sub sectors
{
  ####CHECK INPUT PARAMETERS####

  if(dim(a)[1] != dim(a)[2]) stop("param 'a' should be a squared matrix")
  if(!all(selected_industry %in% colnames(a))) stop("param 'selected industry' is not detected in 'a' dimension names")

  if(length(d) != dim(a)[1]) stop("param 'd' length is not corresponding to the intermediate flows matrix")

  if(nrow(c) != dim(a)[1]) stop("param 'c' length is not corresponding to the intermediate flows matrix")

  d = matrix(data = d, ncol = 1, dimnames = list(colnames(a),"demand"))

  ####DEFINE COMMON VARIABLES#####

  l = solve(diag(nrow = nrow(a)) - a)

  x = l %*% d

  #Consistency check

  #if(any(round(x) != round(rowSums(a%*%x) + d))) stop('System inconsistency')

  #c = matrix(data = c, ncol = 1, dimnames = list(rownames(c),"intensity"))

  selected_industry_num = which(colnames(a) %in% selected_industry)

  sub_x = x[selected_industry_num,1,drop = F]

  ###METHOD 2

  sub_invL = solve(l[selected_industry_num,selected_industry_num])

  prescaling_sub_prod = sub_invL %*% sub_x

  full_prod = l[,selected_industry_num] %*% prescaling_sub_prod

  ####COMPUTE THE CORRESPONDING FOOTPRINT####

  c[is.nan(unlist(c))] = 0

  fpt = t(c) %*% full_prod

  ####FORMAT AND RETURN RESULTS####

  formatted_results = full_prod %>%
    as.data.frame() %>%
    `rownames<-`(colnames(a)) %>%
    `colnames<-`("Total Requirements") %>%
    mutate(`Initial Production` = case_when(rownames(.) %in% selected_industry ~ sub_x[match(rownames(.),rownames(sub_x))],
                                            T ~ 0),
           `Additional intermediate requirements` = `Total Requirements` - `Initial Production`,
           `Impact intensity per unit of output` = c[match(rownames(.),rownames(c)),1],
           `Distributed footprint` = `Impact intensity per unit of output` * `Total Requirements`) %>%
    select(`Initial Production`,`Additional intermediate requirements`,`Total Requirements`,`Impact intensity per unit of output`,`Distributed footprint`) %>%
    rownames_to_column("industry")

  TOTAL_results = data.frame(industry = "TOTAL",
                             t(colSums(formatted_results[,-1],na.rm = T)),
                             check.names = F) %>%
    select(!c(`Impact intensity per unit of output`))

  if(unlist(round(fpt)) != round(TOTAL_results$`Distributed footprint`)) warning(paste0("Aggregated computed footprint = ",fpt))

  return(list(`Full results` = formatted_results,
              `Totals` = TOTAL_results))
}

#Import all economic data from FIGARO and the corresponding emissions vector from Eurostat 'env_ac_ghgfp' database (Eurostat estimates)

fetch_format_data = function(year)
{
  ####IMPORT FIGARO####

  #Detect corresponding tables

  links = read_html("https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/database") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    subset(grepl("matrix_eu-ic",.) & grepl(year,.))

  linkZ = links %>%
    subset(grepl("io_ind-by-ind",.))

  #Fetch raw data

  rawZ = fread(paste0("https://ec.europa.eu/",linkZ))

  rm(links,linkZ)

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


}

#Format data type by type rather than year by year
build_list_data = function(years,verbose = F)
{
  list_data = vector("list",length = 7) %>%
    `names<-`(c("transaction_flows","distributed_demand","aggregated_demand","intermediate_consumption","production","value_added","direct_emissions"))

  for(year in years)
  {
    raw_yearly_data = fetch_format_data(year)

    list_data$transaction_flows[[as.character(year)]] = raw_yearly_data$transaction_flow

    list_data$distributed_demand[[as.character(year)]] = raw_yearly_data$distributed_demand

    list_data$aggregated_demand[[as.character(year)]] = raw_yearly_data$aggregated_demand

    list_data$intermediate_consumption[[as.character(year)]] = raw_yearly_data$intermediate_consumption

    list_data$production[[as.character(year)]] = raw_yearly_data$production

    list_data$value_added[[as.character(year)]] = raw_yearly_data$value_added

    list_data$direct_emissions[[as.character(year)]] = raw_yearly_data$direct_emissions

    if(verbose) print(year)

  }

  return(list_data)

}

#Based on list_data, return a list of parameters to be used in the SDA loop
sda_params = function(data,previous_year,current_year,selected_industry)
{
  params = vector("list",length = 12) %>%
    `names<-`(c("current_emissions_intensity",
                "previous_emissions_intensity",
                "current_emissions_intensity_previous_ict",
                "previous_emissions_intensity_current_ict",
                "previous_technology",
                "current_technology",
                "current_technology_previous_ict",
                "previous_technology_current_ict",
                "current_final_demand",
                "previous_final_demand",
                "current_final_demand_previous_ict",
                "previous_final_demand_current_ict"))

  #current_emissions_intensity
  params$current_emissions_intensity = data$direct_emissions[[as.character(current_year)]] /
    (rowSums(data$transaction_flows[[as.character(current_year)]]) + data$aggregated_demand[[as.character(current_year)]])

  params[['current_emissions_intensity']][is.nan(unlist(params[['current_emissions_intensity']])) | is.infinite(unlist(params[['current_emissions_intensity']])),1] = 0

  #previous_emissions_intensity
  params$previous_emissions_intensity = data$direct_emissions[[as.character(previous_year)]] /
    (rowSums(data$transaction_flows[[as.character(previous_year)]]) + data$aggregated_demand[[as.character(previous_year)]])

  params[['previous_emissions_intensity']][is.nan(unlist(params[['previous_emissions_intensity']])) | is.infinite(unlist(params[['previous_emissions_intensity']])),1] = 0

  #current_emissions_intensity_previous_ict
  params$current_emissions_intensity_previous_ict = params$current_emissions_intensity

  params$current_emissions_intensity_previous_ict[selected_industry,] = params$previous_emissions_intensity[selected_industry,]

  #previous_emissions_intensity_current_ict
  params$previous_emissions_intensity_current_ict = params$previous_emissions_intensity

  params$previous_emissions_intensity_current_ict[selected_industry,] = params$current_emissions_intensity[selected_industry,]

  #previous_technology
  params$previous_technology = compute_a(
    z = data$transaction_flows[[as.character(previous_year)]],
    d = data$aggregated_demand[[as.character(previous_year)]]
  )

  #current_technology
  params$current_technology = compute_a(
    z = data$transaction_flows[[as.character(current_year)]],
    d = data$aggregated_demand[[as.character(current_year)]]
  )

  #current_technology_previous_ict

  params$current_technology_previous_ict = params$current_technology

  params$current_technology_previous_ict[,selected_industry] = params$previous_technology[,selected_industry]

  #previous_technology_current_ict

  params$previous_technology_current_ict = params$previous_technology

  params$previous_technology_current_ict[,selected_industry] = params$current_technology[,selected_industry]

  #current_final_demand

  params$current_final_demand = data$aggregated_demand[[as.character(current_year)]]

  #previous_final_demand

  params$previous_final_demand = data$aggregated_demand[[as.character(previous_year)]]

  #current_final_demand_previous_ict

  params$current_final_demand_previous_ict = params$current_final_demand

  params$current_final_demand_previous_ict[selected_industry] = params$previous_final_demand[selected_industry]

  #previous_final_demand_current_ict

  params$previous_final_demand_current_ict = params$previous_final_demand

  params$previous_final_demand_current_ict[selected_industry] = params$current_final_demand[selected_industry]

  return(params)

}
