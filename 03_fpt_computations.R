x = c('tidyverse','data.table','arrow','leontief','here')
lapply(x, library,character.only = T)

lapply(lapply(c('00_aggregate_CO2_emission_files.R',
         '01_aggregate_value_added_files.R',
         '02_deflation_procedure.R'),here),source)

values_agg = format_iot(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                        exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                        update = F) %>%
  filter(rowLabels %in% colnames(.))

emissions <- format_emissions(exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                              update = F) %>%
  unite(resource_id, ref_area,industry, sep = "_") |>
  group_by(resource_id,time_period) %>%
  summarise(direct_emissions = sum(obs_value,na.rm = T)) %>%
  mutate(time_period = as.character(time_period)) %>%
  ungroup()


X_time_serie = data.frame(value = rowSums(values_agg %>% select(!c(time_period,rowLabels)),na.rm=T),
                          values_agg %>% select(c(time_period,rowLabels)))

emissions_bis =  format_emissions(exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                                  update = F,
                                  version = 'online') %>%
  unite(resource_id, ref_area,industry, sep = "_") %>%
  mutate(output = X_time_serie$value[match(paste0(time_period,resource_id),
                                           paste0(X_time_serie$time_period,X_time_serie$rowLabels))],
         intens = obs_value / output) %>%
  filter(!is.na(output))

eeio_computations = function(input_output,
                             emissions,
                             verbose = T)
{

  if(verbose) print("Initialisation of variables")

  X = rowSums(input_output,na.rm = T)

  B = emissions %>%
    mutate(production = X[match(resource_id,names(X))]) %>%
    reframe(value = case_when(production == 0 ~ 0,
                              T ~ direct_emissions / production))


  Ds = input_output[,grepl('P3|P5',colnames(input_output))] %>%
    as.matrix()

  D = rowSums(Ds,na.rm = T)

  Z =
    input_output %>%
    select(all_of(rownames(.))) %>%
    as.matrix()

  # the Leontief inverse is not invertible in two situations
  #
  # 1) if a column has only a non-zero value which is at the
  #  diagonal (because of (1-A) would equal zero if A =X_iJ/X_j = 1) therefore columns are excluded when
  # the column sum equals the diagonal
  #
  # 2) if the column sum equals zero because x_ij is not solvable

  diag(Z)[diag(Z) == colSums(Z,na.rm = T)] = 0

  A = sweep(Z,2,as.numeric(X),`/`) ; A[is.nan(A) | is.infinite(A)] = 0

  if(verbose) print(paste0("Inversing (I-A) matrix..."))

  L = L_adjust = solve(diag(nrow = nrow(A)) - A)


  if(verbose) print(paste0("Computing production footprints..."))


  Int <- matrix(0, nrow = dim(L_adjust)[1], ncol = dim(L_adjust)[1])

  selected_industry_num = grep('26|61|62|63',colnames(x=L_adjust))

  for(i in selected_industry_num)
  {
    #equation 8 / equation 11
    Int <- Int + (1 / L_adjust[i, i] *  L_adjust[, i]  %*%   t(L_adjust[i, ]))

    # equation 9
    L_adjust  <- L -  Int
  }

  if(verbose) print(paste0("Computing FD footprints..."))

  marginal_carbon_per_demand = t(B) %*% Int

  distributed_fd_fpt = list()

  for(i in 1:ncol(Ds))
  {

  embedded_emissions_di = t(sweep(marginal_carbon_per_demand,2,Ds[,i],`*`)) %>%
    as.data.frame() %>%
    `colnames<-`(paste0("embodied_emissions_",colnames(Ds)[i]))

  distributed_fd_fpt[[i]] = embedded_emissions_di

  }
  distributed_fd_fpt = as.data.frame(distributed_fd_fpt) %>%
    rownames_to_column('resource_id')

  #Decompose ICT sector scope 2 and 3 simply by aggregating non exogenous measure would overcount fpt by 0.3 GT in 2021
  #It is required to decompose the analysis within a system where ICT sectors are exogenous, by taking advantage of the production method (2)

  sub_invL = solve(L[selected_industry_num,selected_industry_num])

  prescaling_sub_prod = sub_invL %*% X[selected_industry_num,drop = F]

  full_prod = L[,selected_industry_num] %*% diag(x = as.numeric(prescaling_sub_prod))

  distributed_fpt = diag(x = unlist(B)) %*% full_prod

  total_fpt = colSums(distributed_fpt,na.rm = T)

  E_ict = diag(x = emissions$direct_emissions) ; E_ict = E_ict[,selected_industry_num]

  indirect_fpt = distributed_fpt - E_ict

  scope2 = colSums(indirect_fpt[grep('D35',colnames(x=L_adjust)),],na.rm = T)
  scope3 = colSums(indirect_fpt[-grep('D35',colnames(x=L_adjust)),],na.rm = T)

  production_fpt_elements = data.frame(
    resource_id = rownames(L_adjust)[selected_industry_num],
    scope_2 = scope2,
    scope_3 = scope3,
    production_footprint = total_fpt
  )

  # scope1 = emissions$direct_emissions[selected_industry_num]
  # print(cbind(scope1+scope2+scope3,total_fpt)) #IT WORKS

  results_table = L %>%
    as.data.frame() %>%
    rownames_to_column('resource_id') %>%
    left_join(as.data.frame(Ds) %>%
                rownames_to_column('resource_id'),
              by = 'resource_id') %>%
    left_join(as.data.frame(Int) %>%
                `rownames<-`(colnames(.)) %>%
                `colnames<-`(paste0("Intwght_",colnames(.))) %>%
                rownames_to_column('resource_id'),
              by = 'resource_id') %>%
    left_join(distributed_fd_fpt,by = 'resource_id') %>%
    mutate(total_final_demand = D,
           total_output = X,
           direct_emissions = emissions$direct_emissions[match(resource_id,emissions$resource_id)]) %>%
    left_join(production_fpt_elements,by = 'resource_id')


  return(results_table)

}


# previous_results = readRDS("C:/Users/joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23/Leontief_weights_ghgs_all_23_data.rds")
# previous_results %>%
#   filter(time_period == 2021) %>%
#   select(production_footprint,industry_ref_area) %>%
#   mutate(industry = substr(industry_ref_area,1,3)) %>%
#   filter(grepl('26|61|62|63',industry)) %>%
#   summarise(value = sum(production_footprint))

eeio_analysis = function(values_agg,
                         emissions,
                         file_name,
                         exdir,
                         basis = 2020,
                         update = F,
                         verbose = T)
{
  if(!update)
  {
    if(file.exists(paste0(exdir,"/",file_name)))
    {
      if(verbose) print('Returning cached data')
      return(read_parquet(paste0(exdir,"/",file_name)))
    }
    if(verbose) print('File does not exists yet, updating...')
  }

  if(verbose) print('Computing price indexes and deflating...')

  price_index = get_value_added_price_index(basis) |>
    rename(deflator = value,
           ref_area = country,
           time_period = year) |>
    select(-base)

  if(basis != F)
  {
    values_agg =
    values_agg %>%
    separate(rowLabels,into = c('ref_area','industry'),sep = "_",remove = F,extra = 'merge') %>%
    full_join(price_index, by = c("ref_area" ,"industry" ,"time_period")) |>
    mutate(across(where(is.numeric), \(.x){.x*deflator} )) |>
    select(-c("ref_area","industry","deflator"))
  }


  sto_results = c()

  for(time_period in unique(values_agg$time_period))
  {


  if(verbose) print(paste0("Formatting ",time_period," IOTs..."))

    input_output =
      values_agg %>%
      filter(time_period == !!time_period) |>
      select(!time_period) %>%
      filter(rowLabels %in% colnames(.)) %>% #rm W2 components (essentially value added and TLS)
      column_to_rownames('rowLabels')

    emissions_year = emissions %>%
      filter(time_period == !!time_period) %>%
      sort_id('resource_id') %>%
      filter(!grepl("HH",resource_id))

    results = eeio_computations(input_output,
                                emissions_year,
                                verbose) %>%
      mutate(time_period = !!time_period)


    sto_results = rbind(sto_results,results)

  }

  if(verbose) print(paste0('Data cached in ',exdir,"/",file_name))

  write_parquet(sto_results, paste0(exdir,"/",file_name))

  return(sto_results)
}

# eeio_analysis(values_agg = values_agg,
#               emissions = emissions,
#               basis = 2021,
#               file_name = "footprint_results_23_data.parquet",
#               exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
#               update = T)




