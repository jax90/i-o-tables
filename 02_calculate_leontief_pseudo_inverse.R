x = c('tidyverse','data.table','arrow','leontief')
lapply(x, library,character.only = T)


values_agg = format_iot(folder = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                        exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                        update = F)

generate_leontief = function(values_agg,
                             exdir,
                             basis = 2020,
                             update = F,
                             verbose = T)
{

  if(!update)
  {
    if(file.exists(paste0(exdir,"/Leontief_weights_23_data.parquet")))
    {
      if(verbose) print('Returning cached data')
      return(read_parquet(paste0(exdir,"/Leontief_weights_23_data.parquet")))
    }
    if(verbose) print('File does not exists yet, updating...')
  }

  if(verbose) print('Computing price indexes and deflating...')

  price_index = get_value_added_price_index(basis) |>
    rename(deflator = value,
           ref_area = country,
           time_period = year) |>
    select(-base)

  deflated_values_agg =
    values_agg %>%
    separate(rowLabels,into = c('ref_area','industry'),sep = "_",remove = F,extra = 'merge') %>%
    full_join(price_index, by = c("ref_area" ,"industry" ,"time_period")) |>
    mutate(across(where(is.numeric), \(.x){.x*deflator} )) |>
    select(-deflator)

  if(verbose) print(paste0("Formatting ",.time_period," IOTs..."))

  sto_L = c()

  for(time_period in unique(values_agg$time_period))
  {

  input_output =
    values_agg %>%
    filter(time_period == !!time_period) |>
    select(!time_period) %>%
    filter(rowLabels %in% colnames(.)) %>% #rm W2 components (essentially value added and TLS)
    column_to_rownames('rowLabels')

  X = rowSums(input_output,na.rm = T)

  D = rowSums(input_output[,grepl('P3|P5',colnames(input_output))],na.rm = T)

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

  if(verbose) print(paste0("Inversing ",time_period," matrix..."))

  L = solve(diag(nrow = nrow(A)) - A) %>%
    as.data.frame() %>%
    rownames_to_column("resource_id") %>%
    mutate(time_period = time_period,
           final_demand = D,
           total_output = X) %>%
    left_join(input_output %>%
                rownames_to_column('resource_id') %>%
                select(
                  contains("P3_S13"), contains("P3_S14"),resource_id,
                  contains("P3_S15"), contains("P51G"), contains("P5M")
                ),
              by="resource_id")

  sto_L = rbind(sto_L,L)
  }

  if(verbose) print(paste0('Data cached in ',exdir,"/Leontief_weights_23_data.parquet"))

  write_parquet(sto_L, paste0(exdir,"/Leontief_weights_23_data.parquet"))

  return(sto_L)

}

generate_leontief(values_agg,
                  exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                  update = T)


