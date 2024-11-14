library(tidyverse)
library(matlib)
library(leontief)
library(corpcor)

setwd("C:/Users/JAX/Desktop/sektor_analyse")

value_added_agg <- read_rds("./data/values/values_agg_23.rds") #|> 
 # filter(ref_area == "AR")

generate_Leontief <- function(.value_added_agg, .time_period ){
  
  #prepere for input-output representation
  input_output <-
    .value_added_agg |>
    filter(time_period == .time_period) |> 
    select(-time_period) |> 
    pivot_longer(
      cols = -c(ref_area, industry, counterpart_area),
      names_to = "counterpart_industry",
      values_to = "value"  ) |> 
    unite(industry_ref_area, industry, ref_area, sep = "_") |> 
    unite(industry_counterpart_area, counterpart_industry, counterpart_area, sep = "_")
 
  
  # the Leontief inverse is not invertible in two situations
  #
  # 1) if a column has only a non-zero value which is at the  
  #  diagonal (because of (1-A) would equal zero if A =X_iJ/X_j = 1) therefore columns are excluded when 
  # the column sum equals the diagonal 
  #
  # 2) if the column sum equals zero because x_ij is not solvable
  
  input_output <-
    
    input_output|> 
    group_by(industry_counterpart_area) |>
      mutate(total =sum (value)) |> # caluclate column sum
    ungroup() |>
    mutate(value = if_else(
        value == total & industry_ref_area == industry_counterpart_area, # check whether a column has a non-zero value only at the column sum
        0, value
      )
    ) |>
    select(-total) |>
    #generate input-output format
    pivot_wider(
      names_from = industry_counterpart_area  ,
      values_from = value
    ) 
  # ensure squared matrix
  overlap_values <- intersect(input_output$industry_ref_area |> unique(), 
                              input_output  |> colnames())
  
  # include only relevant information
  X_prep <- input_output|> 
    filter(industry_ref_area %in% overlap_values) |>  
    select(
      starts_with("P3_S13"), #Y
      starts_with("P3_S14"), #Y
      starts_with("P3_S15"), #Y
      starts_with("P51G"),#Y
      starts_with("P5M"),#Y
      all_of(overlap_values) #X
    )  |> 
    mutate(Y_total = rowSums(across(-all_of(overlap_values)))) |> 
    mutate(X_total = rowSums(across(-Y_total))) |> 
   # mutate(across(overlap_values, ~ ./(X_total))) |> 
  #  mutate( Y = across(Y_total, ~ ./(X_total))) |> 
    select(
      - starts_with("P3_S13"),   # only keep the total Y value
      - starts_with("P3_S14"), 
      - starts_with("P3_S15"), 
      - starts_with("P51G"),
      - starts_with("P5M") 
    ) |> drop_na()
  
# prepare matrix A -> x_ij
  A_prep <- X_prep |> 
    select(
      all_of(overlap_values)
    ) |> 
    as.matrix()
  
  # prepere vector x_j
  d <- X_prep$X_total |> as.matrix() 
  
  #generate matrix A
  A <- input_requirement(A_prep, d)
  
  A <- A |> as_tibble() |>  mutate(across(everything(), ~replace_na(., 0))) |> as.matrix()


  print(A)
  
  I <- diag(dim(A)[1])
  
  #generate Leontief inverse
  L <- pseudoinverse(I-A)
  
  print(L)   
  colnames(L) <- overlap_values

  print(.time_period)
  
  # additional information to the Leontief inverse
  L_aug <-   L |> as_tibble() |> 
    mutate(time_period = .time_period) |>  # add time_period
    mutate(industry_ref_area = overlap_values ) |> # add row names
    mutate(Y_total = X_prep$Y_total) |>  # add aggregated output
    mutate(X_total = X_prep$X_total) |>  # add aggregated output
    left_join( input_output |> select( # add disaggregated output
        industry_ref_area, starts_with("P3_S13"), starts_with("P3_S14"), 
        starts_with("P3_S15"), starts_with("P51G"), starts_with("P5M")
        ), 
      by="industry_ref_area") 
        
  #test whether everything is probably calculated
  print(sum(d)) # industrie inputs + privat vergnügen
  print(sum(L %*% X_prep$Y_total |>  as.matrix())) # leontieff weights * privat vergnügen
  
  return(L_aug)

}

# # calculate leontief weights for several years
Leontief_frame <-
  unique(value_added_agg$time_period) |>
  map(\(.period){generate_Leontief(value_added_agg, .period)}) |>
  bind_rows() |>
  as_tibble()


# Save as RData file
write_rds(Leontief_frame, "data/Leontief_weights_23_data.rds", compress="gz")


