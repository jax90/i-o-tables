library(tidyverse)
library(tibble)
library(matlib)
library(leontief)
library(corpcor)

# Set working directory
setwd("C:/Users/JAX/Desktop/sektor_analyse")

# Read data files
leontief_weights <- read_rds("./data/Leontief_weights_23_data.rds")
emissions <- read_rds("./data/emissions/co2e_emission_agg.rds")
value_added_agg <- read_rds("./data/values/values_agg.rds") 

merge_prep <- emissions |> 
  unite(industry_ref_area, industry, ref_area, sep = "_") |> 
  group_by(industry_ref_area, time_period) |> 
  mutate(direct_emissions = sum(obs_value)) |> 
  mutate(time_period = as.character(time_period)) |> 
  ungroup() |> 
  select(time_period, industry_ref_area, direct_emissions) |> 
  distinct() 

# Merge data and calculate emissions per output
df <- merge_prep |> 
  full_join(leontief_weights, by = c("industry_ref_area", "time_period")) |> 
  mutate(emissions_per_output = direct_emissions / X_total) |> 
  mutate(emissions_per_output = if_else (is.infinite(emissions_per_output) & Y_total == 0, 
                                         direct_emissions, emissions_per_output)) |> 
  filter(!(if_any(everything(), ~ is.na(.x) & direct_emissions == 0)))

#generate a data frame that allows to consider embodied emissisions with respect
# different kinds of demand

y_country <- value_added_agg |> 
  select(
    starts_with("P3_S13"),  # Y
    starts_with("P3_S14"),  # Y
    starts_with("P3_S15"),  # Y
    starts_with("P51G"),    # Y
    starts_with("P5M"),     # Y
    time_period, counterpart_area, ref_area, industry
  ) |> 
  pivot_longer( 
    cols=matches("^P"), 
    names_to= "sto",
    values_to="Y"
  ) |> 
  unite(industry_ref_area, industry, ref_area, sep = "_") 


add_embodied_emissions <- function(df, year , y_country, value_added_agg){ 
  
  # Print the current time period
  print(year)
  
  # Filter data for the specified time period
  df <- df |> 
    filter(time_period == year) 
  
  rel_industries <- intersect(colnames(df), df$industry_ref_area)
  # Calculate matrices B, L, and Y
  B <- df$emissions_per_output |> na.omit() |> as.matrix()
  L <- df |> 
    select(rel_industries) |> 
    filter(df$industry_ref_area %in% rel_industries) |> 
    as.matrix()
  
  rownames(L) <- rel_industries
  colnames(L) <- rel_industries

  
  ## modify the Leontief inverse to consider demand in other industries
  
  Int <- matrix(0, nrow = dim(L)[1], ncol = dim(L)[1])
  
  # initialize L adjusted  
  L_adjust <- L
  
  #now the actual adjustment starts:
  
  for (i in df |> select(matches("(26|61|62|63)")) |> colnames()) {
    
    #equation 8 / equation 11  
    Int <- Int + (1 / L_adjust[i, i] *  L_adjust[, i]  %*%   t(L_adjust[i, ])   )
    
    # euqation 9 
    L_adjust  <- L -  Int
    
  }
  
  # Calculate embodied emissions
  # iterate of different types of countries 
  for (country in y_country |> select(counterpart_area) |> distinct() |>  pull()){
    
    #iterate over different types of demand
    for (demand_type in c("P3_S13", "P3_S14", "P3_S15","P51G", "P5M")){
    
    # pick the right demand type y
    y <- df |> inner_join(y_country |> filter(time_period ==year,
                                              counterpart_area == country,
                                              sto == demand_type), 
                          by = c("industry_ref_area"))|> pull(paste0(demand_type, "_", country)) |> na.omit() 
    
    
    Y <- matrix(0, nrow = length(y), ncol = length(y))
    diag(Y) <- y
    
    # Calculate embodied emissions
    embodied_emissions <- t(B) %*% Int %*% Y
  
    # prepare emboded emisssions fpr merge
    df_embodied <- tibble(
      industry_ref_area = rel_industries) |> 
      mutate(!!paste0("embodied_emissions_", demand_type, "_", country) := embodied_emissions[1,])
    
    # Add embodied emissions to the dataframe
    df <- df |> 
      full_join(df_embodied, by = c("industry_ref_area"))
    
  }}
  
  # add the adjusted Leontief weights to the dataframe
  colnames(Int) <- paste0("Int_wght_", rel_industries)
  
  Int <- Int |>  as_tibble() |> 
    mutate(industry_ref_area = rel_industries)
  
  df <- df |> 
    full_join(Int, by = c("industry_ref_area"))
  
  
  # add scope 2 emissions
  industry_names <- value_added_agg |> 
    select(-ref_area, -industry, -time_period, -counterpart_area  ) |> 
    colnames()
  
  # add Scope 2 emissions
  scope2 <- value_added_agg |> 
    filter(time_period == year) |> 
    unite(industry_ref_area, industry, ref_area, sep = "_") |> 
    filter(grepl("D35", industry_ref_area)) |> 
    pivot_wider(
      names_from = counterpart_area, 
      values_from = all_of(industry_names), 
      names_glue = "{.value}_{counterpart_area}") |> 
    inner_join(df |>  select(industry_ref_area,emissions_per_output ), by = "industry_ref_area" ) |> 
    mutate(across(all_of(rel_industries), ~ . * emissions_per_output)) |>
    select(-time_period,-industry_ref_area) |> 
    summarize(across(everything(),\(x) sum(x, na.rm = TRUE))) |> 
    pivot_longer(cols = everything(), 
                 names_to = "industry_ref_area", 
                 values_to = "scope_2") 
    
  df <- df |> 
    full_join(scope2, by = c("industry_ref_area"))
  
  
  # add Scope 3 emissions (calculate Scope 3 emissions by the productions 
  # perspective (see Charpentier & Blain 2024))
  
  x <-  df$X_total |> na.omit() |> as.matrix()
  X <- matrix(0, nrow = length(x), ncol = length(x))
  diag(X) <- x
  
  correction_factor<- diag(L)
  embodied_emissions_production <- t(B) %*% L  %*% X
  
  production_footprint <- tibble(
    industry_ref_area = rel_industries,
    production_footprint= embodied_emissions_production[1,],
    correction_factor = correction_factor) |> 
    mutate(production_footprint = production_footprint/ correction_factor)  |> 
    select(-correction_factor)
  
  
  df <- df |> 
    full_join(production_footprint, by = c("industry_ref_area"))
  
 
  return(df)
}

# Apply the function to each unique time period and bind the results into a dataframe
df_with_embodied_emissions <- unique(df$time_period) |> 
  map(\(.period){add_embodied_emissions(df, .period,y_country, value_added_agg)}) |> 
  bind_rows() |> 
  as_tibble()

# Save the resulting dataframe as an RData file
write_rds(df_with_embodied_emissions, "data/Leontief_weights_ghgs_all_23_data.rds", compress = "gz")


