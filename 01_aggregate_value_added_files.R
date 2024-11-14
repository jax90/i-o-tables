library(tidyverse)

setwd("C:/Users/JAX/Desktop/sektor_analyse")

# Pfad zum Ordner, in dem sich die Dateien befinden
path <- "./data/values"

# Liste der Dateien im Ordner, die mit einer Zahl beginnen
filenames <- list.files(path, pattern = "matrix_eu-ic-io_ind-by-ind_23ed", full.names = TRUE)

read_csv_with_time <- function(file_path) {
  # Read the CSV file
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Extract time period from filename
  time_period <- gsub(".*_(\\d{4})\\.csv", "\\1", file_path)
  
  # Add 'time_period' column
  df$time_period <- time_period
  
  return(df)
}

# Read and combine CSV files while adding 'time_period' column
df <- filenames |> 
  map_dfr(read_csv_with_time) |> 
  separate(rowLabels, into = c("ref_area", "industry"), sep = "_",  extra = "merge") 


df_long <- df |> 
  pivot_longer(
    cols = -c(ref_area, industry, time_period),
    names_to = c("counterpart_area", ".value"),
    names_pattern = "^(\\D+1?)_(\\w*_?)"  )
# Display the resulting dataframe
print(df_long)

# Save as RDS file
readr::write_rds(df_long, "data/values/values_agg_23.rds", compress="gz")
