library(tidyverse)
library(readxl)

setwd("C:/Users/JAX/Desktop/sektor_analyse")

# Pfad zum Ordner, in dem sich die Dateien befinden
path <- "./data/emissions"

# Liste der Dateien im Ordner, die mit einer Zahl beginnen
filenames <- list.files(path, pattern = "ghg", full.names = TRUE)

# Anzeigen der ausgewählten Dateien
print(filenames)

df <- read.csv(filenames[1]) |> 
  as_tibble()

# Display the content of the first CSV file as a tibble
print(df)

#upload dataframes and bind row-wise
df <- filenames |> 
  map_dfr(read_csv, show_col_types=FALSE)


emissions2015 <- read_excel("./data/emissions/AggregationReport_23ed_GHGfp.xlsx", sheet = "Matrix")

# Save as RData file
write_rds(df, "data/emissions/co2e_emission_agg.rds", compress="gz")
