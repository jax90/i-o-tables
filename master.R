# ------------------------------------------------------------------
# MASTER SCRIPT: Global Embodied Emissions of Digital Technologies
# ------------------------------------------------------------------
# This script orchestrates the entire analysis pipeline, from data
# processing to generating the final results and figures for the paper.
#
# INSTRUCTIONS:
# 1. Indicate the local directory of the raw data in the `data_directory` object, if applicable.
# 2. Run this script from the console.
#
# It will automatically handle paths and run all sub-scripts in the
# correct order.
# ------------------------------------------------------------------
# --- SETUP: Configure Environment and Parameters ---

# Clear workspace to ensure a clean run
rm(list = ls())

# User should set the local directory location of the initial dataset, if applicable.

#AXENBECK
#data_directory <- "/home/jannaaxe/Schreibtisch/Projekte/IO-analysis"

#BLAIN
#data_directory <- "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23"


# Load all required packages for the project
message("Loading all required packages...")
project_packages <- c('tidyverse','data.table','arrow','here','progress','leontief','readxl','countrycode','eurostat',
                      'xml2','rvest','ggthemes','xtable','ggsankeyfier','jsonlite')

lapply(project_packages, library, character.only = TRUE)

# Define global analysis parameters
# These can be easily changed here to re-run the analysis for different editions or time periods.
edition    <- "23"
start_year <- "2010"
end_year   <- "2021"

message(paste0(
  "Analysis configured for FIGARO edition: '", edition,
  "' | Time period: ", start_year, " - ", end_year
))

# --- ANALYSIS WORKFLOW: Execute Scripts in Order ---

message("\n--- STEP 1 of 6: Aggregating raw FIGARO data files ---")
source(here("01_aggregate_FIGARO_files.R"))

message("\n--- STEP 2 of 6: Building price deflators ---")
source(here("02_deflation_procedure.R"))

message("\n--- STEP 3 of 6: Computing carbon footprints (EEIO analysis) ---")
source(here("03_fpt_computations.R"))

message("\n--- STEP 4 of 6: Generating main results (figures and tables) ---")
source(here("04_main_results.R"))

message("\n--- STEP 5 of 6: Performing decomposition analysis ---")
source(here("05_decomposition_time_trends.R"))

message("\n--- STEP 6 of 6: Generating Sankey diagrams ---")
source(here("06_sankey.R"))


# --- SUPPLEMENTARY ANALYSIS (Optional) ---
# The uncertainty analysis is a long-running process and is not part of the
# default pipeline. Uncomment the line below to run it.
#
# message("\n--- OPTIONAL STEP: Running Monte Carlo uncertainty analysis ---")
# source(here("code", "07_uncertainty_analysis.R"))
