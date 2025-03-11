main_path <- "/home/jannaaxe/Schreibtisch/Projekte/IO-analysis"
user = "jax"

x = c('tidyverse','data.table','arrow','leontief','here',
      'curl','stringr','eurostat','xml2','rvest','countrycode','here',
      'leontief','ggthemes','xtable', 'networkD3', 'jsonlite', 'ggsankeyfier')

lapply(x, library,character.only = T)

edition = "23"
start_year = "2010"
end_year = "2021"

lapply(
  lapply(c(#'01_aggregate_FIGARO_files.R',
         #  '02_deflation_procedure.R',
          #  '03_fpt_computations.R',
          # '04_main_results.R',
          # '05_decomposition_time_trends.R'
           '06_sankey.R'
           ), here),
  source)
