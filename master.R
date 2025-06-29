main_path <- "/home/jannaaxe/Schreibtisch/Projekte/IO-analysis"
user = "jax"

setwd(main_path)

x = c('tidyverse','data.table','arrow','leontief','here',
      'curl','stringr','eurostat','xml2','rvest','countrycode','here',
      'leontief','ggthemes','xtable', 'networkD3', 'jsonlite', 'ggsankeyfier')

lapply(x, library,character.only = T)

edition = "23"
start_year = "2010"
end_year = "2021"

lapply(
  lapply(c('code/01_aggregate_FIGARO_files.R',
          'code/02_deflation_procedure.R',
            'code/03_fpt_computations.R',
           'code/04_main_results.R',
           'code/05_decomposition_time_trends.R',
           'code/06_sankey.R'
           ), here),
  source)
