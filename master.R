user = "jax"
user = "joris"

if(user == "jax") main_path = "/home/jannaaxe/Schreibtisch/Projekte/IO-analysis"
if(user == "joris") main_path = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23"

setwd(main_path)

x = c('tidyverse','data.table','arrow','leontief','here',
      'curl','stringr','eurostat','xml2','rvest','countrycode','here',
      'leontief','ggthemes','xtable', 'networkD3', 'jsonlite', 'ggsankeyfier')

lapply(x, library,character.only = T)

edition = "23"
start_year = "2010"
end_year = "2021"


lapply(list.files(here(),full.names = T,pattern = "0"),source)
