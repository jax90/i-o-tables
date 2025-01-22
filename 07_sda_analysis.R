x = c('dplyr','tidyr','tibble','curl','stringr','ggplot2','eurostat','xml2','rvest','data.table','arrow')

lapply(x,library,character.only = T)

#We aim to divide ICT production footprint into seven terms :
# - Emission intensity contribution
#     - ICT direct emission intensity contribution (additive) 1
#     - ICT suppliers' direct emissions intensity contribution (additive) 2
#
# - Technology change contribution
#     - ICT technology change production (non-additive) 3
#     - ICT suppliers' technology change production (non-additive) 4
#     - Technological interaction term 5
#
# - Final demand contribution
#     - Final demand to the ICT sector contribution (additive) 6
#     - Final demand to the rest of the economy contribution (additive) 7


#Define the time period coverage
year_list = 2010:2021

#Collect data into list_data
list_data = build_list_data(year_list)

#Select all ICT industries across countries
selected_industry = rownames(list_data[[1]][[1]]) %>% subset(grepl("C26|J61|J62_63",.))

#Initialize SDA by computing the 2010 baseline

current_technology = compute_a(
  z = list_data$transaction_flows[["2010"]],
  d = list_data$aggregated_demand[["2010"]])

sda_init =
  io_aggregation_methods(d = as.matrix(list_data$aggregated_demand[["2010"]]),
                         c = as.matrix(list_data$direct_emissions[["2010"]]) / as.matrix(list_data$production[["2010"]]),
                         a = current_technology,
                         selected_industry = selected_industry)

#Store the main results in sda_results

sda_results = sda_init$Totals %>%
  mutate(year = 2010,
         case = 7,
         `ICT Emissions intensity` = 2010,
         `VC Emissions intensity` = 2010,
         `ICT Technology change` = 2010,
         `VC Technology change` = 2010,
         `ICT Final demand` = 2010,
         `VC Final demand` = 2010)

#Perform SDA

for(year in list_year)
{

  current_year = year

  previous_year = year - 1

  params = sda_params(list_data,previous_year = previous_year,current_year = current_year,selected_industry = selected_industry)


  #Case 1 : assess the ICT direct emissions intensity contribution

  sda_case1 =
    io_aggregation_methods(d = params$previous_final_demand,
                           c = params$previous_emissions_intensity_current_ict,
                           a = params$previous_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case1$Totals %>%
            mutate(year = current_year,
                   case = 1,
                   `ICT Emissions intensity` = current_year,
                   `VC Emissions intensity` = previous_year,
                   `ICT Technology change` = previous_year,
                   `VC Technology change` = previous_year,
                   `ICT Final demand` = previous_year,
                   `VC Final demand` = previous_year))

  #Case 2 : assess ICT suppliers' direct emissions intensity contribution

  sda_case2 =
    io_aggregation_methods(d = params$previous_final_demand,
                           c = params$current_emissions_intensity_previous_ict,
                           a = params$previous_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case2$Totals %>%
            mutate(year = current_year,
                   case = 2,
                   `ICT Emissions intensity` = previous_year,
                   `VC Emissions intensity` = current_year,
                   `ICT Technology change` = previous_year,
                   `VC Technology change` = previous_year,
                   `ICT Final demand` = previous_year,
                   `VC Final demand` = previous_year))



  #Case 3 : assess direct production technology contribution

  sda_case3 =
    io_aggregation_methods(d = params$previous_final_demand,
                           c = params$previous_emissions_intensity,
                           a = params$previous_technology_current_ict,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case3$Totals %>%
            mutate(year = current_year,
                   case = 3,
                   `ICT Emissions intensity` = previous_year,
                   `VC Emissions intensity` = previous_year,
                   `ICT Technology change` = current_year,
                   `VC Technology change` = previous_year,
                   `ICT Final demand` = previous_year,
                   `VC Final demand` = previous_year))



  #Case 4 : assess ICT suppliers' production technology contribution

  sda_case4 =
    io_aggregation_methods(d = params$previous_final_demand,
                           c = params$previous_emissions_intensity,
                           a = params$current_technology_previous_ict,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case4$Totals %>%
            mutate(year = current_year,
                   case = 4,
                   `ICT Emissions intensity` = previous_year,
                   `VC Emissions intensity` = previous_year,
                   `ICT Technology change` = previous_year,
                   `VC Technology change` = current_year,
                   `ICT Final demand` = previous_year,
                   `VC Final demand` = previous_year))

  #Case 5 : assess ICT final demand contribution

  sda_case5 =
    io_aggregation_methods(d = params$previous_final_demand_current_ict,
                           c = params$previous_emissions_intensity,
                           a = params$previous_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case5$Totals %>%
            mutate(year = current_year,
                   case = 5,
                   `ICT Emissions intensity` = previous_year,
                   `VC Emissions intensity` = previous_year,
                   `ICT Technology change` = previous_year,
                   `VC Technology change` = previous_year,
                   `ICT Final demand` = current_year,
                   `VC Final demand` = previous_year))

  #Case 6 : assess ICT final demand contribution

  sda_case6 =
    io_aggregation_methods(d = params$current_final_demand_previous_ict,
                           c = params$previous_emissions_intensity,
                           a = params$previous_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case6$Totals %>%
            mutate(year = current_year,
                   case = 6,
                   `ICT Emissions intensity` = previous_year,
                   `VC Emissions intensity` = previous_year,
                   `ICT Technology change` = previous_year,
                   `VC Technology change` = previous_year,
                   `ICT Final demand` = previous_year,
                   `VC Final demand` = current_year))


  #Case 7 : assess emissions intensity contribution

  sda_case7 =
    io_aggregation_methods(d = params$previous_final_demand,
                           c = params$current_emissions_intensity,
                           a = params$previous_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case7$Totals %>%
            mutate(year = current_year,
                   case = 7,
                   `ICT Emissions intensity` = current_year,
                   `VC Emissions intensity` = current_year,
                   `ICT Technology change` = previous_year,
                   `VC Technology change` = previous_year,
                   `ICT Final demand` = previous_year,
                   `VC Final demand` = previous_year))

  #Case 8 : assess technology contribution

  sda_case8 =
    io_aggregation_methods(d = params$previous_final_demand,
                           c = params$previous_emissions_intensity,
                           a = params$current_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case8$Totals %>%
            mutate(year = current_year,
                   case = 8,
                   `ICT Emissions intensity` = previous_year,
                   `VC Emissions intensity` = previous_year,
                   `ICT Technology change` = current_year,
                   `VC Technology change` = current_year,
                   `ICT Final demand` = previous_year,
                   `VC Final demand` = previous_year))

  #Case 9 : assess final demand contribution

  sda_case9 =
    io_aggregation_methods(d = params$current_final_demand,
                           c = params$previous_emissions_intensity,
                           a = params$previous_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case9$Totals %>%
            mutate(year = current_year,
                   case = 9,
                   `ICT Emissions intensity` = previous_year,
                   `VC Emissions intensity` = previous_year,
                   `ICT Technology change` = previous_year,
                   `VC Technology change` = previous_year,
                   `ICT Final demand` = current_year,
                   `VC Final demand` = current_year))

  #Case 10 : Current year Production footprint

  sda_case10 =
    io_aggregation_methods(d = params$current_final_demand,
                           c = params$current_emissions_intensity,
                           a = params$current_technology,
                           selected_industry = selected_industry)

  sda_results = sda_results %>%
    rbind(sda_case10$Totals %>%
            mutate(year = current_year,
                   case = 10,
                   `ICT Emissions intensity` = current_year,
                   `VC Emissions intensity` = current_year,
                   `ICT Technology change` = current_year,
                   `VC Technology change` = current_year,
                   `ICT Final demand` = current_year,
                   `VC Final demand` = current_year))


  }

write_parquet(sda_results,
              paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/sda_results_raw.parquet"))

#REMAINING WORK :

# -add rows for technology interaction term
# -deduct contributions over the time serie
# -format a barplot composed of each contribution (geom_bar) and a net total term (geom_point)
#
# -apply the method to the rest of the economy for comparison purposes.
# -write the corresponding paper section
# -cite some suited references
