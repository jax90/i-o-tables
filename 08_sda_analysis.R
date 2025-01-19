x = c('dplyr','tidyr','tibble','curl','stringr','ggplot2','eurostat','xml2','rvest','data.table','arrow','ggthemes')

lapply(x,library,character.only = T)

results = read_parquet("C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23/footprint_results_23_data.parquet")

sda_analysis = function(results,verbose = T)
{
  time_span = sort(unique(results$time_period))

  sda_results = c()

  for(year in as.numeric(time_span[-1]))
  {

    e0 = results %>%
      filter(time_period == (year-1)) %>%
      column_to_rownames('resource_id') %>%
      select(production_footprint)

    L0 = results %>%
      filter(time_period == (year-1)) %>%
      column_to_rownames('resource_id') %>%
      select(matches("Intwght")) %>%
      as.matrix()

    b0 = results %>%
      filter(time_period == (year-1)) %>%
      column_to_rownames('resource_id') %>%
      select(direct_emissions,total_output) %>%
      mutate(value = case_when(total_output == 0 ~ 0,
                                T ~ direct_emissions / total_output)) %>%
      select(value) %>%
      as.matrix()

    D0 = results %>%
      filter(time_period == (year-1)) %>%
      column_to_rownames('resource_id') %>%
      select(total_final_demand) %>%
      as.matrix()

    e1 = results %>%
      filter(time_period == (year)) %>%
      column_to_rownames('resource_id') %>%
      select(production_footprint)

    L1 = results %>%
      filter(time_period == (year)) %>%
      column_to_rownames('resource_id') %>%
      select(matches("Intwght"))  %>%
      as.matrix()

    b1 = results %>%
      filter(time_period == (year)) %>%
      column_to_rownames('resource_id') %>%
      select(direct_emissions,total_output) %>%
      mutate(value = case_when(total_output == 0 ~ 0,
                               T ~ direct_emissions / total_output)) %>%
      select(value) %>%
      as.matrix()

    D1 = results %>%
      filter(time_period == (year)) %>%
      column_to_rownames('resource_id') %>%
      select(total_final_demand) %>%
      as.matrix()

    #t(b0) %*% L0 %*% D0 #should equal sum(e0,na.rm = T)

    #Non unique solution, we implement a polar decomposition method, see Dietzenbacher and Los (1998)
    #Dietzenbacher, E., & Los, B. (1998). Structural decomposition techniques: sense and sensitivity. Economic Systems Research, 10(4), 307-324.

    dif_B = (1/2) * t(b1 - b0) %*% L1 %*% D1 + (1/2) * t(b1 - b0) %*% L0 %*% D0

    dif_L = (1/2) * t(b0) %*% (L1 - L0) %*% D1 + (1/2) * t(b1) %*% (L1 - L0) %*% D0

    dif_D = (1/2) * t(b0) %*% L0 %*% (D1 - D0) + (1/2) * t(b1) %*% L1 %*% (D1 - D0)

    if((round(dif_B + dif_L + dif_D) != round(sum(e1,na.rm=T) - sum(e0,na.rm=T))) & verbose) print(paste0('Error detected :',year)) #IT WORKS

    sda_year = data.frame(`Direct emission intensity` = as.numeric(dif_B),
                          `Technology change` = as.numeric(dif_L),
                          `Final demand` = as.numeric(dif_D),
                          time_period = year,
                          Start = NA,
                          End = NA) %>%
      pivot_longer(1:3,names_to = "Contributions",values_to = "Amount",names_repair = 'minimal')

  sda_year$Start[1] = sum(e0,na.rm=T)
  sda_year$End[1] = sda_year$Start[1] + sda_year$Amount[1]

  sda_year$Start[2] = sda_year$End[1]
  sda_year$End[2] = sda_year$Start[2] + sda_year$Amount[2]

  sda_year$Start[3] = sda_year$End[2]
  sda_year$End[3] = sda_year$Start[3] + sda_year$Amount[3]


    sda_results = rbind(sda_results,
                        sda_year)

    if(verbose) print(year)

  }

  return(sda_results)

}

ggplot(sda_results %>% mutate(id = seq(2010,2020+2/3,1/3), Contributions = gsub("\\."," ",Contributions)), aes(time_period, fill = Contributions)) + geom_rect(aes(
                                                             xmin = id, xmax = id + 1/3, ymin = End,
                                                             ymax = Start)) +
  theme_tufte() + scale_x_continuous(n.breaks = 11) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),  # Set overall text size
    axis.title = element_text(size = 14),  # Set axis title size
    axis.text = element_text(size = 14),  # Set axis title size
    legend.text = element_text(size = 14),  # Set legend text size
    legend.title = element_text(size = 14),  # Set legend title size
    strip.text = element_text(size = 14)    # Set facet strip text size
  ) +
  scale_y_continuous(name = "MT CO2e",labels = function(x) x / 1000) +
  xlab(NULL) +
  theme(panel.grid.major.x = element_line(linewidth = 0.01,colour = 'grey'),
        axis.ticks = element_blank())


