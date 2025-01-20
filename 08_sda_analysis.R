x = c('dplyr','tidyr','tibble','curl','stringr','ggplot2','eurostat','xml2','rvest','data.table','arrow','ggthemes','ggbreak')

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

    Y0 = sum(D0,na.rm=T) #Y the quantity effect of demand

    u0 = D0 / Y0 #u the basket i.e. the preferences effect of demand

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

    Y1 = sum(D1,na.rm=T)

    u1 = D1 / Y1

    #t(b1) %*% L1 %*% u1 %*% Y1

    #t(b0) %*% L0 %*% D0 #should equal sum(e0,na.rm = T)

    #Non unique solution, we implement a polar decomposition method, see Dietzenbacher and Los (1998)
    #Dietzenbacher, E., & Los, B. (1998). Structural decomposition techniques: sense and sensitivity. Economic Systems Research, 10(4), 307-324.

    dif_B = (1/2) * t(b1 - b0) %*% L1 %*% u1 %*% Y1 + (1/2) * t(b1 - b0) %*% L0 %*% u0 %*% Y0

    dif_L = (1/2) * t(b0) %*% (L1 - L0) %*% u1 %*% Y1 + (1/2) * t(b1) %*% (L1 - L0) %*% u0 %*% Y0

    dif_u = (1/2) * t(b0) %*% L0 %*% (u1 - u0) %*% Y1 + (1/2) * t(b1) %*% L1 %*% (u1 - u0) %*% Y0

    dif_Y = (1/2) * t(b0) %*% L0 %*% u0 %*% (Y1 - Y0) + (1/2) * t(b1) %*% L1 %*% u1 %*% (Y1 - Y0)


    if((round(dif_B + dif_L + dif_u + dif_Y) != round(sum(e1,na.rm=T) - sum(e0,na.rm=T))) & verbose) print(paste0('Error detected :',year)) #IT WORKS

    sda_year = data.frame(total_prod = sum(e0,na.rm=T),
                          direct_em = as.numeric(dif_B),
                          eco_tech = as.numeric(dif_L),
                          fd_vol = as.numeric(dif_Y),
                          fd_str = as.numeric(dif_u),
                          time_period = year,
                          Start = NA,
                          End = NA,
                          amount_share = NA,
                          year_changes = NA) %>%
      pivot_longer(1:5,names_to = "Contributions",values_to = "Amount",names_repair = 'minimal') %>%
      mutate(Contributions = case_when(Contributions == "total_prod" ~ "",
                                       Contributions == "direct_em" ~ "Direct emissions intensities",
                                       Contributions == "eco_tech" ~ "Technology changes",
                                       Contributions == "fd_vol" ~ "Final demand : volume",
                                       Contributions == "fd_str" ~ "Final demand : basket"))


  sda_year$Start[1] = 0
  sda_year$End[1] = sda_year$Start[1] + sda_year$Amount[1]

  sda_year$Start[2] = sda_year$End[1]
  sda_year$End[2] = sda_year$Start[2] + sda_year$Amount[2]

  sda_year$Start[3] = sda_year$End[2]
  sda_year$End[3] = sda_year$Start[3] + sda_year$Amount[3]

  sda_year$Start[4] = sda_year$End[3]
  sda_year$End[4] = sda_year$Start[4] + sda_year$Amount[4]

  sda_year$Start[5] = sda_year$End[4]
  sda_year$End[5] = sda_year$Start[5] + sda_year$Amount[5]

  sda_year$amount_share = round(sda_year$Amount / sda_year$End[1] * 100,1)

  sda_year$year_changes = round(sda_year$End[5] - sda_year$End[1])


    sda_results = rbind(sda_results,
                        sda_year)

    if(verbose) print(year)

  }

  return(sda_results)

}

#add last_year total

sda_results =
  sda_results %>% rbind(
  data.frame(
    time_period = as.numeric(max(results$time_period))+1,
    Start = 0,
    Amount = results %>%
      filter(time_period == max(time_period)) %>%
      column_to_rownames('resource_id') %>%
      select(production_footprint) %>%
      sum(na.rm=T),
    Contributions = "",
    amount_share = 100,
    year_changes = 0) %>%
    mutate(End = Start + Amount))


sda_results = sda_results %>%
  mutate(id = seq(2010,2021,1/5),
         amount_share = case_when(amount_share == 100 ~ "",
                                  amount_share < 0 ~ paste0("- ",abs(amount_share)," %"),
                                  T ~ paste0("+ ",amount_share," %")),
         year_changes = case_when(year_changes < 0 ~ paste0("- ",round(abs(year_changes / 1000))),
                                  T ~ paste0("+ ",round(abs(year_changes / 1000)))))


# ggplot(sda_results, aes(time_period, fill = Contributions)) + geom_rect(aes(
#                                                              xmin = id, xmax = id + 1/5, ymin = End,
#                                                              ymax = Start)) +
#   scale_fill_manual(breaks = c("Direct emissions intensities","Technology changes","Final demand : volume","Final demand : basket"),
#                     values = c("#CC9900","#9900CC","#FF0000","#0066CC",'#333333'))+
#   #scale_y_break(c(100,1400),scales = 2) +
#   theme_tufte() +
#   scale_x_continuous(n.breaks = 11) +
#   theme(
#     #legend.position = "bottom",
#     text = element_text(size = 14),  # Set overall text size
#     axis.title = element_text(size = 18),  # Set axis title size
#     axis.text = element_text(size = 16),  # Set axis title size
#     legend.text = element_text(size = 16),  # Set legend text size
#     legend.title = element_text(size = 18),  # Set legend title size
#     strip.text = element_text(size = 16),   # Set facet strip text size
#     # legend.justification = "center",
#     # legend.direction = "horizontal"
#     ) +
#   scale_y_continuous(name = "MT CO2e",labels = function(x) x / 1000) +
#   xlab(NULL) +
#   theme(panel.grid.major.x = element_line(linewidth = 0.01,colour = 'grey'),
#         axis.ticks = element_blank()) +
#   coord_cartesian(ylim = c(limits = c(1400000,2300000)),
#                   xlim = c(limits = c(2010,max(sda_results$time_period)-1)),
#                   clip = 'on') +
#   geom_text(aes(label=amount_share,y = pmax(Start,End)+30000,x = id+1/12,angle = 90),size = 4) +
#   geom_segment(data = sda_results %>% filter(time_period != max(time_period)),aes(x = time_period - 4/5, y = 1400000, xend = time_period - 0.05, yend = 1400000),
#                arrow = arrow(length = unit(0.2, "cm")),linewidth = 1) +
#   geom_text(data = sda_results %>% filter(time_period != max(time_period)),aes(label = year_changes,y = 1415000,x = time_period - 0.4),size = 6)

#save png width = 1500 height = 1200

ggplot(sda_results, aes(time_period, fill = Contributions)) + geom_rect(aes(
  xmin = id, xmax = id + 1/5, ymin = End,
  ymax = Start)) +
  scale_fill_manual(breaks = c("Direct emissions intensities","Technology changes","Final demand : volume","Final demand : basket"),
                    values = c("#CC9900","#9900CC","#FF0000","#0066CC",'#333333'))+
  #scale_y_break(c(100,1400),scales = 2) +
  theme_tufte() +
  scale_x_continuous(n.breaks = 11) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 10),  # Set overall text size
    axis.title = element_text(size = 14),  # Set axis title size
    axis.text = element_text(size = 12),  # Set axis title size
    legend.text = element_text(size = 9),  # Set legend text size
    legend.title = element_text(size = 11),  # Set legend title size
    strip.text = element_text(size = 12),   # Set facet strip text size
    # legend.justification = "center",
    # legend.direction = "horizontal"
  ) +
  scale_y_continuous(name = "MT CO2e",labels = function(x) x / 1000) +
  xlab(NULL) +
  theme(panel.grid.major.x = element_line(linewidth = 0.01,colour = 'grey'),
        axis.ticks = element_blank()) +
  coord_cartesian(ylim = c(limits = c(1400000,2300000)),
                  xlim = c(limits = c(2010,max(sda_results$time_period)-1)),
                  clip = 'on') +
  geom_text(aes(label=amount_share,y = pmax(Start,End)+60000,x = id+1/12,angle = 90),size = 3) +
  geom_segment(data = sda_results %>% filter(time_period != max(time_period)),aes(x = time_period - 4/5, y = 1400000, xend = time_period - 0.05, yend = 1400000),
               arrow = arrow(length = unit(0.2, "cm")),linewidth = .8) +
  geom_text(data = sda_results %>% filter(time_period != max(time_period)),aes(label = year_changes,y = 1420000,x = time_period - 0.4),size = 3.5)

#save pdf A5 landscape

