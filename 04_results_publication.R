#Results paper:

# library(tidyverse)
# library(janitor)
# library(curl)
# library(matlib)
# library(corpcor)
# library(ggthemes)
# library(gganimate)
# library(shiny)
# library(viridis)
# library(patchwork)
# library(hrbrthemes)
# #library(circlize)
# #library(chorddiag)
# #library(webshot)
# #library(htmlwidgets)
# library(scales)
# library(xtable)



x = c('arrow','ggthemes','xtable','countrycode')

lapply(x,library,character.only=T)

options(scipen = 100, digits = 4)

emissions <- read_parquet("C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23/footprint_results_24_data.parquet") |>
  separate(resource_id,into = c('country',"industry"),extra = 'merge',sep = "_")

df <- emissions |>
  select(direct_emissions, matches("embodied_emissions"), industry, country, time_period, scope_2, production_footprint, X_total = total_output) |>
  group_by(time_period) |>
  mutate(absolute_emissions  = sum(direct_emissions)) |>
  ungroup()



fig_frame <- df |>
  mutate(embodied_emissions = rowSums(df |> select(starts_with("embodied_emissions")), na.rm = TRUE)) |>
  group_by(time_period) |>
  summarise(embodied_total  = sum(embodied_emissions, na.rm = TRUE),
            embodied_final = sum(ifelse(grepl("(26|61|62|63)", industry), embodied_emissions, 0), na.rm = TRUE),
            absolute_emissions  = mean(absolute_emissions, na.rm = TRUE)) |>
  mutate(rel_embodied_total = embodied_total / absolute_emissions) |>
  mutate(rel_embodied_final = embodied_final / absolute_emissions) |>
  ungroup() |> drop_na()


#TABLE ON DIGITAL DEMAND FOOTPRINT DISTRIBUTIONS

non_oecd = countrycode(c("Argentine","Bulgarie","Brésil","Chine","Chypre","Croatie","Indonésie","Inde","Malte","Roumanie","Russie","Arabie Saoudite","Afrique du Sud"),
                       'country.name.fr',
                       'iso2c')

additional_analysis_fd =
  df %>%
  select(time_period,industry,country_prod = country,matches("_P3|_P5")) %>%
  pivot_longer(-c(1:3),names_to = 'fd') %>%
  mutate(fd1 = case_when(grepl('S13',fd) ~ "General government",
                         grepl('S14',fd) ~ "Households",
                         grepl('S15',fd) ~ "NPISH",
                         grepl('P51G',fd) ~ "Gross Fixed Capital Formation",
                         grepl('P5M',fd) ~ "Change in Inventories and Valuables"
                         ))  %>%
  mutate(fd3 = substring(fd,20),
         country = case_when(grepl('FIGW1',fd3) ~ 'FIGW1',
                             T ~ substr(fd3,1,2))) %>%
  mutate(country_zone = case_when(country %in% codelist$iso2c[!is.na(codelist$eu28)] ~ "EU",
                                  country %in% c("US","JP","CN","IN","KR",'RU') ~ country,
                                  T ~ 'ROW'),
         country_prod_zone = case_when(country_prod %in% codelist$iso2c[!is.na(codelist$eu28)] ~ "EU",
                                       country_prod %in% c("US","JP","CN","IN","KR",'RU') ~ country_prod,
                                       T ~ 'ROW')) %>%
  mutate(country_label = case_when(country_zone == 'EU' ~ 'European Union (28)',
                                   country %in% c("US","CN") ~ countrycode(country,'iso2c','country.name'),
                                   country %in% non_oecd  ~ "Rest of the world (Non-OECD)",
                                   T ~ 'Rest of the world (OECD)'),
         country_prod_label = case_when(country_prod_zone == 'EU' ~ 'European Union (28)',
                                        country_prod %in% c("US","CN") ~ countrycode(country_prod,'iso2c','country.name'),
                                        country_prod %in% non_oecd  ~ "Rest of the world (Non-OECD)",
                                        T ~ 'Rest of the world (OECD)'))

fd_by_type =
  additional_analysis_fd %>%
  group_by(fd1,time_period) %>%
  summarise(value = sum(value,na.rm = T)) %>%
  group_by(time_period) %>%
  mutate(share = value / sum(value,na.rm = T))
#general government = +0.5 pts 2010 to 2021

ggplot(data = fd_by_type,aes(x = time_period,y = value,color = fd1,group = fd1)) + geom_line()

fd_by_country =
  additional_analysis_fd  %>%
  group_by(country_zone,time_period) %>%
  summarise(value = sum(value,na.rm = T))  %>%
  group_by(time_period) %>%
  mutate(share = value / sum(value,na.rm = T)) %>%
  group_by(country_zone) %>%
  mutate(evol = value[time_period == 2021] / value[time_period == 2010],
         invevol = (value[time_period == 2021] - value[time_period == 2010]) / value[time_period == 2010])

ggplot(data = fd_by_country %>% filter(share > .05),aes(x = time_period,y = share,color = country_zone,group = country_zone)) + geom_line()



figure_time_self_located_ict_emissions =
  additional_analysis_fd %>%
  group_by(country_label,country_prod_label,time_period)  %>%
  summarise(value = sum(value)) %>%
  group_by(time_period,country_label) %>%
  mutate(share = round(100 * value / sum(value),2)) %>%
  ungroup() %>%
  filter(country_label == country_prod_label)

table_figure_self_located = figure_time_self_located_ict_emissions %>%
  filter(time_period %in% c(min(time_period),max(time_period))) %>%
  mutate(col_name = paste0("Share in ",time_period)) %>%
  select(Country = country_label,col_name,share) %>%
  pivot_wider(names_from = col_name,values_from = share) %>%
  arrange(-`Share in 2021`)

xtable(table_figure_self_located)


prep_table=additional_analysis_fd %>%
  group_by(country_label,fd1,industry,time_period) %>%
  filter(time_period %in% c(2021)) %>%
  summarise(value = sum(value)) %>%
  group_by(time_period,country_label) %>%
  mutate(share = value / sum(value)) %>%
  mutate(top5 = sort(share,partial=n()-4)[n()-4]) %>%
  filter(share >= top5) %>%
  arrange(-share) %>%
  select(country_label,fd1,industry,time_period,share) %>%
  mutate(n = 1:n(),
         lib = paste0(country_label," - ",n)) %>%
   ungroup() %>%
  mutate(industry = case_when(industry == 'C26' ~ 'Hardware',
                              industry == 'J61' ~ 'Communications',
                              industry == 'J62_63' ~ 'IT services',
                              industry == 'O84' ~ 'Public administration',
                              industry == 'F' ~ 'Construction',
                              industry == 'C28' ~ 'Machinery and equipment'),
         fd1 = case_when(fd1 == "Gross Fixed Capital Formation" ~ 'Investment',
                         fd1 == "General government" ~ "Public consumption",
                         fd1 == "Households" ~ "Households consumption"),
         share = round(share * 100,2))

prep_table_aggreg=additional_analysis_fd %>%
  group_by(country_label,fd1) %>%
  filter(time_period %in% c(2021)) %>%
  summarise(value = sum(value)) %>%
  group_by(country_label) %>%
  mutate(share = round(value / sum(value) * 100,2)) %>%
  filter(fd1 %in% c("Gross Fixed Capital Formation","Households")) %>%
  arrange(-share) %>%
  select(Country = country_label,fd1,share) %>%
  ungroup() %>%
  pivot_wider(names_from = fd1,values_from = share)

disaggreg_table =
  data.frame(`Country or Area` = unique(prep_table$country_label),check.names = F) %>%
  mutate(prep_table[match(paste0(`Country or Area`," - ",1),prep_table$lib),c("share","industry","fd1")]) %>%
  rename(`Share 1` = share,`Industry 1` = industry,`Final demand category 1` = fd1) %>%
  merge(prep_table_aggreg,by.x = 'Country or Area',by.y = 'Country') %>%
  # mutate(prep_table[match(paste0(`Country or Area`," - ",2),prep_table$lib),c("share","industry","fd1")]) %>%
  # rename(`Share 2` = share,`Industry 2` = industry,`Final demand category 2` = fd1) %>%
  # mutate(prep_table[match(paste0(`Country or Area`," - ",3),prep_table$lib),c("share","industry","fd1")]) %>%
  # rename(`Share 3` = share,`Industry 3` = industry,`Final demand category 3` = fd1) %>%
  mutate(`Country or Area` = case_when(`Country or Area` == "Rest of the world (Non-OECD)" ~ "ROW (Non-OECD)",
                                       `Country or Area` == "Rest of the world (OECD)" ~ "ROW (OECD)",
                                       `Country or Area` == "European Union (28)" ~ "EU 28",
                                       T ~ `Country or Area`)) %>%
  {.[c(1,2,5,4,3),]} %>%
  xtable()

#1st share 2nd share 3rd share 4th share 5th share || global without industry
#
# View(additional_analysis_fd %>%
#        group_by(country_zone,time_period) %>%
#        summarise(value = sum(value)) %>%
#        group_by(time_period) %>%
#        mutate(share = value / sum(value)))
#
# View(additional_analysis_fd %>%
#        group_by(fd1,time_period) %>%
#        summarise(value = sum(value)) %>%
#        group_by(time_period,country_zone) %>%
#        mutate(share = value / sum(value)))
#
# View(additional_analysis_fd %>%
#        group_by(country_zone,fd1,country_prod_zone,industry,time_period) %>%
#        summarise(value = sum(value)) %>%
#        group_by(time_period,country_zone) %>%
#        mutate(share = value / sum(value)))
#
# View(additional_analysis_fd %>%
#        mutate(industry = case_when(
#          industry == "C26" ~ "hardware",
#          industry == "J61" ~ "communications",
#          industry == "J62_63" ~ "IT services",
#          TRUE ~ "mediated via other industries")) %>%
#        group_by(industry,time_period) %>%
#        summarise(value = sum(value)) %>%
#        group_by(time_period) %>%
#        mutate(share = value / sum(value)))
#
# View(df %>%
#        select(direct_emissions,industry,time_period) %>%
#        group_by(time_period,industry) %>%
#        summarise(value = sum(direct_emissions,na.rm=T)))

#Understanding Electricity and gas supply constitute the largest share of direct emissions (40.9\%), followed by basic metals (11.2\%), and mining and quarrying (9.8\%)

# Link A, L and indirect_fpt


#Analyzing sectoral scope 2 evolutions. Maybe the average carbonation of the D35 used by ICT
#Analyzing sector scope 3 evolutions and structure (decomposing)

# Results Figure 1:

fd_emissions_over_time <- ggplot(fig_frame, aes(x = as.integer(time_period))) +

  geom_line(aes(y = embodied_total / 1000, linetype = "total demand", color = "absolute")) +
  geom_text(aes(y = embodied_total / 1000, label = round(embodied_total / 1000)),
            vjust = -0.5, color = "#0072B2", size = 4.5, family = "serif") +

  geom_line(aes(y = rel_embodied_total * 100 * 100, linetype = "total demand", color = "relative")) +
  geom_text(aes(y = rel_embodied_total * 100 * 100, label = round(rel_embodied_total * 100, 2)),
            vjust = -0.5, color = "#D55E00", size = 4.5 , family = "serif") +

  # geom_line(aes(y = embodied_final / 1000, linetype = "final demand", color = "absolute")) +
  # geom_text(aes(y = embodied_final / 1000, label = round(embodied_final / 1000)),
  #           vjust = -0.5, color = "#0072B2", size = 4.5, family = "serif") +
  #
  # geom_line(aes(y = rel_embodied_final * 100 * 100, linetype = "final demand", color = "relative")) +
  # geom_text(aes(y = rel_embodied_final * 100 * 100, label = round(rel_embodied_final * 100, 2)),
  #           vjust = -0.5, color = "#D55E00", size = 4.5, family = "serif") +
  #
  scale_linetype_manual(values = c("final demand" = "dashed", "total demand" = "solid"),
                        name = "") +

  scale_color_manual(values = c("absolute" = "#0072B2", "relative" = "#D55E00"),
                     name = "") +

  scale_y_continuous(
    name = "CO2e in mt",
    sec.axis = sec_axis(~ ./100, name = "relative CO2e in %"),
    limits = c(0, 2200)
  ) +

  labs(x = "year",
       y = "CO2e in mt") +

  theme_tufte() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),  # Set overall text size
     axis.title = element_text(size = 14),  # Set axis title size
     axis.text = element_text(size = 14),  # Set axis title size
     legend.text = element_text(size = 14),  # Set legend text size
     legend.title = element_text(size = 14),  # Set legend title size
     strip.text = element_text(size = 14)    # Set facet strip text size
  #
  )

fd_emissions_over_time

ggsave("./results/figures/emissions_over_time.pdf",
       plot = fd_emissions_over_time, width = 8, height = 6, dpi = 300)

fig_frame_industry <- df |>
  mutate(embodied_emissions = rowSums(df |> select(starts_with("embodied_emissions")), na.rm = TRUE)) |>
  mutate(industry = case_when(
    industry == "C26" ~ "hardware",
    industry == "J61" ~ "communications",
    industry == "J62_63" ~ "IT services",
    TRUE ~ "mediated via other industries")) |>
   group_by(time_period, industry) |>
    summarise(embodied_emissions  = sum(embodied_emissions, na.rm = TRUE)) |>
  ungroup() |>
  drop_na()

industry_order <- c("mediated via other industries" , "hardware",  "communications" ,
                    "IT services")

emissions_over_time_by_industry <- fig_frame_industry |>
  mutate(industry = factor(industry, levels = industry_order)) |>
  # mutate(across(c(direct, indirect), ~ ./absolute_emissions)) |>
  ggplot(aes(x = as.integer(time_period), y = embodied_emissions/1000, group = industry, fill = industry)) +
  geom_area(position = "stack") +
  labs(x = "year", y = "CO2e in mt", fill = "") +
  scale_fill_manual(values = c(
    "mediated via other industries" = "grey",
    "hardware" = "#68011f",
    "communications" = "#f2a27d",
    "IT services" =  "#2367ae")) +
  # "hardware" = "#56B4E9",
  # "communications" = "#009E73",
  # "IT services" = "#F0E442")) +
  theme_tufte() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),  # Set overall text size
        axis.title = element_text(size = 14),  # Set axis title size
        axis.text = element_text(size = 14),  # Set axis title size
        legend.text = element_text(size = 14),  # Set legend text size
        legend.title = element_text(size = 14),  # Set legend title size
        strip.text = element_text(size = 14)    # Set facet strip text size
        # Increase facet strip text size if you use facets
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE), colour = guide_legend(nrow = 1))

emissions_over_time_by_industry

ggsave("./results/figures/emissions_over_time_by_industry.pdf",
       plot = emissions_over_time_by_industry, width = 8, height = 6, dpi = 300)


fig_frame_industry |>
  pivot_wider(names_from="industry",
              values_from="embodied_emissions") |>
  rename(year = time_period) |>
  xtable()

  #industry_order <- c( "hardware",  "communications", "IT services")

# % scope 1, 2, 3 emissions
# scopes <- df |>
#   select(time_period, industry, country, direct_emissions, scope_2, matches("embodied_emissions")) |>
#   rename(scope_1 = direct_emissions ) |>
#   mutate(embodied_emissions = rowSums(df |> select(starts_with("embodied_emissions")), na.rm = TRUE)) |>
#   group_by(time_period) |>
#     mutate(scope_3  = sum(embodied_emissions, na.rm = TRUE)) |>
#   ungroup() |>
#   filter(grepl("(26|61|62|63)", industry)) |>
#   select(time_period,  matches("scope")) |>
#    group_by(time_period) |>
#     mutate(scope_1  = sum(scope_1, na.rm = TRUE)) |>
#     mutate(scope_2  = sum(scope_2, na.rm = TRUE)) |>
#   ungroup() |>
#   distinct() |>
#   pivot_longer(c(scope_1, scope_2, scope_3), names_to= "scope", names_prefix = "scope_",values_to= "value") |>
#   # mutate(scope = case_when(scope == 1 ~ "scope 1",
#   #                          scope == 2 ~ "scope 2",
#   #                          scope == 3 ~ "embodied emissions",
#   #                          TRUE ~ scope))
#   mutate(scope = scope |> factor(
#     levels=c(1,2, 3),
#     labels=c("scope 1", "scope 2",  "embodied emissions")
#     ))
#
#
#
# # Plot
#  scopes_over_time<- ggplot() +
#    # Darker grey outline and transparent bars for 2021
#    geom_bar(data = scopes %>% filter(time_period == 2021),
#             aes(x = scope, y = value/1000),
#             stat = "identity", color = "darkgrey", fill = "transparent") +
#    # Add rotated squares for 2010
#    geom_point(data = scopes %>% filter(time_period == "2010"),
#               aes(x = scope, y = value/1000),
#               shape = 16, color = "red", size = 2) +
#    # Dummy points to create the custom legend
#    geom_point(aes(x = Inf, y = Inf, shape = "2010", color = "2010"), size = 2) +
#    geom_point(aes(x = Inf, y = Inf, shape = "2021", color = "2021"), size = 3) +
#    # Custom legend entries
#    scale_shape_manual(name = "year", values = c("2010" = 16, "2021" = 22)) +
#    scale_color_manual(name = "year", values = c("2010" = "red", "2021" = "darkgrey")) +
#    # Labels and theme
#    labs(
#      x = "",
#      y = "CO2e in mt") +
#    theme_tufte() +
#    theme(
#          legend.position = "right")  # Place legend on the right
#
#  scopes_over_time
#
#
#  ggsave("./results/figures/scopes_over_time.pdf",
#         plot = scopes_over_time, width = 8, height = 6, dpi = 300)

scopes_industry <- df |>
   select(time_period, industry, country, direct_emissions, scope_2, production_footprint) |>
   rename(scope_1 = direct_emissions ) |>
   rename(scope_3 = production_footprint) |>
   filter(grepl("(26|61|62|63)", industry)) |>
   select(time_period, industry, matches("scope")) |>
   group_by(time_period, industry) |>
   mutate(scope_1  = sum(scope_1, na.rm = TRUE)) |>
   mutate(scope_2  = sum(scope_2, na.rm = TRUE)) |>
   mutate(scope_3  = sum(scope_3, na.rm = TRUE)) |>
   ungroup() |>
   mutate(scope_3  = scope_3 - scope_1 - scope_2) |>
   mutate(industry = case_when(
     industry == "C26" ~ "hardware",
     industry == "J61" ~ "communications",
     industry == "J62_63" ~ "IT services",
     TRUE ~ industry)) |>
   distinct() |>
   pivot_longer(c(scope_1, scope_2, scope_3), names_to= "scope", names_prefix = "scope_",values_to= "value") |>
   mutate(scope = scope |> factor(
     levels=c(1,2, 3),
     labels=c("Scope 1", "Scope 2",  "embodied emissions")
   )) |>
   mutate(industry = factor(industry, levels = industry_order))

scopes_by_industry <- ggplot() +
  # Darker grey outline and transparent bars for 2021
  geom_bar(data = scopes_industry %>% filter(time_period == 2021),
           aes(x = scope, y = value / 1000, color = "2021", fill = industry),
           stat = "identity", alpha = 0.5) +
  # Add points for 2010
  geom_point(data = scopes_industry %>% filter(time_period == "2010"),
             aes(x = scope, y = value / 1000, color = "2010", shape = "2010"),
             size = 4) +
  # Define shape and color scales for legend
  scale_color_manual(name = "year", values = c("2010" = "red", "2021" = "darkgrey")) +
  scale_fill_manual(
    name = "industry",
    values = c(
      "hardware" = "#68011f",
      "communications" = "#f2a27d",
      "IT services" = "#2367ae"
    )
  ) +
  # Facet by industry
  facet_wrap(~industry) +
  # Labels and theme
  labs(
    x = "",
    y = "CO2e in mt (in log-scales)"
  ) +
  theme_tufte() +
  theme(
    legend.position = "right"
  ) +
  guides(shape = "none", fill ="none") +
  scale_y_log10() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),  # Set overall text size
        axis.title = element_text(size = 20),  # Set axis title size
        axis.text = element_text(size = 20),  # Set axis title size
        legend.text = element_text(size = 20),  # Set legend text size
        legend.title = element_text(size = 20),  # Set legend title size
        strip.text = element_text(size = 20)    # Set facet strip text size
        # Increase facet strip text size if you use facets
  )
scopes_by_industry





ggsave("./results/figures/scopes_by_industry.pdf",
       # "./results/figures/scopes_by_industry_log.pdf",
        plot = scopes_by_industry, width = 18, height = 6, dpi = 300)


scopes_industry |>
  filter(time_period ==2010 |time_period ==2021) |>
  pivot_wider(names_from= "time_period", values_from = "value") |>
  mutate("change  2010 and 2021 in %" = (`2021`- `2010`)/`2010` * 100) |>
  xtable()



# scopes |>
#    ggplot(aes(x = as.integer(time_period), y = value/1000, color= scope)) +
#   geom_line(size = 0.8) +
#   labs(x = "year", y = "CO2e in mt", fill = "Scope:") +
#   geom_text(aes(label = round(value / 1000)),
#            size = 3,
#            vjust = -0.5,  # Adjust text position above the line
#            show.legend = FALSE) +
#   scale_color_brewer(type = "qual", palette = 2)  +  # Use Viridis palette (discrete version)
#   theme_tufte() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 12),  # Increase overall text size
#         axis.title = element_text(size = 12),  # Increase axis titles size
#         legend.text = element_text(size = 12),  # Increase legend text size
#         legend.title = element_text(size = 12),  # Increase legend title size
#         strip.text = element_text(size = 12)  # Increase facet strip text size if you use facets
#   ) +
#   guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))

#method:

# improve sankey diagram by "digital components of other industries" (middle) and where the final demand stems from final consumption (private, government, investments), to which industry components go to (industry, services, etc.),
#
# industry_order <- c( "hardware",  "communications", "IT services")
#
# prep_df <- emissions |>
#   select(industry, country,time_period, emissions_per_output, matches( "(26|61|62|63)")) |>
#   pivot_longer(
#     cols=matches("//d+_"), names_to="industry_country",
#     values_to="leon_wght"
#   ) |>
#   rename(ref_country = country)|>
#   separate_wider_regex(
#     industry_country, c(ICT = ".*", "_", country = ".*"))|>
#   inner_join(emissions |>
#                filter(grepl("(26|61|62|63)", industry)) |>
#                rename(ICT = industry) |>
#                select(ICT, country, Y_total, embodied_emissions, time_period) ,
#              by =  c("ICT", "country", "time_period")) |>
#   rename(ICT_country = country) |>
#   mutate(ICT = case_when(
#     ICT == "C26" ~ "hardware",
#     ICT == "J61" ~ "communications",
#     ICT == "J62_63" ~ "IT services",
#     TRUE ~ ICT
#   )) |>
#   mutate(demand_weight = leon_wght*Y_total)
#
#
# df <-prep_df|>
#   group_by(time_period, ICT, ICT_country) |>
#   mutate(county_weight = sum(leon_wght,na.rm = TRUE)) |>
#   ungroup() |>
#   group_by(time_period, ICT) |>
#   summarise(
#     weight = weighted.mean(x=county_weight, w=Y_total),
#     embodied_emissions = weighted.mean(x=embodied_emissions),
#     Y = sum(Y_total),
#     CO2e_intensity = weighted.mean(x=emissions_per_output, w=demand_weight, na.rm = TRUE),
#   ) |>
#   ungroup()
#
# figure_df <- df |>
#   pivot_longer(
#     cols=c("embodied_emissions", "weight", "Y", "CO2e_intensity"), names_to="decomposition",
#     values_to="value"
#   ) |>
#   mutate(ICT = factor(ICT, levels = industry_order)) |>
#   group_by(ICT, decomposition) |>
#   mutate(baseline_2010 = value[time_period == "2010"],  # Get baseline value for 2010
#          value_normalized = (value / baseline_2010 -1) * 100) |>  # Normalize values to 2010 baseline
#   ungroup()
#
# facet_labels <- c("embodied_emissions" = "a) embodied emissions",
#                   "CO2e_intensity" = "b) emission intensity",
#                   "weight" = "c) sum of Leontief weights",
#                   "Y" = "d) final demand")
#
#
# decomposition_order <- c( "embodied_emissions",  "CO2e_intensity","weight", "Y")
#
#
# decomposition_changes <- figure_df |>
#   mutate(decomposition = factor(decomposition, levels = decomposition_order)) |>
#   ggplot(aes(x = as.integer(time_period), y = value_normalized, color = ICT)) +
#   geom_line() +
#   facet_wrap(~decomposition, labeller = as_labeller(facet_labels), ncol = 2) +
#   theme_tufte() +
#   theme(
#     legend.position = "bottom",
#     text = element_text(size = 14),  # Set overall text to serif and size 12
#     axis.title = element_text(size = 14),  # Set axis titles to size 12
#     legend.text = element_text(size = 14),  # Set legend text to size 12
#     legend.title = element_text(size = 14),  # Set legend title to size 12
#     strip.text = element_text(size = 14))+    # Set facet strip text to size 1) +
#   scale_color_manual(values = c(
#     "hardware" = "#68011f",
#     "communications" = "#f2a27d",
#     "IT services" = "#2367ae" )) +
#   labs(x = "year", y = "change in % (basline 2010)", color = "ICT industry:")
# # scale_y_continuous(limits= c(-50, -250))
#
# ggsave("./results/figures/decomposition_changes.pdf",
#        plot = decomposition_changes, width = 8, height = 6, dpi = 300)
#





# ranking <- sankey_df |>
#   arrange(desc(weight)) |>
#   mutate(ref_industry = factor(ref_industry,
#                                levels = c(setdiff(unique(ref_industry), "other industries"),
#                                           "other industries"))) |>
#   arrange(ref_industry)
#
# sankey <- sankey_df |>  bind_rows(demand)
#
#
#
# unique_nodes <- c(as.character(ranking$ref_industry), sankey |> arrange(desc(weight)) |> pull(counter_industry)) |>  unique()
# # Create the nodes DataFrame with clear and informative column names
# nodes <- unique_nodes |>
#   as_tibble() |>
#   mutate(id = row_number() -1) |>
#   rename(name = value) |> as.data.frame()
#
# total <- sankey_df |> select(weight) |> pull() |>  sum()
#
# # Create links dataframe
# links <- sankey |>
#   left_join(nodes, by = c("ref_industry" = "name")) |>
#   rename(source = id) |>
#   left_join(nodes, by = c("counter_industry" = "name")) |>
#   mutate(target = id) |>
#   select(-id) |>
#   #distinct(source, weight, .keep_all = TRUE) |>
#   #distinct(target, weight, .keep_all = TRUE) |>
#   select(source, weight,target) |> as.data.frame() |>
#   arrange(source) |>
#   mutate(weight = round(weight/ total *100, 2))
#
# # Corrected color scale definition
# # Corrected color scale definition
# # Define distinct node groups
#
# # Step 5: Create the Sankey diagram
# sankey_fig <- sankeyNetwork(Links = links, Nodes = nodes, Source = 'source',
#                             Target = 'target', Value = 'weight', NodeID = 'name',
#                             nodeWidth = 30,  fontSize = 14,
#                             NodeGroup = "name", iterations = 0,
#                             width = 1200,   height = 600)
#
# # Step 6: Add rounded values to nodes
# sankey_txt <- onRender(
#   sankey_fig,
#   '
#   function(el, x) {
#     d3.select(el).selectAll(".node text")
#       .text(function(d) { return d.name + " (" + d.value.toFixed(0) + " %)"; });
#   }
#   '
# )
#
# # Display the Sankey diagram
# sankey_txt
#
#
# htmlwidgets::saveWidget(sankey_txt, "./results/figures/sankey_inputs.html")
#
# webshot("./results/figures/sankey_inputs.html", "./results/figures/sankey_inputs_total.png")
#
#
#
# sankey_country <- transfer_of_emissions |>  as.tibble() |>
#   mutate(industry_ref_area = rel_industries) |>
#   pivot_longer(-industry_ref_area,
#                names_to = "industry_counterpart_area",
#                values_to = "weight") |>
#   separate_wider_regex(industry_ref_area,
#                        c(ref_industry = ".*", "_", ref_area = ".*")) |>
#   separate_wider_regex(industry_counterpart_area,
#                        c(counter_industry = ".*", "_", counter_area = ".*")) |>
#   group_by(ref_industry, counter_industry ) |>
#   summarise(
#     weight = sum(weight, na.rm = TRUE )
#   )


# generate three staged flow chart, in which country products are combined

# improve decomposition figure by digital components.

# share of Chinese exports over time.

