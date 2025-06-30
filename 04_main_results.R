x = c('arrow','ggthemes','xtable')
lapply(x,library,character.only=T)

options(scipen = 100, digits = 4)

#Figure 1 - Share of output stored in prd - Get deflated output#

values_agg = format_iot(folder = if(user =="jax"){paste0(main_path, "/data/values")}else{main_path},
                        exdir = if(user =="jax"){paste0(main_path, "/data/values")}else{main_path},
                        update = F,
                        edition = edition)

price_index = get_value_added_price_index(2021,update = F) |>
  rename(deflator = value,
         ref_area = country,
         time_period = year) |>
  select(-base)

values_agg =
  values_agg %>%
  separate(rowLabels,into = c('ref_area','industry'),sep = "_",remove = F,extra = 'merge') %>%
  full_join(price_index, by = c("ref_area" ,"industry" ,"time_period")) |>
  mutate(across(where(is.numeric), \(.x){.x*deflator} )) |>
  select(-c("ref_area","industry","deflator"))

rm(price_index)

prd = values_agg %>%
  pivot_longer(-c(rowLabels,time_period)) %>%
  filter(!grepl('W2',rowLabels)) %>%
  group_by(rowLabels,time_period) %>%
  summarise(value = sum(value,na.rm=T)) %>%
  group_by(time_period) %>%
  summarise(share_of_output = sum(ifelse(grepl("C26|J61|J62_63",rowLabels),value,0),na.rm=T) / sum(value,na.rm = T))

rm(values_agg)

#Fetch and format results of vizualisation#

emissions <- read_parquet( if(user =="jax"){paste0(main_path, "/data/footprint_results_",edition,"_data.parquet")}else{paste0(main_path, "/footprint_results_",edition,"_data.parquet")}) |>
  separate(resource_id,into = c('country',"industry"),extra = 'merge',sep = "_") |>
  mutate(year = as.integer(time_period)) |>
  filter(year >= as.integer(start_year)) |>
  filter(year <= as.integer(end_year)) |>
  select(-year)

df <- emissions |>
  select(industry, country,
         time_period, direct_emissions, matches("embodied_emissions"), matches("scope"),
         X_total = total_output)

#Figure 1#

fig_frame =
  emissions %>%
  mutate(production_footprint = rowSums(df |> select(starts_with("embodied_emissions")), na.rm = TRUE)) %>%
  select(country,time_period,industry,production_footprint,direct_emissions) %>%
  group_by(time_period) %>%
  summarise(share_footprint = sum(production_footprint,na.rm = T) / sum(direct_emissions,na.rm=T),
            total_footprint = sum(production_footprint,na.rm=T)) %>%
  left_join(prd,by = 'time_period')

fd_emissions_over_time <- ggplot(fig_frame, aes(x = as.integer(time_period))) +

  geom_line(aes(y = total_footprint / 1000, color = "absolute emissions")) +
  geom_point(aes(y = total_footprint / 1000, color = "absolute emissions"), size = 3) +
  geom_text(aes(y = total_footprint / 1000, label = round(total_footprint / 1000)),
            vjust = -0.5, color = "#0072B2", size = 4.5, family = "sans") +

  geom_line(aes(y = share_footprint * 100 * 100, color = "share of emissions")) +
  geom_point(aes(y = share_footprint* 100 * 100, color = "share of emissions"), size = 3) +
  geom_text(aes(y = share_footprint * 100 * 100, label = round(share_footprint * 100, 2)),
            vjust = +1.5, color = "#D55E00", size = 4.5 , family = "sans") +

  geom_line(aes(y = share_of_output * 100 * 100, color = "share of output")) +
  geom_point(aes(y = share_of_output * 100 * 100, color = "share of output"), size = 3) +
  geom_text(aes(y = share_of_output * 100 * 100, label = round(share_of_output * 100, 2)),
            vjust = -1.5, color = "#B03A2E", size = 4.5 , family = "sans") +

  scale_color_manual(values = c("absolute emissions" = "#0072B2", "share of emissions" = "#D55E00","share of output" = "#B03A2E"),
                     name = "") +

  scale_y_continuous(
    name = "CO2e in mt",
    sec.axis = sec_axis(~ ./100, name = "relative CO2e in %"),
    limits = c(0, 3000)
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



ggsave(
       paste0("./results/figures/emissions_over_time_",edition ,"_", start_year, "_", end_year ,".pdf"),
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
  scale_y_continuous(
    limits = c(0, 3000)
  ) +
  theme_tufte() +
  theme(
        legend.position = "bottom",
        text = element_text(size = 14, family = "sans"),  # Set overall text size
        axis.title = element_text(size = 14),  # Set axis title size
        axis.text = element_text(size = 14),  # Set axis title size
        legend.text = element_text(size = 14),  # Set legend text size
        legend.title = element_text(size = 14),  # Set legend title size
        strip.text = element_text(size = 14)    # Set facet strip text size
        # Increase facet strip text size if you use facets
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE), colour = guide_legend(nrow = 1))

emissions_over_time_by_industry

ggsave(paste0("./results/figures/emissions_over_time_by_industry_",edition ,"_", start_year, "_", end_year ,".pdf"),
       plot = emissions_over_time_by_industry, width = 8, height = 6, dpi = 300)

fig_frame_industry |>
  mutate(embodied_emissions = as.integer(round(embodied_emissions/1000))) |>
  pivot_wider(names_from="industry",
              values_from="embodied_emissions") |>
  rename(year = time_period) |>
  xtable()

scopes_industry <- df |>
   select(time_period, matches("scope")) %>%
  distinct() %>% pivot_longer(
    matches("scope"),
    names_to = "scope",
    values_to = "value"
  ) %>%
  mutate(
    Scope = str_extract(scope, "^[^_]+"),
    # Extract industry (everything after the first underscore)
    industry = str_extract(scope, "(?<=_).+")
  ) %>%
  select(-scope) %>%
   mutate(industry = case_when(
     industry == "C26" ~ "hardware",
     industry == "J61" ~ "communications",
     industry == "J62_63" ~ "IT services",
     TRUE ~ industry)) |>
  mutate(scope = case_when(
    Scope == "scope1" ~ 1,
    Scope == "scope2" ~ 2,
    Scope == "scope3" ~ 3,
    TRUE ~ NA)) |>
   mutate(scope = scope |> factor(
     levels=c(1,2, 3),
     labels=c("Scope 1", "Scope 2",  "Scope 3")
   )) |>
  select(-Scope) %>%
   mutate(industry = factor(industry, levels = industry_order))

scopes_industry <- scopes_industry %>%
  mutate(time_period = factor(time_period, levels = c(start_year, end_year)))

scopes_by_industry <- ggplot() +
  # Bar plot for end_year
  geom_bar(data = scopes_industry %>% filter(time_period == end_year),
           aes(x = scope, y = value / 1000, color = time_period, fill = industry),
           stat = "identity", alpha = 0.5) +
  # Add points for start_year
  geom_point(data = scopes_industry %>% filter(time_period == start_year),
             aes(x = scope, y = value / 1000, color = time_period, shape = time_period),
             size = 4) +
  # Define shape and color scales for legend
  scale_color_manual(
    name = "year",
    values = c("2010" = "red", "2021" = "darkgrey") # Ensure levels match
  ) +
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
    y = "CO2e in mt"
  ) +
  theme_tufte() +
  # theme(
  #   legend.position = "right"
  # ) +
  guides(shape = "none", fill ="none") +
  #scale_y_log10() +
  theme(
        legend.position = "bottom",
        text = element_text(size = 20, family = "sans"),  # Set overall text size
        axis.title = element_text(size = 20),  # Set axis title size
        axis.text = element_text(size = 20),  # Set axis title size
        legend.title = element_text(size = 20),  # Set legend title size
        strip.text = element_text(size = 20)    # Set facet strip text size
        # Increase facet strip text size if you use facets
  )
scopes_by_industry


ggsave(paste0("./results/figures/scopes_by_industry_",edition ,"_", start_year, "_", end_year ,".pdf"),,
       # "./results/figures/scopes_by_industry_log.pdf",
        plot = scopes_by_industry, width = 18, height = 6, dpi = 300)


scope_comparision <- scopes_industry |>
  rename(year = time_period) |>
  filter(year == end_year| year == start_year) |>
   pivot_wider(names_from="year",
               values_from="value") |>
  mutate("change between years in %"= (across(4) - across(3)) / across(3)) |>
  group_by(industry) |>
  mutate("2010 (relative)" = across(2)/sum(across(2)),
         "2021 (relative)" = across(3)/sum(across(3)))|>
  ungroup() |>
  mutate(across(c(`2010`, `2021`), ~ as.integer(round(.x / 1000))))

# Table 4:
scope_comparision|>
  xtable()

final_emissions <- fig_frame_industry |>
  mutate(embodied_emissions = as.integer(round(embodied_emissions/1000))) |>
  rename(year = time_period) |>
  filter(year == end_year| year == start_year) |>
  # pivot_wider(names_from="year",
  #             values_from="embodied_emissions") |>
  filter(industry != "mediated via other industries")  |>
  mutate(type = "final demand")


embodied_emissions <- scope_comparision |>
  group_by(industry) |>
  summarise("2010" = sum(across(2)),
         "2021" = sum(across(3)))|>
  ungroup() |>
  mutate(type = "total demand") |>
  pivot_longer(cols = -c(industry, type),
               values_to = "embodied_emissions",
               names_to = "year")



embodied_emissions <- scope_comparision |>
  group_by(industry) |>
  summarise("2010" = sum(across(2)),
            "2021" = sum(across(3)))|>
  ungroup() |>
  mutate(type = "total demand") |>
  pivot_longer(cols = -c(industry, type),
               values_to = "embodied_emissions",
               names_to = "year")


total_emissions <- final_emissions |>
  rbind(embodied_emissions) |>
  pivot_wider(names_from = type,
              values_from = embodied_emissions) |>
  mutate(mediated =   `total demand` - `final demand`) |>
  select(year, industry, mediated) |>
  # Group by industry and filter for 2010 and 2021
  group_by(industry) |>
  mutate("change in %" = ((mediated[year == "2021"] / mediated[year == "2010"]) - 1)*100)   |>
  ungroup() |>
  pivot_wider(names_from = year,
              values_from = mediated) |>
  arrange(desc(industry)) |>
  select(1,3,4,2)# Sort in descending order of industry


total_emissions |> xtable()

