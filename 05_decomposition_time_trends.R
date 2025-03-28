
options(scipen = 100, digits = 4)

emissions <- read_parquet( if(user =="jax"){paste0(main_path, "/data/footprint_results_",edition,"_data.parquet")}
                           else{main_path}) |>
  mutate(year = as.integer(time_period)) |>
  filter(year >= as.integer(start_year)) |>
  filter(year <= as.integer(end_year)) |>
  select(-year) |>
  separate(resource_id,into = c('country',"industry"),extra = 'merge',sep = "_") %>%
  rename(Y_total = total_final_demand) %>%
  mutate(emissions_per_output = direct_emissions/total_output)

# embodied emissions
embodied_all <- emissions |>
  mutate(embodied = rowSums(emissions |> select(direct_emissions), na.rm = TRUE)) |>
  summarise(embodied = sum(embodied, na.rm = TRUE), .by = c(time_period)) |>
  drop_na() |>
  mutate(embodied = embodied/embodied[time_period ==start_year]) |>
  mutate(ICT = "absolute")

embodied_ICT <- emissions |>
  mutate(embodied = rowSums(emissions |> select(starts_with("embodied_emissions")), na.rm = TRUE)) |>
  mutate(ICT = case_when(
    industry == "C26" ~ "hardware",
    industry == "J61" ~ "communications",
    industry == "J62_63" ~ "IT services",
    TRUE ~ "mediated"
  )) |>
  summarise(embodied = sum(embodied, na.rm = TRUE), .by = c(time_period, ICT)) |>
  drop_na() |>
  group_by(ICT) |>
  mutate(embodied = embodied/embodied[time_period ==start_year]) |>
  ungroup()

decomposition <-bind_rows(embodied_all, embodied_ICT)

demand_all <- emissions |>
  select(time_period, Y_total) |>
  summarise(Y = sum(Y_total, na.rm = TRUE), .by = c(time_period)) |>
  drop_na() |>
  mutate(Y = Y/Y[time_period == start_year]) |>
  mutate(ICT = "absolute")

demand_ICT <- emissions |>
  mutate(ICT = case_when(
    industry == "C26" ~ "hardware",
    industry == "J61" ~ "communications",
    industry == "J62_63" ~ "IT services",
    TRUE ~ "mediated"
  )) |>
  summarise(Y = sum(Y_total, na.rm = TRUE), .by = c(time_period, ICT)) |>
  drop_na() |>
  group_by(ICT) |>
  mutate(Y = Y/Y[time_period ==start_year]) |>
  ungroup()

demand <-bind_rows(demand_all, demand_ICT)

decomposition <- decomposition |>
  left_join(demand, by= c( "time_period", "ICT"))

rel_industries <- emissions |>
  unite("industry_ref_area", country, industry, sep = "_")

rel_industries <- intersect(colnames(rel_industries), rel_industries$industry_ref_area)

weights_all <-emissions |>
  select(time_period, all_of(rel_industries)) |>
  group_by(time_period) |>
  summarise(across(everything(), \(.x){sum(.x, na.rm=TRUE)})) |>
  drop_na() |>
  pivot_longer(cols= -time_period, names_to= "industry_country",
               values_to="mean_wght") |>
  #separate_wider_regex(industry_country, c(country = ".*", "_", industry = ".*")) |>
  separate(industry_country,into = c('country',"industry"),extra = 'merge',sep = "_") %>%
  left_join(emissions |> select(time_period, industry, country, Y_total),
            by = c("time_period", "industry", "country")) |>
  drop_na() |>
  summarise(mean_wght = weighted.mean(x =mean_wght , w = Y_total, na.rm = TRUE), .by = c(time_period)) |>
  mutate(mean_wght = mean_wght/mean_wght[time_period ==start_year]) |>
  mutate(ICT = "absolute")

weights_ICT <- emissions |>
  select(matches("Intwght"), time_period ) |>
  group_by(time_period) |>
  summarise(across(everything(), \(x){sum(x, na.rm=TRUE)})) |>
  drop_na() |>
  pivot_longer(cols = -time_period,
               names_to = "industry_country",
               values_to= "Intwght") |>
  mutate(industry_country = str_replace(industry_country,"Intwght_", "")) |>
  separate(industry_country,into = c('country',"industry"),extra = 'merge',sep = "_") |>
  left_join(emissions |> select (Y_total, time_period, industry, country),
            by= c( "time_period", "industry" , "country")) |>
  mutate(ICT = case_when(
    industry == "C26" ~ "hardware",
    industry == "J61" ~ "communications",
    industry == "J62_63" ~ "IT services",
    TRUE ~ "mediated"
  )) |>
  drop_na() |>
  summarise(mean_wght = weighted.mean(x =Intwght , w = Y_total, na.rm = TRUE), .by = c(time_period, ICT)) |>
  drop_na() |>
  group_by(ICT) |>
  mutate(mean_wght = mean_wght/mean_wght[time_period ==start_year]) |>
  ungroup()


weights <-bind_rows(weights_all, weights_ICT)

decomposition <- decomposition |>
  left_join(weights, by= c( "time_period", "ICT"))

intensity_all <- emissions |>
  select(time_period, direct_emissions, total_output) |>
  summarise(across(everything(), \(x){sum(x, na.rm=TRUE)}), .by = time_period) |>
  drop_na() |>
  mutate(emission_intensity = direct_emissions/total_output) |>
  mutate(emission_intensity = emission_intensity/emission_intensity[time_period ==start_year]) |>
  mutate(ICT = "absolute") |>
  select(-direct_emissions,- total_output)

intensity_ICT <- emissions |>
  select( time_period, emissions_per_output, matches("Intwght_"))

colnames(intensity_ICT) <- sub("Intwght_", "", colnames(intensity_ICT))

intensity_ICT <- intensity_ICT |>
  pivot_longer(
    cols = -c(time_period,  emissions_per_output),
    names_to = "industry_country",
    values_to = "Intwght_"
  )


intensity_ICT <- intensity_ICT |> left_join( emissions|> select(time_period, industry, country, Y_total) |>
                                               unite(industry_country,  country, industry, sep = "_") ,
                                             by = c("industry_country", "time_period"))

intensity_ICT <- intensity_ICT |> mutate(weight = Intwght_*Y_total)

intensity_ICT <- intensity_ICT |>
  mutate(ICT = case_when(
    grepl("C26", industry_country  ) ~ "hardware",
    grepl("J61", industry_country  ) ~ "communications",
    grepl("J62_63", industry_country  ) ~ "IT services",
    TRUE ~ "mediated"
  ))|>
  drop_na() |>
  summarise(emission_intensity = weighted.mean(x =emissions_per_output  , w = weight, na.rm = TRUE), .by = c(time_period, ICT)) |>
  drop_na()

intensity_ICT <- intensity_ICT |>
  group_by(ICT) |>
  mutate(emission_intensity = emission_intensity/emission_intensity[time_period ==start_year]) |>
  ungroup()

intensity <-bind_rows(intensity_all, intensity_ICT)

decomposition <- decomposition |>
  left_join(intensity, by= c( "time_period", "ICT"))


facet_labels <- c("embodied" = "a) embodied emissions",
                  "emission_intensity" = "b) emission intensity",
                  "mean_wght" = "c) sum of Leontief weights",
                  "Y" = "d) final demand")

decomposition_order <- c( "embodied",  "emission_intensity","mean_wght", "Y")

decomposition_final <- decomposition |>
  mutate(ICT = str_replace(ICT, "mediated", "mediated via other industries")) |>
  mutate(ICT = str_replace(ICT, "absolute", "total economy")) |>
  pivot_longer(
    cols=-c(time_period, ICT),
    names_to= "decomposition",
    values_to= "value") |>
  mutate(decomposition = factor(decomposition, levels = decomposition_order)) |>
  mutate(ICT = factor(ICT, levels = c("IT services", "communications", "hardware",
                                      "mediated via other industries", "total economy")))


decomposition_changes <- decomposition_final |>
  ggplot(aes(x = as.integer(time_period), y = value *100, color = ICT, linetype = ICT)) +
  geom_line(linewidth =0.65) +
  facet_wrap(~decomposition,
             labeller = as_labeller(facet_labels),
             ncol = 2,
             axes = "all_y") +
  scale_linetype_manual(values = c(
    "mediated via other industries"  = "solid",
    "hardware" = "solid",
    "communications" = "solid",
    "IT services" = "solid",
    "total economy" = "dashed"  # Dashed line for "total economy's emissions"
  )) +
  scale_color_manual(values = c(
    "mediated via other industries" = "darkgrey",
    "hardware" = "#68011f",
    "communications" = "#f2a27d",
    "IT services" = "#2367ae",
    "total economy" = "black"  # Assign color for "total economy's emissions" if needed
  )) +
  ggtitle(paste0("change in % (baseline ",start_year,")")) +
  labs(x = "year", y = "") +
  theme_tufte() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, family = "serif", size = 14),
    text = element_text(family = "serif", size = 14),  # Set overall text to serif and size 12
    axis.title = element_text(size = 14),  # Set axis titles to size 12
    legend.text = element_text(size = 12),  # Set legend text to size 12
    legend.title=element_blank(),  # Set legend title to size 12
    strip.text = element_text(size = 14)  # Set facet strip text to size 12
  )

decomposition_changes


ggsave(paste0("./results/figures/decomposition_changes_deflat_",edition ,"_", start_year, "_",end_year ,".pdf"),
       plot = decomposition_changes, width = 8, height = 6, dpi = 300)

