
#main_path <- "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23/footprint_results_23_data.parquet"
main_path <- "/home/jannaaxe/Schreibtisch/Projekte/IO-analysis"
setwd(main_path)

x = c('arrow','ggthemes','xtable')
lapply(x,library,character.only=T)

options(scipen = 100, digits = 4)

emissions <- read_parquet( if(user =="jax"){paste0(main_path, "/data/footprint_results_",edition,"_data.parquet")}else{main_path}) |>
  separate(resource_id,into = c('country',"industry"),extra = 'merge',sep = "_") |>
  mutate(year = as.integer(time_period)) |>
  filter(year >= as.integer(start_year)) |>
  filter(year <= as.integer(end_year)) |>
  select(-year)

df <- emissions |>
  select(industry, country,
         time_period, direct_emissions, matches("embodied_emissions"),  absolute_emissions, matches("scope"),
         X_total = total_output)

fig_frame <- df |>
  mutate(embodied_emissions = rowSums(df |> select(starts_with("embodied_emissions_")), na.rm = TRUE)) |>
  select(-starts_with("embodied_emissions_")) %>%
  group_by(time_period) |>
  summarise(embodied_total  = sum(embodied_emissions, na.rm = TRUE),
           # embodied_final = sum(ifelse(grepl("(26|61|62|63)", industry), embodied_emissions, 0), na.rm = TRUE)) |>
            absolute_emissions = mean(absolute_emissions)) %>%
  mutate(rel_embodied_total = embodied_total / absolute_emissions) |>
 # mutate(rel_embodied_final = embodied_final / absolute_emissions) |>
  ungroup() |>
  drop_na()

write_parquet(fig_frame, paste0(main_path,"/results/emissions_over_time"))


# Results Figure 1:

fd_emissions_over_time <- ggplot(fig_frame, aes(x = as.integer(time_period))) +

  geom_line(aes(y = embodied_total / 1000, linetype = "total demand", color = "absolute")) +
  geom_text(aes(y = embodied_total / 1000, label = round(embodied_total / 1000)),
            vjust = -0.5, color = "#0072B2", size = 4.5, family = "serif") +

  geom_line(aes(y = rel_embodied_total * 100 * 100, linetype = "total demand", color = "relative")) +
  geom_text(aes(y = rel_embodied_total * 100 * 100, label = round(rel_embodied_total * 100, 2)),
            vjust = -0.5, color = "#D55E00", size = 4.5 , family = "serif") +


  scale_linetype_manual(values = c("final demand" = "dashed", "total demand" = "solid"),
                        name = "") +

  scale_color_manual(values = c("absolute" = "#0072B2", "relative" = "#D55E00"),
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

ggsave(paste0("./results/figures/emissions_over_time_by_industry_",edition ,"_", start_year, "_", end_year ,".pdf"),
       plot = emissions_over_time_by_industry, width = 8, height = 6, dpi = 300)

fig_frame_industry |>
  pivot_wider(names_from="industry",
              values_from="embodied_emissions") |>
  rename(year = time_period) |>
  xtable()

scopes_industry <- df |>
   select(time_period,matches("scope")) %>%
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


scopes_by_industry <- ggplot() +
  geom_bar(data = scopes_industry %>% filter(time_period == end_year),
           aes(x = scope, y = value / 1000, color = end_year, fill = industry),
           stat = "identity", alpha = 0.5) +
  # Add points for 2010
  geom_point(data = scopes_industry %>% filter(time_period == start_year),
             aes(x = scope, y = value / 1000, color =start_year, shape = start_year),
             size = 4) +
  # Define shape and color scales for legend
  scale_color_manual(name = "year",
                     values = c( start_year = "red", end_year= "darkgrey")) +
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
  theme(
    legend.position = "right"
  ) +
  guides(shape = "none", fill ="none") +
  #scale_y_log10() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),  # Set overall text size
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
  ungroup()

# Table 4:
scope_comparision|>
  xtable()

final_emissions <- fig_frame_industry |>
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
  mutate("change in %" = (mediated[year == "2021"] / mediated[year == "2010"]) - 1)   |>
  ungroup() |>
  pivot_wider(names_from = year,
              values_from = mediated) |>
  arrange(desc(industry)) |>
  select(1,3,4,2)# Sort in descending order of industry


total_emissions |> xtable()

