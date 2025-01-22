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

#main_path <- "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23/footprint_results_23_data.parquet"
main_path <- "/home/jannaaxe/Schreibtisch/Projekte/IO-analysis"
setwd("/home/jannaaxe/Schreibtisch/Projekte/IO-analysis")

x = c('arrow','ggthemes','xtable')
lapply(x,library,character.only=T)

options(scipen = 100, digits = 4)

emissions <- read_parquet( if(user =="jax"){paste0(main_path, "/data/footprint_results_23_data.parquet")}else(main_path)) |>
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
  geom_bar(data = scopes_industry %>% filter(time_period == 2022),
           aes(x = scope, y = value / 1000, color = "2022", fill = industry),
           stat = "identity", alpha = 0.5) +
  # Add points for 2010
  geom_point(data = scopes_industry %>% filter(time_period == "2010"),
             aes(x = scope, y = value / 1000, color = "2010", shape = "2010"),
             size = 4) +
  # Define shape and color scales for legend
  scale_color_manual(name = "year", values = c("2010" = "red", "2022" = "darkgrey")) +
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
 # scale_y_log10() +
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




