options(scipen = 100, digits = 4)

# Centralize ggplot theme for consistency across all plots
theme_publication <- theme_tufte(base_size = 14, base_family = "sans") +
  theme(
    legend.position = "bottom",
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 16) # Facet labels slightly larger
  )

# Centralize color palettes
color_palette_shares <- c(
  "absolute emissions" = "#0072B2",
  "share of emissions" = "#D55E00",
  "share of output" = "#B03A2E"
)

color_palette_industries <- c(
  "mediated via other industries" = "grey",
  "hardware" = "#68011f",
  "communications" = "#f2a27d",
  "IT services" = "#2367ae"
)

output_dir <- here("results", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# --- Data Loading and Preparation ---

message("Loading and preparing data for visualizations...")

# Get deflated output for 'share of output' metric
values_agg <- format_iot(
  folder = file.path(data_directory, "values"),
  exdir = file.path(data_directory, "values"),
  update = FALSE,
  edition = edition
)

if("resource_id" %in% colnames(values_agg)) values_agg = values_agg %>% rename(rowLabels = resource_id)

price_index <- get_value_added_price_index(2021, update = FALSE) |>
  rename(deflator = value, ref_area = country, time_period = year) |>
  select(-base)

values_agg_deflated <- values_agg %>%
  separate(rowLabels, into = c('ref_area', 'industry'), sep = "_", remove = FALSE, extra = 'merge') %>%
  full_join(price_index, by = c("ref_area", "industry", "time_period")) |>
  mutate(across(where(is.numeric), ~ .x * deflator)) |>
  select(-c("ref_area", "industry", "deflator"))

prd <- values_agg_deflated %>%
  pivot_longer(-c(rowLabels, time_period)) %>%
  filter(!grepl('W2', rowLabels)) %>%
  group_by(time_period) %>%
  summarise(
    share_of_output = sum(if_else(grepl("C26|J61|J62_63", rowLabels), value, 0), na.rm = TRUE) / sum(value, na.rm = TRUE)
  )

rm(values_agg, price_index, values_agg_deflated)

footprint_results_path <- file.path(data_directory, paste0("footprint_results_", edition, "_data.parquet"))

emissions <- read_parquet(footprint_results_path) |>
  separate(resource_id, into = c('country', 'industry'), extra = 'merge', sep = "_") |>
  mutate(year = as.integer(time_period)) |>
  filter(year >= as.integer(start_year), year <= as.integer(end_year)) |>
  select(-year)

df <- emissions |>
  select(
    industry, country, time_period, direct_emissions,
    matches("embodied_emissions"), matches("scope"),
    X_total = total_output
  )

# --- Generate Figures and Tables ---

message("Generating Figure 1: Emissions Over Time...")

fig_frame <- emissions %>%
  mutate(production_footprint = rowSums(select(., starts_with("embodied_emissions")), na.rm = TRUE)) %>%
  group_by(time_period) %>%
  summarise(
    direct_emissions = sum(direct_emissions,na.rm=T),
    share_footprint = sum(production_footprint, na.rm = TRUE) / sum(direct_emissions, na.rm = TRUE),
    total_footprint = sum(production_footprint, na.rm = TRUE)
  ) %>%
  left_join(prd, by = 'time_period')

fd_emissions_over_time <- ggplot(fig_frame, aes(x = as.integer(time_period))) +
  geom_line(aes(y = total_footprint / 1000, color = "absolute emissions")) +
  geom_point(aes(y = total_footprint / 1000, color = "absolute emissions"), size = 3) +
  geom_text(aes(y = total_footprint / 1000, label = round(total_footprint / 1000)), vjust = -0.8, color = "#0072B2", size = 4) +
  geom_line(aes(y = share_footprint * 100 * 100, color = "share of emissions")) +
  geom_point(aes(y = share_footprint * 100 * 100, color = "share of emissions"), size = 3) +
  geom_text(aes(y = share_footprint * 100 * 100, label = round(share_footprint * 100, 2)), vjust = 1.8, color = "#D55E00", size = 4) +
  geom_line(aes(y = share_of_output * 100 * 100, color = "share of output")) +
  geom_point(aes(y = share_of_output * 100 * 100, color = "share of output"), size = 3) +
  geom_text(aes(y = share_of_output * 100 * 100, label = round(share_of_output * 100, 2)), vjust = -1.8, color = "#B03A2E", size = 4) +
  scale_color_manual(values = color_palette_shares, name = "") +
  scale_y_continuous(
    name = "CO2e in Mt",
    sec.axis = sec_axis(~ . / 100, name = "Share in %"),
    limits = c(0, 3000)
  ) +
  labs(x = "Year", y = "CO2e in Mt") +
  theme_publication

ggsave(
  file.path(output_dir, paste0("emissions_over_time_", edition, "_", start_year, "_", end_year, ".pdf")),
  plot = fd_emissions_over_time, width = 8, height = 6, dpi = 300
)

# Figure 2: Emissions by Industry (Stacked Area)

message("Generating Figure 2: Emissions by Industry...")
fig_frame_industry <- df %>%
  mutate(embodied_emissions = rowSums(select(., starts_with("embodied_emissions")), na.rm = TRUE)) |>
  mutate(industry = case_when(
    industry == "C26" ~ "hardware",
    industry == "J61" ~ "communications",
    industry == "J62_63" ~ "IT services",
    TRUE ~ "mediated via other industries"
  )) |>
  group_by(time_period, industry) |>
  summarise(embodied_emissions = sum(embodied_emissions, na.rm = TRUE), .groups = "drop") |>
  drop_na()

industry_order <- c("mediated via other industries", "hardware", "communications", "IT services")

emissions_over_time_by_industry <- fig_frame_industry |>
  mutate(industry = factor(industry, levels = industry_order)) |>
  ggplot(aes(x = as.integer(time_period), y = embodied_emissions / 1000, group = industry, fill = industry)) +
  geom_area(position = "stack") +
  labs(x = "Year", y = "CO2e in Mt", fill = "") +
  scale_fill_manual(values = color_palette_industries) +
  scale_y_continuous(limits = c(0, 3000)) +
  theme_publication +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

ggsave(
  file.path(output_dir, paste0("emissions_over_time_by_industry_", edition, "_", start_year, "_", end_year, ".pdf")),
  plot = emissions_over_time_by_industry, width = 8, height = 6, dpi = 300
)

# Table 1: Data for Figure 2

fig_frame_industry |>
  mutate(embodied_emissions = as.integer(round(embodied_emissions / 1000))) |>
  pivot_wider(names_from = "industry", values_from = "embodied_emissions") |>
  rename(year = time_period) |>
  xtable()

# Figure 3: Scopes by Industry

scopes_industry <- df |>
  select(time_period, matches("scope")) %>%
  distinct() %>%
  pivot_longer(matches("scope"), names_to = "scope", values_to = "value") %>%
  mutate(
    Scope = str_extract(scope, "^[^_]+"),
    industry = str_extract(scope, "(?<=_).+")
  ) %>%
  mutate(
    industry = case_when(
      industry == "C26" ~ "hardware",
      industry == "J61" ~ "communications",
      industry == "J62_63" ~ "IT services",
      TRUE ~ industry
    ),
    scope = case_when(
      Scope == "scope1" ~ "Scope 1",
      Scope == "scope2" ~ "Scope 2",
      Scope == "scope3" ~ "Scope 3",
      TRUE ~ NA_character_
    )
  ) |>
  select(time_period, industry, scope, value) |>
  mutate(
    scope = factor(scope, levels = c("Scope 1", "Scope 2", "Scope 3")),
    industry = factor(industry, levels = c("hardware", "communications", "IT services"))
  )

scopes_by_industry <- ggplot() +
  geom_col(data = scopes_industry %>% filter(time_period == end_year),
           aes(x = scope, y = value / 1000, fill = industry),
           alpha = 0.6) +

  geom_point(data = scopes_industry %>% filter(time_period == start_year),
             aes(x = scope, y = value / 1000, color = as.character(start_year)),
             size = 4, shape = 18) +

  geom_point(data = data.frame(scope = 'Scope 1', value = -9999),
             aes(x = scope, y = value, color = as.character(end_year)),
             size = 4, shape = 15) +

  scale_color_manual(
    name = "year",
    values = setNames(c("red", "grey50"), c(as.character(start_year), as.character(end_year)))
  ) +
  scale_fill_manual(
    values = color_palette_industries,
    guide = "none"
  ) +
  facet_wrap(~industry, scales = "free_y") +
  labs(x = "", y = "CO2e in Mt") +
  coord_cartesian(ylim = c(0, max(scopes_industry$value[scopes_industry$time_period %in% c(start_year, end_year)], na.rm = TRUE) / 1000 * 1.05)) +
  theme_publication +
  theme(legend.position = "bottom")



ggsave(
  file.path(output_dir, paste0("scopes_by_industry_", edition, "_", start_year, "_", end_year, ".pdf")),
  plot = scopes_by_industry, width = 12, height = 6, dpi = 300
)

# Table 2: Scope Comparison Data

scope_comparison <- scopes_industry |>
  rename(year = time_period) |>
  filter(year == end_year | year == start_year) |>
  pivot_wider(names_from = "year", values_from = "value") |>
  mutate(
    "change_in_percent" = (`2021` - `2010`) / `2010` * 100
  ) |>
  group_by(industry) |>
  mutate(
    "relative_2010" = `2010` / sum(`2010`),
    "relative_2021" = `2021` / sum(`2021`)
  ) |>
  ungroup() |>
  mutate(across(c(`2010`, `2021`), ~ as.integer(round(.x / 1000))))

scope_comparison |> xtable()


# Table 3: Mediated Emissions Calculation

final_emissions <- fig_frame_industry |>
  mutate(embodied_emissions = as.integer(round(embodied_emissions / 1000))) |>
  rename(year = time_period) |>
  filter(year == end_year | year == start_year) |>
  filter(industry != "mediated via other industries") |>
  mutate(type = "final demand")

total_demand_emissions <- scope_comparison |>
  group_by(industry, year = "2010") |>
  summarise(embodied_emissions = sum(`2010`)) |>
  bind_rows(
    scope_comparison |> group_by(industry, year = "2021") |> summarise(embodied_emissions = sum(`2021`))
  ) |>
  mutate(type = "total demand")

total_emissions <- final_emissions |>
  bind_rows(total_demand_emissions) |>
  pivot_wider(names_from = type, values_from = embodied_emissions) |>
  mutate(mediated = `total demand` - `final demand`) |>
  select(year, industry, mediated) |>
  group_by(industry) |>
  mutate(change_in_percent = (mediated[year == "2021"] / mediated[year == "2010"] - 1) * 100) |>
  ungroup() |>
  pivot_wider(names_from = year, values_from = mediated) |>
  arrange(desc(industry)) |>
  select(industry, `2010`, `2021`, change_in_percent)

total_emissions |> xtable()
