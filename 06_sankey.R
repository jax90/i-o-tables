# Load necessary library

font_fam <- "PT Serif"
sysfonts::font_add_google(font_fam)
showtext::showtext_auto()

options(scipen = 100, digits = 4)

emissions <- read_parquet( if(user =="jax"){paste0(main_path, "/data/footprint_results_23_data.parquet")}
                           else{main_path}) |>
  mutate(year = as.integer(time_period)) |>
  filter(year >= as.integer(start_year)) |>
  select(-year) |>
  separate(resource_id,into = c('country',"industry"),extra = 'merge',sep = "_") |>
  rename(Y_total = total_final_demand) %>%
  mutate(emissions_per_output = direct_emissions/total_output)


df <-
  emissions |>
  filter(time_period == end_year) |>
  select(industry, country,  matches("Intwght_"),emissions_per_output, Y_total) |>
  mutate(across(matches("Intwght_"), ~ .x * emissions_per_output)) |>
  select(-emissions_per_output)  |>
  unite(industry_ref_area, country, industry, sep = "_")

names(df) <- gsub("^Intwght_", "", names(df))

rel_industries <- intersect(colnames(df), df$industry_ref_area)

L_adjusted <- df |>
  select(all_of(rel_industries)) |>
  filter(df$industry_ref_area %in% rel_industries) |>
  as.matrix()

y <- df |>
  filter(df$industry_ref_area %in% rel_industries) |>
  select( Y_total)  |>
  pull()

Y <- matrix(0, nrow = length(y), ncol = length(y))
diag(Y) <- y

transfer_of_emissions <- L_adjusted %*% Y

colnames(transfer_of_emissions) <- rel_industries

sankey_prepare <- transfer_of_emissions |>  as_tibble() |>
  mutate(industry_ref_area = rel_industries) |>
  pivot_longer(-industry_ref_area,
               names_to = "industry_counterpart_area",
               values_to = "weight") |>
  separate(industry_ref_area,into = c('ref_area',"ref_industry"),extra = 'merge',sep = "_") %>%
  separate(industry_counterpart_area ,into = c('counter_area',"counter_industry"),extra = 'merge',sep = "_") |>
  group_by(ref_industry, counter_industry ) |>
  summarise(
    weight = sum(weight, na.rm = TRUE )
  )

ref_rel_industries <- sankey_prepare |>
  group_by(ref_industry ) |>
  summarise(weight = sum(weight, na.rm = TRUE)) |>
  ungroup() |>
  arrange(desc(weight)) |>
  select(ref_industry) |>
  distinct() |>
  slice(1:10) |>
  pull()


# Create sankey data
sankey_df <- sankey_prepare |>
  mutate(ref_industry = if_else(ref_industry %in% ref_rel_industries,
                                ref_industry, "other industries")) |>
  mutate(counter_industry = if_else(str_detect(counter_industry, "(26|61|62|63)"),
                                    "digital industries", "mediated via other industries")) |>
  group_by(ref_industry, counter_industry) |>
  summarise(weight = sum(weight, na.rm = TRUE)) |>
  ungroup() |>
  arrange(desc(weight)) |>
  mutate(ref_industry = case_when(
    ref_industry == "D35" ~ "electricity & gas supply",
    ref_industry == "C24" ~ "basic metals",
    ref_industry == "B" ~ "mining & quarrying",
    ref_industry == "C23" ~ "non-metallics",
    ref_industry == "C20" ~ "chemicals",
    ref_industry == "C26" ~ "hardware",
    ref_industry == "E37T39" ~ "water supply & waste management",
    ref_industry == "H50" ~ "water transport",
    ref_industry == "A01" ~ "agriculture & fishing",
    ref_industry == "H49" ~ "land transport",
    TRUE ~ ref_industry
  ))


demand <- emissions |> filter(time_period == end_year) |>
  unite(industry_ref_area, industry, country, sep = "_") |>
  select(industry_ref_area, matches("embodied_emissions_"))

names(demand) <- gsub("^embodied_emissions_.+_P3_S13$", "government", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P3_S14$", "households", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P3_S15$", "non-profit", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P51G$", "capital formation", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P5M$", "changes in inventories/ assets", names(demand))

demand <- demand |>
  pivot_longer(-industry_ref_area,
                                  names_to = "demand_type",
                                  values_to = "weight") |>
  separate(industry_ref_area,into = c('ref_industry',"ref_area"),extra = 'merge',sep = "_") |>
  mutate(ref_industry = if_else(str_detect(ref_industry, "(26|61|62|63)"),
                                "digital industries", "mediated via other industries")) |>
  group_by(ref_industry, demand_type ) |>
  summarise(
    weight = sum(weight, na.rm = TRUE ))|>
  ungroup() |>
  rename(counter_industry = ref_industry)


total <- sankey_df$weight |>  sum()


# Combine the two data frames
sankey_data <- sankey_df |>
  left_join(demand, by = "counter_industry") |>
  select(ref_industry, counter_industry, demand_type, weight.x, weight.y) |>
  rename(weight_ref_counter = weight.x,
         weight_counter_demand = weight.y)

pos <- position_sankey(
  v_space=0.05, order="as_is", width=0.05, align="center"
)

sankey_figure <- sankey_data |>
  mutate(
    across(c(weight_ref_counter, weight_counter_demand), \(.x){.x/sum(.x)})
  ) |>
  pivot_stages_longer(
    stages_from=c("ref_industry", "counter_industry", "demand_type"),
    values_from=c("weight_ref_counter", "weight_counter_demand")
  ) |>
  mutate(
    weight = case_when(
      stage[connector == "from"] == "ref_industry" ~ weight_ref_counter,
      stage[connector == "from"] == "counter_industry" ~ weight_counter_demand,
      .default=NA_real_
    ),
    .by=edge_id
  ) |>
  mutate(
    node_size =
      c(sum(weight[connector == "from"]), sum(weight[connector == "to"])) |>
      discard(`==`, 0) |>
      first(),
    .by=node
  ) |>
  mutate(
    node_size_to = node_size[connector == "to"],
    .by=edge_id
  ) |>
  mutate(
   # node_pretty = str_remove_all(node, " *\\(e\\.g\\..+?\\)"),
    node_label = str_c(
      node, ": ", scales::label_percent(accuracy=0.1)(node_size),
      "    "
    ),
    stage = stage |> factor(
      levels=c("ref_industry", "counter_industry", "demand_type"),
      labels=c(
        "industries producing \n direct emissions", "industries associated with \nembodied emissions",
        "final demand where\n embodied emissions\n are transfered to"
      )
    ),
    node = fct_relevel(
      fct_reorder(node, node_size), "other industries", after=0
    )
  ) |>
  arrange(node_size_to) |>
  glimpse()

sankey_figure|>
  ggplot(aes(
    x=stage, y=weight, group=node, connector=connector, edge_id=edge_id
  )) +
  geom_sankeyedge(aes(fill=node), position=pos, alpha=0.6) +
  geom_sankeynode(fill="gray40", position=pos) +
  geom_text(
    aes(label=node_label), stat="sankeynode", position=pos, size=2.5,
    family=font_fam, lineheight = 0.5, hjust=1
  ) +
  scale_fill_manual(
    values=c(
      "#555283",
      "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377",
      "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C"
    )
  ) +
  scale_x_discrete(expand=expansion(c(0.35, 0.08))) +
  theme_void(base_family=font_fam) +
  theme(
    legend.position="none",
    plot.margin=unit(c(0, 2, 1, 2), "mm"),
    axis.text.x=element_text(size=8)
  )


ggsave(paste0("./results/figures/sankey_industry_",edition ,"_", end_year ,".pdf"),
       width=210, height=120, units="mm")

# Europe (Schengen + GB)
europe <- c("AT", "BE", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "IE",
            "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK",
            "BG", "CZ", "DK", "HR", "HU", "PL", "RO", "SE",
            "GB", "CH", "NO")
# United States
us <- c("US")
# Americas (Excluding US)
americas_ex_us <- c("CA", "MX", "AR", "BR")
# Asia-Pacific
asia_pacific <- c("JP", "KR", "IN", "ID", "AU")
# China
china <- c("CN")
# Others
others <- c("RU", "TR", "SA", "ZA", "FIGW1")


sankey_prepare <- transfer_of_emissions |>  as.tibble() |>
  mutate(industry_ref_area = rel_industries) |>
  pivot_longer(-industry_ref_area,
               names_to = "industry_counterpart_area",
               values_to = "weight_ref_counter") |>
  separate(industry_ref_area,into = c('ref_area',"ref_industry"),extra = 'merge',sep = "_") |>
  separate(industry_counterpart_area ,into = c("counter_area",'counter_industry'),extra = 'merge',sep = "_") |>
  mutate(
    counter = case_when(
      counter_area %in%  europe ~ "Europe (Schengen + GB)",
      counter_area %in%  us ~ "US",
      counter_area %in%  americas_ex_us ~ "Americas (without US)",
      counter_area %in%  asia_pacific ~ "Asia-Pacific (without China)",
      counter_area %in%  china ~ "China",
      counter_area %in%  others ~ "others")) |>
  mutate(
    ref = case_when(
      ref_area %in%  europe ~ "Europe (Schengen + GB)",
      ref_area %in%  us ~ "US",
      ref_area %in%  americas_ex_us ~ "Americas (without US)",
      ref_area %in%  asia_pacific ~ "Asia-Pacific (without China)",
      ref_area %in%  china ~ "China",
      ref_area %in%  others ~ "others")) |>
  summarise(
    weight_ref_counter = sum(weight_ref_counter, na.rm = TRUE ), .by = c(ref, counter)
  )

demand <- emissions |> filter(time_period == end_year) |>
  select( country,  matches("embodied_emissions_"))

names(demand) <- gsub("embodied_emissions_", "", names(demand))
names(demand) <- gsub("_(P3_S13|P3_S14|P3_S15|P51G|P5M)$", "", names(demand))

demand <- demand |>  pivot_longer(-country,
                                  names_to = "demand_area",
                                  values_to = "weight_counter_demand") |>
  mutate(
    demand = case_when(
      demand_area %in%  europe ~ "Europe (Schengen + GB)",
      demand_area %in%  us ~ "US",
      demand_area %in%  americas_ex_us ~ "Americas (without US)",
      demand_area %in%  asia_pacific ~ "Asia-Pacific (without China)",
      demand_area %in%  china ~ "China",
      demand_area %in%  others ~ "others")) |>
  mutate(
    counter = case_when(
      country %in%  europe ~ "Europe (Schengen + GB)",
      country %in%  us ~ "US",
      country %in%  americas_ex_us ~ "Americas (without US)",
      country %in%  asia_pacific ~ "Asia-Pacific (without China)",
      country %in%  china ~ "China",
      country %in%  others ~ "others")) |>
  summarise(
    weight_counter_demand = sum(weight_counter_demand, na.rm = TRUE ),
    .by = c(counter, demand)
  )


# Combine the two data frames
sankey_data <- sankey_prepare |>
  left_join(demand, by = "counter") |>
  select(ref, counter, demand, weight_ref_counter, weight_counter_demand) |>
  distinct()

pos <- position_sankey(
  v_space=0.05, order="as_is", width=0.05, align="center"
)

sankey_figure <-sankey_data |>
  mutate(
    across(c(weight_ref_counter, weight_counter_demand), \(.x){.x/sum(.x)})
  ) |>
  pivot_stages_longer(
    stages_from=c("ref", "counter", "demand"),
    values_from=c("weight_ref_counter", "weight_counter_demand")
  ) |>
  mutate(
    weight = case_when(
      stage[connector == "from"] == "ref" ~ weight_ref_counter,
      stage[connector == "from"] == "counter" ~ weight_counter_demand,
      .default=NA_real_
    ),
    .by=edge_id
  ) |>
  mutate(
    node_size =
      c(sum(weight[connector == "from"]), sum(weight[connector == "to"])) |>
      discard(`==`, 0) |>
      first(),
    .by=c(node, stage)
  ) |>
  mutate(
    node_size_to = node_size[connector == "to"],
    .by=edge_id
  ) |>
  mutate(
    # node_pretty = str_remove_all(node, " *\\(e\\.g\\..+?\\)"),
    node_label = str_c(
      node, ": ", scales::label_percent(accuracy=0.1)(node_size),
      "    "
    ),
    stage = stage |> factor(
      levels=c("ref", "counter", "demand"),
      labels=c(
        "countries producing \n direct emissions", "countries associated with \nembodied emissions",
        "final demand where\n embodied emissions\n are transfered to"
      )
    ),
    node = fct_relevel(
      fct_reorder(node, node_size), "others", after=0
    )
  ) |>
  arrange(node_size_to) |>
  glimpse()

sankey_figure |>
  ggplot(aes(
    x=stage, y=weight, group=node, connector=connector, edge_id=edge_id
  )) +
  geom_sankeyedge(aes(fill=node), position=pos, alpha=0.6) +
  geom_sankeynode(fill="gray40", position=pos) +
  geom_text(
    aes(label=node_label), stat="sankeynode", position=pos, size=2.5,
    family=font_fam, lineheight = 0.5, hjust=1
  ) +
  scale_fill_manual(
    values=c(
      "#555283",
      "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377",
      "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C"
    )
  ) +
  scale_x_discrete(expand=expansion(c(0.35, 0.08))) +
  theme_void(base_family=font_fam) +
  theme(
    legend.position="none",
    plot.margin=unit(c(0, 2, 1, 2), "mm"),
    axis.text.x=element_text(size=8)
  )


ggsave(paste0("./results/figures/sankey_country_",edition ,"_", end_year, ".pdf"),
       width=210, height=120, units="mm")

df <-
  emissions |>
  filter(time_period == start_year) |>
  select(industry, country,  matches("Intwght_"),emissions_per_output, Y_total) |>
  mutate(across(matches("Intwght_"), ~ .x * emissions_per_output)) |>
  select(-emissions_per_output)  |>
  unite(industry_ref_area, country, industry, sep = "_")

names(df) <- gsub("^Intwght_", "", names(df))

rel_industries <- intersect(colnames(df), df$industry_ref_area)

L_adjusted <- df |>
  select(all_of(rel_industries)) |>
  filter(df$industry_ref_area %in% rel_industries) |>
  as.matrix()

y <- df |>
  filter(df$industry_ref_area %in% rel_industries) |>
  select( Y_total)  |>
  pull()

Y <- matrix(0, nrow = length(y), ncol = length(y))
diag(Y) <- y

transfer_of_emissions <- L_adjusted %*% Y

colnames(transfer_of_emissions) <- rel_industries

sankey_prepare <- transfer_of_emissions |>  as_tibble() |>
  mutate(industry_ref_area = rel_industries) |>
  pivot_longer(-industry_ref_area,
               names_to = "industry_counterpart_area",
               values_to = "weight") |>
  separate(industry_ref_area,into = c('ref_area',"ref_industry"),extra = 'merge',sep = "_") %>%
  separate(industry_counterpart_area ,into = c('counter_area',"counter_industry"),extra = 'merge',sep = "_") |>
  group_by(ref_industry, counter_industry ) |>
  summarise(
    weight = sum(weight, na.rm = TRUE )
  )

ref_rel_industries <- sankey_prepare |>
  group_by(ref_industry ) |>
  summarise(weight = sum(weight, na.rm = TRUE)) |>
  ungroup() |>
  arrange(desc(weight)) |>
  select(ref_industry) |>
  distinct() |>
  slice(1:10) |>
  pull()


# Create sankey data
sankey_df <- sankey_prepare |>
  mutate(ref_industry = if_else(ref_industry %in% ref_rel_industries,
                                ref_industry, "other industries")) |>
  mutate(counter_industry = if_else(str_detect(counter_industry, "(26|61|62|63)"),
                                    "digital industries", "mediated via other industries")) |>
  group_by(ref_industry, counter_industry) |>
  summarise(weight = sum(weight, na.rm = TRUE)) |>
  ungroup() |>
  arrange(desc(weight)) |>
  mutate(ref_industry = case_when(
    ref_industry == "D35" ~ "electricity & gas supply",
    ref_industry == "C24" ~ "basic metals",
    ref_industry == "B" ~ "mining & quarrying",
    ref_industry == "C23" ~ "non-metallics",
    ref_industry == "C20" ~ "chemicals",
    ref_industry == "C26" ~ "hardware",
    ref_industry == "E37T39" ~ "water supply & waste management",
    ref_industry == "H50" ~ "water transport",
    ref_industry == "A01" ~ "agriculture & fishing",
    ref_industry == "H49" ~ "land transport",
    TRUE ~ ref_industry
  ))


demand <- emissions |> filter(time_period == start_year) |>
  unite(industry_ref_area, industry, country, sep = "_") |>
  select(industry_ref_area, matches("embodied_emissions_"))

names(demand) <- gsub("^embodied_emissions_.+_P3_S13$", "government", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P3_S14$", "households", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P3_S15$", "non-profit", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P51G$", "capital formation", names(demand))
names(demand) <- gsub("^embodied_emissions_.+_P5M$", "changes in inventories/ assets", names(demand))

demand <- demand |>
  pivot_longer(-industry_ref_area,
               names_to = "demand_type",
               values_to = "weight") |>
  separate(industry_ref_area,into = c('ref_industry',"ref_area"),extra = 'merge',sep = "_") |>
  mutate(ref_industry = if_else(str_detect(ref_industry, "(26|61|62|63)"),
                                "digital industries", "mediated via other industries")) |>
  group_by(ref_industry, demand_type ) |>
  summarise(
    weight = sum(weight, na.rm = TRUE ))|>
  ungroup() |>
  rename(counter_industry = ref_industry)


total <- sankey_df$weight |>  sum()


# Combine the two data frames
sankey_data <- sankey_df |>
  left_join(demand, by = "counter_industry") |>
  select(ref_industry, counter_industry, demand_type, weight.x, weight.y) |>
  rename(weight_ref_counter = weight.x,
         weight_counter_demand = weight.y)

pos <- position_sankey(
  v_space=0.05, order="as_is", width=0.05, align="center"
)

sankey_figure <- sankey_data |>
  mutate(
    across(c(weight_ref_counter, weight_counter_demand), \(.x){.x/sum(.x)})
  ) |>
  pivot_stages_longer(
    stages_from=c("ref_industry", "counter_industry", "demand_type"),
    values_from=c("weight_ref_counter", "weight_counter_demand")
  ) |>
  mutate(
    weight = case_when(
      stage[connector == "from"] == "ref_industry" ~ weight_ref_counter,
      stage[connector == "from"] == "counter_industry" ~ weight_counter_demand,
      .default=NA_real_
    ),
    .by=edge_id
  ) |>
  mutate(
    node_size =
      c(sum(weight[connector == "from"]), sum(weight[connector == "to"])) |>
      discard(`==`, 0) |>
      first(),
    .by=node
  ) |>
  mutate(
    node_size_to = node_size[connector == "to"],
    .by=edge_id
  ) |>
  mutate(
    # node_pretty = str_remove_all(node, " *\\(e\\.g\\..+?\\)"),
    node_label = str_c(
      node, ": ", scales::label_percent(accuracy=0.1)(node_size),
      "    "
    ),
    stage = stage |> factor(
      levels=c("ref_industry", "counter_industry", "demand_type"),
      labels=c(
        "industries producing \n direct emissions", "industries associated with \nembodied emissions",
        "final demand where\n embodied emissions\n are transfered to"
      )
    ),
    node = fct_relevel(
      fct_reorder(node, node_size), "other industries", after=0
    )
  ) |>
  arrange(node_size_to) |>
  glimpse()

sankey_figure|>
  ggplot(aes(
    x=stage, y=weight, group=node, connector=connector, edge_id=edge_id
  )) +
  geom_sankeyedge(aes(fill=node), position=pos, alpha=0.6) +
  geom_sankeynode(fill="gray40", position=pos) +
  geom_text(
    aes(label=node_label), stat="sankeynode", position=pos, size=2.5,
    family=font_fam, lineheight = 0.5, hjust=1
  ) +
  scale_fill_manual(
    values=c(
      "#555283",
      "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377",
      "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C"
    )
  ) +
  scale_x_discrete(expand=expansion(c(0.35, 0.08))) +
  theme_void(base_family=font_fam) +
  theme(
    legend.position="none",
    plot.margin=unit(c(0, 2, 1, 2), "mm"),
    axis.text.x=element_text(size=8)
  )


ggsave(paste0("results/figures/sankey_industry_",edition ,"_", start_year,".pdf"), width=210, height=120, units="mm")

sankey_prepare <- transfer_of_emissions |>  as.tibble() |>
  mutate(industry_ref_area = rel_industries) |>
  pivot_longer(-industry_ref_area,
               names_to = "industry_counterpart_area",
               values_to = "weight_ref_counter") |>
  separate(industry_ref_area,into = c('ref_area',"ref_industry"),extra = 'merge',sep = "_") |>
  separate(industry_counterpart_area ,into = c("counter_area",'counter_industry'),extra = 'merge',sep = "_") |>
  mutate(
    counter = case_when(
      counter_area %in%  europe ~ "Europe (Schengen + GB)",
      counter_area %in%  us ~ "US",
      counter_area %in%  americas_ex_us ~ "Americas (without US)",
      counter_area %in%  asia_pacific ~ "Asia-Pacific (without China)",
      counter_area %in%  china ~ "China",
      counter_area %in%  others ~ "others")) |>
  mutate(
    ref = case_when(
      ref_area %in%  europe ~ "Europe (Schengen + GB)",
      ref_area %in%  us ~ "US",
      ref_area %in%  americas_ex_us ~ "Americas (without US)",
      ref_area %in%  asia_pacific ~ "Asia-Pacific (without China)",
      ref_area %in%  china ~ "China",
      ref_area %in%  others ~ "others")) |>
  summarise(
    weight_ref_counter = sum(weight_ref_counter, na.rm = TRUE ), .by = c(ref, counter)
  )

demand <- emissions |> filter(time_period == start_year) |>
  select( country,  matches("embodied_emissions_"))

names(demand) <- gsub("embodied_emissions_", "", names(demand))
names(demand) <- gsub("_(P3_S13|P3_S14|P3_S15|P51G|P5M)$", "", names(demand))

demand <- demand |>  pivot_longer(-country,
                                  names_to = "demand_area",
                                  values_to = "weight_counter_demand") |>
  mutate(
    demand = case_when(
      demand_area %in%  europe ~ "Europe (Schengen + GB)",
      demand_area %in%  us ~ "US",
      demand_area %in%  americas_ex_us ~ "Americas (without US)",
      demand_area %in%  asia_pacific ~ "Asia-Pacific (without China)",
      demand_area %in%  china ~ "China",
      demand_area %in%  others ~ "others")) |>
  mutate(
    counter = case_when(
      country %in%  europe ~ "Europe (Schengen + GB)",
      country %in%  us ~ "US",
      country %in%  americas_ex_us ~ "Americas (without US)",
      country %in%  asia_pacific ~ "Asia-Pacific (without China)",
      country %in%  china ~ "China",
      country %in%  others ~ "others")) |>
  summarise(
    weight_counter_demand = sum(weight_counter_demand, na.rm = TRUE ),
    .by = c(counter, demand)
  )


# Combine the two data frames
sankey_data <- sankey_prepare |>
  left_join(demand, by = "counter") |>
  select(ref, counter, demand, weight_ref_counter, weight_counter_demand) |>
  distinct()

pos <- position_sankey(
  v_space=0.05, order="as_is", width=0.05, align="center"
)

sankey_figure <-sankey_data |>
  mutate(
    across(c(weight_ref_counter, weight_counter_demand), \(.x){.x/sum(.x)})
  ) |>
  pivot_stages_longer(
    stages_from=c("ref", "counter", "demand"),
    values_from=c("weight_ref_counter", "weight_counter_demand")
  ) |>
  mutate(
    weight = case_when(
      stage[connector == "from"] == "ref" ~ weight_ref_counter,
      stage[connector == "from"] == "counter" ~ weight_counter_demand,
      .default=NA_real_
    ),
    .by=edge_id
  ) |>
  mutate(
    node_size =
      c(sum(weight[connector == "from"]), sum(weight[connector == "to"])) |>
      discard(`==`, 0) |>
      first(),
    .by=c(node, stage)
  ) |>
  mutate(
    node_size_to = node_size[connector == "to"],
    .by=edge_id
  ) |>
  mutate(
    # node_pretty = str_remove_all(node, " *\\(e\\.g\\..+?\\)"),
    node_label = str_c(
      node, ": ", scales::label_percent(accuracy=0.1)(node_size),
      "    "
    ),
    stage = stage |> factor(
      levels=c("ref", "counter", "demand"),
      labels=c(
        "countries producing \n direct emissions", "countries associated with \nembodied emissions",
        "final demand where\n embodied emissions\n are transfered to"
      )
    ),
    node = fct_relevel(
      fct_reorder(node, node_size), "others", after=0
    )
  ) |>
  arrange(node_size_to) |>
  glimpse()

sankey_figure |>
  ggplot(aes(
    x=stage, y=weight, group=node, connector=connector, edge_id=edge_id
  )) +
  geom_sankeyedge(aes(fill=node), position=pos, alpha=0.6) +
  geom_sankeynode(fill="gray40", position=pos) +
  geom_text(
    aes(label=node_label), stat="sankeynode", position=pos, size=2.5,
    family=font_fam, lineheight = 0.5, hjust=1
  ) +
  scale_fill_manual(
    values=c(
      "#555283",
      "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377",
      "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C"
    )
  ) +
  scale_x_discrete(expand=expansion(c(0.35, 0.08))) +
  theme_void(base_family=font_fam) +
  theme(
    legend.position="none",
    plot.margin=unit(c(0, 2, 1, 2), "mm"),
    axis.text.x=element_text(size=8)
  )


ggsave(paste0("results/figures/sankey_country_", edition ,"_", start_year,".pdf"), width=210, height=120, units="mm")
#
