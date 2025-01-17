# Load necessary library
library(tidyverse)
library(networkD3)
library(jsonlite)
#library(ggsankey)
library(ggsankeyfier) 

font_fam <- "PT Serif"
sysfonts::font_add_google(font_fam)
showtext::showtext_auto()


setwd("C:/Users/JAX/Desktop/sektor_analyse")


options(scipen = 100, digits = 4)

emissions <- read_rds("data/Leontief_weights_ghgs_all.rds") |> 
  separate_wider_regex(industry_ref_area, c(industry = ".*", "_", country = ".*")) 

df <- 
  emissions |> 
  filter(time_period == "2021") |> 
  select(industry, country,  matches("Int_wght_"),emissions_per_output, Y_total) |> 
  mutate(across(matches("Int_wght_"), ~ .x * emissions_per_output)) |> 
  select(-emissions_per_output)  |> 
  unite(industry_ref_area, industry, country, sep = "_") 

names(df) <- gsub("^Int_wght_", "", names(df))


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

sankey_prepare <- transfer_of_emissions |>  as.tibble() |> 
  mutate(industry_ref_area = rel_industries) |> 
  pivot_longer(-industry_ref_area, 
               names_to = "industry_counterpart_area", 
               values_to = "weight") |> 
  separate_wider_regex(industry_ref_area, 
                       c(ref_industry = ".*", "_", ref_area = ".*")) |> 
  separate_wider_regex(industry_counterpart_area,
                       c(counter_industry = ".*", "_", counter_area = ".*")) |> 
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


demand <- emissions |> filter(time_period == "2021") |> 
  unite(industry_ref_area, industry, country, sep = "_") |> 
  select(industry_ref_area, matches("embodied_emissions_"))

names(demand) <- gsub("embodied_emissions_P3_S13", "government", names(demand))
names(demand) <- gsub("embodied_emissions_P3_S14", "households", names(demand))
names(demand) <- gsub("embodied_emissions_P3_S15", "non-profit", names(demand))
names(demand) <- gsub("embodied_emissions_P51G", "capital formation", names(demand))
names(demand) <- gsub("embodied_emissions_P5M", "changes in inventories/ assets", names(demand))

demand <- demand |>  pivot_longer(-industry_ref_area, 
                                  names_to = "demand_type_counterpart_area", 
                                  values_to = "weight") |> 
  separate_wider_regex(industry_ref_area, 
                       c(ref_industry = ".*", "_", ref_area = ".*")) |> 
  separate_wider_regex(demand_type_counterpart_area,
                       c(demand_type= ".*", "_", counter_area = ".*")) |> 
  mutate(ref_industry = if_else(str_detect(ref_industry, "(26|61|62|63)"), 
                                "digital industries", "mediated via other industries")) |>
  group_by(ref_industry, demand_type ) |> 
  summarise(
    weight = sum(weight, na.rm = TRUE ))|>  
  rename(counter_industry = ref_industry)




total <- sankey_df$weight |>  sum()


# Combine the two data frames
sankey_data <- sankey_df |> 
  left_join(demand, by = "counter_industry") |> 
  select(ref_industry, counter_industry, demand_type, weight.x, weight.y) |> 
  rename(weight_ref_counter = weight.x, weight_counter_demand = weight.y)

pos <- position_sankey(
  v_space=0.05, order="as_is", width=0.05, align="center"
)

sankey_data |> 
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
        "direct emissions", "embodied emissions", 
        "final demand"
      )
    ),
    node = fct_relevel(
      fct_reorder(node, node_size), "other industries", after=0
    )
  ) |> 
  arrange(node_size_to) |> 
  glimpse() |> 
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


ggsave("results/figures/sankey_industry.pdf", width=210, height=120, units="mm")


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
  separate_wider_regex(industry_ref_area, 
                       c(ref_industry = ".*", "_", ref_area = ".*")) |> 
  separate_wider_regex(industry_counterpart_area,
                       c(counter_industry = ".*", "_", counter_area = ".*")) |> 
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


demand <- emissions |> filter(time_period == "2021") |> 
  select( country,  matches("embodied_emissions_"))

names(demand) <- gsub("embodied_emissions_(P3_S13|P3_S14|P3_S15|P51G|P5M)_", "", names(demand))


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

sankey_data |> 
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
        "direct emissions", "embodied emissions", 
        "final demand"
      )
    ),
    node = fct_relevel(
      fct_reorder(node, node_size), "others", after=0
    )
  ) |> 
  arrange(node_size_to) |> 
  glimpse() |> 
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


ggsave("results/figures/sankey_country.pdf", width=210, height=120, units="mm")

# 
# 
# 
# setwd("~/Downloads/sankey_test/") 
# # pak::pkg_install("davidsjoberg/ggsankey") 
# library(tidyverse) 
# 
# library(ggsankey) font_fam <- "PT Serif" sysfonts::font_add_google(font_fam) showtext::showtext_auto() sankey_data <- read_rds("data.rds") sankey_data |> pivot_longer(c(ref_industry, counter_industry, demand_type)) pos <- position_sankey( v_space=0.05, order="as_is", width=0.05, align="center" ) sankey_data |> mutate( across(c(weight_ref_counter, weight_counter_demand), \(.x){.x/sum(.x)}) ) |> pivot_stages_longer( stages_from=c("ref_industry", "counter_industry", "demand_type"), values_from=c("weight_ref_counter", "weight_counter_demand") ) |> mutate( weight = case_when( stage[connector == "from"] == "ref_industry" ~ weight_ref_counter, stage[connector == "from"] == "counter_industry" ~ weight_counter_demand, .default=NA_real_ ), .by=edge_id ) |> mutate( node_size = c(sum(weight[connector == "from"]), sum(weight[connector == "to"])) |> discard(`==`, 0) |> first(), .by=node ) |> mutate( node_pretty = str_remove_all(node, " *\\(e\\.g\\..+?\\)"), node_label = str_c( node_pretty, ": ", scales::label_percent(accuracy=0.1)(node_size), " " ), stage = stage |> case_match( "ref_industry" ~ "direct emissions", "counter_industry" ~ "embodied emissions of final demand", "demand_type" ~ "type of final demand" ) ) |> mutate( node = fct_relevel( fct_reorder(node, node_size), "other industries", after=0 ) ) |> arrange(weight) |> glimpse() |> ggplot(aes( x=stage, y=weight, group=node, connector=connector, edge_id=edge_id )) + geom_sankeyedge(aes(fill=node), position=pos, alpha=0.6) + geom_sankeynode(fill="gray40", position=pos) + geom_text( aes(label=node_label), stat="sankeynode", position=pos, size=2.5, family=font_fam, lineheight = 0.5, hjust=1 ) + scale_fill_manual( values=c( "#555283", "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C" ) ) + scale_x_discrete(expand=expansion(c(0.35, 0.08))) + theme_void(base_family=font_fam) + theme( legend.position="none", plot.margin=unit(c(0, 2, 0, 2), "mm"), axis.text.x=element_text(size=8) ) ggsave("plot.pdf", width=210, height=120, units="mm")
# 
# 
# df <- sankey_data |> 
#   make_long(ref_industry, counter_industry, demand_type, value = weight_ref_counter) |> 
#   filter(x == "ref_industry") |> 
#   bind_rows(
#     sankey_data |> make_long(ref_industry, counter_industry, demand_type, value = weight_counter_demand) |> 
#       filter(x != "ref_industry")
#   ) |> 
#   distinct() |> 
#   mutate(x = recode(x,
#                     "ref_industry" = "direct emissions",
#                     "counter_industry" = "embodied emissions of final demand",
#                     "demand_type" = "type of final demand")) |> 
#   mutate(next_x = recode(next_x,
#                          "ref_industry" = "direct emissions",
#                          "counter_industry" = "embodied emissions of final demand",
#                          "demand_type" = "type of final demand")) |> 
#   mutate(label =
#       str_c("      ", node, ": ", scales::label_percent(accuracy=0.1)(sum(value) / total), "      "), .by = node
#   ) |>
#   mutate(
#     label_hjust = x |> case_match(
#       "direct emissions" ~ 1, 
#       "type of final demand" ~ 0, 
#       .default = 0.5
#     )
#   ) |> 
#   arrange(value)
# 
# 
# 
# 
# #  mutate(next_node  = str_c(next_node , ": ", scales::label_percent(accuracy=0.01)(value/(2140519 *2))))
# 
# 
# # Create the Sankey diagram
# ggplot(df , aes(x = x, 
#                next_x = next_x, 
#                node = node, 
#                next_node = next_node,
#                fill = factor(node),
#                value = value
#                )) +
#   geom_sankey(flow.alpha = 0.7, node.color = 1) +
#   geom_sankey_text(
#     aes(label = if_else(x != "embodied emissions of final demand",
#                         label,
#                         ""), hjust=label_hjust), family="serif", size=3
#   ) +
#   geom_sankey_label(
#     aes(label = if_else(x == "embodied emissions of final demand",
#                         label,
#                         ""),
# 
#         alpha = ifelse(x == "embodied emissions of final demand", 1, 0)),
#   
#     family="serif",
#     size=3,
#     label.size = 0,
#     color = "white"
#    # fill = "white"
# ) +
#   scale_fill_viridis_d() +
#   scale_x_discrete(expand=expansion(0.4)) +
#   theme_sankey(base_size = 16) +
#   guides(fill = guide_legend(title = "")) +
#   theme(
#     legend.position = "none",
#     axis.title = element_blank(),
#     text = element_text(family = "serif", size = 10),
#     # plot.margin= margin(0, 100, 0, 100),
#     
#     # Adjust the size as needed
#   )
# 
#   ggsave("sankey.pdf", width=30, height=10, units="cm")
# 
#   
#    #|> modify_if(
#   #     node == "digital share in other industries", 
#   #     \(.x){str_c(.x, "\n\n\n\n\n\n")}
#   #   ) |> modify_if(
#   #     node == "digital industries", 
#   #     \(.x){str_c( .x, "\n\n\n\n\n\n\n")}
#   #   ),
#   
