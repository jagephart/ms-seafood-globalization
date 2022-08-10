# Preliminary analysis

# Load packages
library(tidyverse)
library(countrycode)
library(circlize)
source("Oceana_report_functions/summarize_S_net_functions.R")

# Set directories 
datadir <-  "/Volumes/jgephart/ARTIS/Outputs"
outdir <- "/Volumes/jgephart/ARTIS/Outputs/Oceana Report"
S_net_file <- "S_net/2021-03-18_S-net_2016-2018_mean-quantity_mean-source.csv"
model_inputs_dir <- "/Volumes/jgephart/ARTIS/Outputs/model_inputs"

# Load data
S_net <- load_clean_dat(file.path(datadir, S_net_file)) 
S_net <- S_net %>%
  select(-X) %>%
  filter(quantity > 1)

prod_data <- read.csv(file.path(model_inputs_dir, "clean_fao_prod.csv"))

# Chord diagram for each production source/environment combination
region.colors <- c("#142B58", # Navy 
                   "#792560", # Purple
                   "#F35D2D", #Other option: "#13808F" # Teal
                   "#39A584", # Sea Green
                   "#C78F0B", # Gold
                   "#355936", # Forest Green
                   "#0686E5") # Light Blue "#00FFFF" # Cyan 
## All trade network
plot_df <- S_net %>%
  group_by(exporter_region, importer_region) %>%
  summarise(trade = sum(quantity)) 

plot_df <- abbrev_region(plot_df)

chordDiagram(plot_df,
             grid.col = region.colors,
             annotationTrack = c("name", "grid"),
             directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.target.prop = FALSE,
             diffHeight = 0.08,
             link.sort = TRUE)
title("All marine and inland trade")
circos.clear()

## Marine capture trade network
plot_df <- S_net %>%
  filter(prod_method == "Capture", habitat == "Marine") %>%
  group_by(exporter_region, importer_region) %>%
  summarise(trade = sum(quantity)) 

plot_df <- abbrev_region(plot_df)

chordDiagram(plot_df,
             grid.col = region.colors,
             annotationTrack = c("name", "grid"),
             directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.target.prop = FALSE,
             diffHeight = 0.08,
             link.sort = TRUE)
title("Marine capture trade")
circos.clear()

## Marine aquaculture trade network
plot_df <- S_net %>%
  filter(prod_method == "Aquaculture", habitat == "Marine") %>%
  group_by(exporter_region, importer_region) %>%
  summarise(trade = sum(quantity)) 

plot_df <- abbrev_region(plot_df)

chordDiagram(plot_df,
             grid.col = region.colors,
             annotationTrack = c("name", "grid"),
             directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.target.prop = FALSE,
             diffHeight = 0.08,
             link.sort = TRUE)
title("Marine aquaculture trade")
circos.clear()

## Inland capture trade network
plot_df <- S_net %>%
  filter(prod_method == "Capture", habitat == "Inland") %>%
  group_by(exporter_region, importer_region) %>%
  summarise(trade = sum(quantity)) 

plot_df <- abbrev_region(plot_df)

chordDiagram(plot_df,
             grid.col = region.colors,
             annotationTrack = c("name", "grid"),
             directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.target.prop = FALSE,
             diffHeight = 0.08,
             link.sort = TRUE)
title("Inland capture trade")
circos.clear()

## Inland aquaculture trade network
plot_df <- S_net %>%
  filter(prod_method == "Aquaculture", habitat == "Inland") %>%
  group_by(exporter_region, importer_region) %>%
  summarise(trade = sum(quantity)) 

plot_df <- abbrev_region(plot_df)

chordDiagram(plot_df,
             grid.col = region.colors,
             annotationTrack = c("name", "grid"),
             directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             link.target.prop = FALSE,
             diffHeight = 0.08,
             link.sort = TRUE)
title("Inland aquaculture trade")
circos.clear()

# Top traded species
plot_df <- S_net %>%
  group_by(common_name) %>%
  summarise(trade = sum(quantity)) %>% 
  filter(common_name != "ray-finned fishes") %>%
  arrange(trade) %>%    
  mutate(common_name=factor(common_name, levels=common_name)) %>%
  top_n(n = 20)

ggplot(plot_df, aes(x = common_name, y = trade/1000000)) +
  geom_segment(aes(xend=common_name, yend=0)) +
  geom_point( size=4, color="#39A584") +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("Total trade (million t)")

# Compare trade to production
prod_summary <- prod_data %>%
  filter(year == 2017) %>%
  mutate(prod_group = case_when(
    production_name_en %in% c("Aquaculture production (freshwater)",
                              "Aquaculture production (brackishwater)",
                              "Aquaculture production (marine)") ~ "Aquaculture_prod",
    production_name_en == "Capture production" ~ "Capture_prod"
  )) %>%
  group_by(country_iso3_alpha, prod_group) %>%
  summarise(prod_quant = sum(quantity)) %>% 
  pivot_wider(names_from = prod_group, values_from = prod_quant)

tmp <- S_net %>% 
  group_by(exporter, prod_method) %>% 
  summarise(quantity = sum(quantity)) %>% 
  arrange(desc(quantity)) %>% 
  pivot_wider(names_from = prod_method, values_from = quantity) %>%
  full_join(prod_summary, by = c("exporter" = "country_iso3_alpha")) %>% 
  mutate(prop_capture = Capture/Capture_prod, prop_aqua = Aquaculture/Aquaculture_prod)

