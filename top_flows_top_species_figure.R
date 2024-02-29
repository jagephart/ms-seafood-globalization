# Draft figure of species-level information
library(tidytext)

con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("DB_NAME"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("DB_USERNAME"),
                 password=Sys.getenv("DB_PASSWORD"))

artis <- dbGetQuery(con, "SELECT * FROM snet") %>%
  select(-record_id)

# Country metadata
country_metadata <- dbGetQuery(con, "SELECT * FROM countries") %>%
  select(-record_id)

# Sciname metadata
sciname_metadata <- dbGetQuery(con, "SELECT * FROM sciname") %>%
  select(-record_id)

# Code max resolved taxa
code_max_resolved_taxa <- dbGetQuery(con, "SELECT * FROM code_max_resolved_taxa") %>%
  select(-record_id) 

# Consumption
consumption <- dbGetQuery(con, "SELECT * FROM complete_consumption") %>%
  select(-record_id)

# Population 
pop <- dbGetQuery(con, "SELECT * FROM population") %>%
  select(-record_id)

# Close database connection
dbDisconnect(con)
rm(list = c("con"))

# Set colors
region6_palette <- c(
  "#741A32", # maroon
  "#B34232", # burnt orange
  "#D38F35", #  orange
  "#D4B95F", # khaki
  "#4FA2A2", # teal
  "#114F59" # dark teal
)

artis_palette <- colorRampPalette(region6_palette)

# FIX IT: Check on consumer NEI 
consumption_scaled <- consumption %>%
  left_join(pop, by = c("year", "consumer_iso3c" = "iso3c")) %>%
  mutate(consumption_kg_percap = 1000*consumption_live_t/pop) %>%
  group_by(year, consumer_iso3c) %>%
  mutate(total_consumption_kg_percap = sum(consumption_kg_percap)) %>% 
  ungroup() %>%
  mutate(scale_factor = consumption_kg_percap/total_consumption_kg_percap) %>%
  mutate(total_consumption_kg_percap = case_when(
    total_consumption_kg_percap > 100 ~ 100, 
    TRUE ~ total_consumption_kg_percap
    )) %>%
  mutate(consumption_kg_percap= total_consumption_kg_percap*scale_factor) %>%
  mutate(consumption_live_t = consumption_kg_percap*pop/1000) %>%
  # Join regions for consumer and source countries
  left_join(country_metadata %>%
              select(iso3c, "consumer_region" = "owid_region"), 
            by = c("consumer_iso3c" = "iso3c")) %>%
  left_join(country_metadata %>%
              select(iso3c, "source_region" = "owid_region"), 
            by = c("source_country_iso3c" = "iso3c")) %>%
  filter(consumer_region != "Other nei", source_region != "Other nei") %>%
  # Add common names associated with sciname_hs_modified names
  left_join(sciname_metadata %>% 
              select(sciname, common_name), 
            by = c("sciname_hs_modified" = "sciname")) 

# Foreign consumption and top flow map
# Marine capture
plot_df <- consumption_scaled %>%
  filter(year == 2019, 
         consumption_source == "foreign", 
         source_country_iso3c != "unknown", 
         habitat == "marine", 
         method == "capture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup() 

plot_df %>%
  slice_max(consumption_live_t, n = 10)

plot_df %>%
  rename("live_weight_t" = "consumption_live_t", 
         "importer_iso3c" = "consumer_iso3c", 
         "exporter_iso3c" = "source_country_iso3c") %>%
  plot_map(country_fill = "importer_iso3c", flow_arrows = TRUE)


# Marine aquaculture
plot_df <- consumption_scaled %>%
  filter(year == 2019, 
         consumption_source == "foreign", 
         source_country_iso3c != "unknown", 
         habitat == "marine", 
         method == "aquaculture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup() 

plot_df %>%
  slice_max(consumption_live_t, n = 10)

plot_df %>%
  rename("live_weight_t" = "consumption_live_t", 
         "importer_iso3c" = "consumer_iso3c", 
         "exporter_iso3c" = "source_country_iso3c") %>%
  plot_map(country_fill = "importer_iso3c", flow_arrows = TRUE)

# Inland capture
plot_df <- consumption_scaled %>%
  filter(year == 2019, 
         consumption_source == "foreign", 
         source_country_iso3c != "unknown", 
         habitat == "inland", 
         method == "capture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup() 

plot_df %>%
  slice_max(consumption_live_t, n = 10)

plot_df %>%
  rename("live_weight_t" = "consumption_live_t", 
         "importer_iso3c" = "consumer_iso3c", 
         "exporter_iso3c" = "source_country_iso3c") %>%
  plot_map(country_fill = "importer_iso3c", flow_arrows = TRUE)

# Inland aquaculture
plot_df <- consumption_scaled %>%
  filter(year == 2019, 
         consumption_source == "foreign", 
         source_country_iso3c != "unknown", 
         habitat == "inland", 
         method == "aquaculture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup() 

plot_df %>%
  slice_max(consumption_live_t, n = 10)

plot_df %>%
  rename("live_weight_t" = "consumption_live_t", 
         "importer_iso3c" = "consumer_iso3c", 
         "exporter_iso3c" = "source_country_iso3c") %>%
  plot_map(country_fill = "importer_iso3c", flow_arrows = TRUE)

# Top species traded
top_scinames <- consumption_scaled %>%
  filter(year == 2019, 
         consumption_source == "foreign", 
         source_country_iso3c != "unknown") %>%
  mutate(habita_method = paste(habitat, method, sep = " ")) %>%
  group_by(sciname_hs_modified, habita_method) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  group_by(habita_method) %>%
  slice_max(order_by = consumption_live_t, n = 5) %>%
  select(sciname_hs_modified, habita_method)


plot_df <- top_scinames %>% 
  left_join(consumption_scaled %>%
              filter(year == 2019, 
                     consumption_source == "foreign", 
                     source_country_iso3c != "unknown") %>%
              mutate(habita_method = paste(habitat, method, sep = " ")),
            by = c("sciname_hs_modified", "habita_method")) %>%
  rename("Consumer" = "consumer_region", "Source" = "source_region") %>%
  group_by(common_name, habita_method, Source, Consumer) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  pivot_longer(cols = Consumer:Source, names_to = "flow", values_to = "region") 

plot_df %>%
  filter(habita_method == "marine capture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total))) %>%
  ggplot(aes(x = consumption_live_t/1000000, y = flow, fill = region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region6_palette) +
  labs(x = "Foreign consumption (mil t, live weight)", y = "") +
  lims(x = c(0, 2.15)) +
  facet_grid(vars(common_name), switch = "y", 
             labeller = labeller(common_name = label_wrap_gen(10))) +
  theme_minimal() +
  theme(strip.placement = "outside", legend.position = "none")


plot_df %>%
  filter(habita_method == "marine aquaculture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total))) %>%
  ggplot(aes(x = consumption_live_t/1000000, y = flow, fill = region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region6_palette) +
  labs(x = "Foreign consumption (mil t, live weight)", y = "") +
  lims(x = c(0, 2.15)) +
  facet_grid(vars(common_name), switch = "y", 
             labeller = labeller(common_name = label_wrap_gen(10))) +
  theme_minimal() +
  theme(strip.placement = "outside", legend.position = "none")


plot_df %>%
  filter(habita_method == "inland capture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total))) %>%
  ggplot(aes(x = consumption_live_t/1000000, y = flow, fill = region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region6_palette) +
  labs(x = "Foreign consumption (mil t, live weight)", y = "") +
  lims(x = c(0, 2.15)) +
  facet_grid(vars(common_name), switch = "y", 
             labeller = labeller(common_name = label_wrap_gen(10))) +
  theme_minimal() +
  theme(strip.placement = "outside", legend.position = "none")

plot_df %>%
  filter(habita_method == "inland aquaculture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total))) %>%
  ggplot(aes(x = consumption_live_t/1000000, y = flow, fill = region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = region6_palette) +
  labs(x = "Foreign consumption (mil t, live weight)", y = "") +
  lims(x = c(0, 2.15)) +
  facet_grid(vars(common_name), switch = "y", 
             labeller = labeller(common_name = label_wrap_gen(10))) +
  theme_minimal() +
  theme(strip.placement = "outside", legend.position = "bottom")
