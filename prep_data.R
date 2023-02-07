
library(tidyverse)
library(countrycode)
library(circlize)
library(here)
library(ggpubr)
library(kableExtra)
library(DBI)
library(janitor)
library(viridis)
library(broom)
library(exploreARTIS)
library(vegan)
library(tidytext)

#-------------------------------------------------------------------------------
# Directory listing

outdir <- "/Volumes/jgephart/ARTIS/Outputs/ms_seafood_globalization/20220111"
outdir <- "outputs_20230206"

#-------------------------------------------------------------------------------
# Initial database pulls

# Database connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("POSTGRES_DB"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("POSTGRES_USER"),
                 password=Sys.getenv("POSTGRES_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# ARTIS dataframe
artis <- dbGetQuery(con, "SELECT * FROM snet") %>%
  select(-record_id) 

artis_fm <- artis %>% 
  filter(hs6 == "230120") %>% 
  group_by(year) %>%
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE),
            product_weight_t = sum(product_weight_t, na.rm = TRUE))

artis_nonfood <- artis %>% 
  # Keep only FMFO and ornamental species
  filter(hs6 %in% c("230120", "051191", "030110", "030111", "030119"))

artis <- artis %>% 
  # Remove FMFO and ornamental species
  filter(!(hs6 %in% c("230120", "051191", "030110", "030111", "030119")))

# Production dataframe
prod <- dbGetQuery(con, "SELECT * FROM production") %>%
  select(-record_id)

# Country metadata
country_metadata <- dbGetQuery(con, "SELECT * FROM countries") %>%
  select(-record_id)

# Code max resolved taxa
code_max_resolved_taxa <- dbGetQuery(con, "SELECT * FROM code_max_resolved_taxa") %>%
  select(-record_id) %>%
  mutate(hs6 = as.character(hs6)) %>%
  mutate(hs6 = case_when(
    str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
    TRUE ~ hs6
  ))

# Sciname metadata
sciname_metadata <- dbGetQuery(con, "SELECT * FROM sciname") %>%
  select(-record_id)

# Close database connection
dbDisconnect(con)
rm(list = c("con"))

# Population data
pop <- read.csv("/Volumes/jgephart/ARTIS/Outputs/clean_metadata/fao_annual_pop.csv") %>% 
  left_join(country_metadata, by = c("iso3c")) %>%
  select(iso3c, year, "region" = "owid_region", pop)

# write.csv(artis, file.path(outdir, "artis.csv"), row.names = FALSE)
write.csv(prod, file.path(outdir, "prod.csv"), row.names = FALSE)

# Write out the fishmeal trade data, aggregated by year, for summary stats
write.csv(artis_fm, file.path(outdir, "artis_fm.csv"), row.names = FALSE)

# Write out the bilater non-food trade data to use in supply calculations
write.csv(artis_nonfood, file.path(outdir, "artis_fm.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Summarize cleaned FAO production data
prod <- prod %>% 
  select("year", "iso3c", "sciname", 
         "method", "habitat", 
         "production_t" = "live_weight_t") %>%
  group_by(year, iso3c, sciname, method, habitat) %>%
  summarise(production_t = sum(production_t, na.rm = TRUE)) %>%
  ungroup()

# Join production data to ARTIS
artis <- artis %>%
  filter(live_weight_t >= 0.1) %>%
  # Add importer and exporter region
  left_join(country_metadata %>% 
              select("exporter_iso3c" = "iso3c", "exporter_region" = "owid_region"), by = c("exporter_iso3c")) %>%
  left_join(country_metadata %>% 
              select("importer_iso3c" = "iso3c", "importer_region" = "owid_region"), by = c("importer_iso3c")) %>%
  # FIX IT: Determine if this should be deleted. Production of only species exports may not be relevant
  left_join(prod, by = c("year", "exporter_iso3c" = "iso3c", 
                         "sciname", "method", "habitat")) %>%
  ungroup()

# Update artis data to lowest resolution by comparing code taxa_level and prod taxa_level
artis <- artis %>%
  left_join(code_max_resolved_taxa,
            by = c("hs_version", "hs6", "sciname")) %>%
  left_join(sciname_metadata, 
            by = "sciname") %>%
  # Leave any missing sciname_hs_modified as original sciname
  mutate(sciname_hs_modified = case_when(
    is.na(sciname_hs_modified) ~ sciname,
    TRUE ~ sciname_hs_modified
  )) %>%
  rename(environment = habitat) %>%
  # Add habitat-method column
  mutate(habitat_method = paste(environment, method, sep =" ")) %>% 
  mutate(habitat_method = case_when(
    str_detect(habitat_method, "unknown") ~ "unknown", 
    TRUE ~ habitat_method
  )) %>% 
  # Set factor levels
  mutate(habitat_method = factor(habitat_method, levels = c("marine capture", "inland capture",
                                                            "marine aquaculture", "inland aquaculture",
                                                            "unknown" )))

# Remove data frames after merged with ARTIS
rm(list = c("code_max_resolved_taxa", "sciname_metadata"))

#-------------------------------------------------------------------------------
# Summaries for results and figures

# Figure 1

# Figure 1a
global_export_by_source <- artis %>% 
  mutate(food_or_fmfo = case_when(
    hs6 == "230120" ~ "fishmeal",
    hs6 != "230120" ~ "nonfishmeal"
  )) %>%
  group_by(year, habitat_method, food_or_fmfo) %>%
  summarise(live_weight_t = sum(live_weight_t)) 

write.csv(global_export_by_source, file.path(outdir, "global_export_by_source.csv"), row.names = FALSE)

fig_1a <- global_export_by_source %>%
  group_by(year, habitat_method) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ungroup()

write.csv(fig_1a, file.path(outdir, "fig_1a_data.csv"), row.names = FALSE)

# Figure 1b
# Percent of Marine and Freshwater aquaculture and fishery production excluding re exports (only domestic exports)

prod_by_source <- prod %>%
  group_by(year, method, habitat) %>% 
  summarise(production_t = sum(production_t)) %>%
  mutate(habitat_method = paste(habitat, method, sep = " ")) %>%
  # Set factor levels
  mutate(habitat_method = factor(habitat_method, 
                                 levels = c("marine capture", "inland capture",
                                            "marine aquaculture", "inland aquaculture"))) %>%
  ungroup() %>%
  select(-habitat, -method)

write.csv(prod_by_source, file.path(outdir, "prod_by_habitat_method.csv"), row.names = FALSE)

dom_export_by_source <- artis %>% 
  filter(dom_source == "domestic export") %>%
  group_by(year, habitat_method, environment) %>%
  summarise(dom_live_weight_t = sum(live_weight_t))

write.csv(dom_export_by_source, file.path(outdir, "domestic_export_by_habitat_method.csv"), row.names = FALSE)

percent_export_by_source <- dom_export_by_source %>%
  left_join(prod_by_source, by = c("year", "habitat_method")) %>% 
  mutate(percent_export = 100 * dom_live_weight_t / production_t)

write.csv(percent_export_by_source, file.path(outdir, "domestic_export_percent_production_by_habitat_method.csv"), row.names = FALSE)
write.csv(percent_export_by_source, file.path(outdir, "fig_1b_data.csv"), row.names = FALSE)

percent_export_total <- dom_export_by_source %>%
  left_join(prod_by_source, by = c("year", "habitat_method")) %>% 
  group_by(year) %>% 
  summarise(dom_live_weight_t = sum(dom_live_weight_t), 
            production_t = sum(production_t)) %>%
  mutate(percent_export = 100 * dom_live_weight_t / production_t) %>% 
  mutate(calc_type = "domestic exports") %>% 
  select(year, percent_export, calc_type)

percent_export_total_all_source <- artis %>% 
  group_by(year) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  left_join(prod_by_source %>% 
              group_by(year) %>% 
              summarise(production_t = sum(production_t)), 
            by = c("year")) %>% 
  mutate(percent_export = 100 * live_weight_t / production_t) %>% 
  mutate(calc_type = "all export sources") %>% 
  select(year, percent_export, calc_type)

percent_export_total <- percent_export_total %>%
  bind_rows(percent_export_total_all_source)

write.csv(percent_export_total, file.path(outdir, "total_export_percent_prod.csv"), row.names = FALSE)

# Figure 1c
# Out and In degree summary
all_degree_summary <-  artis %>%
  # Getting distinct bilateral trade records
  select(year, exporter_iso3c, importer_iso3c) %>%
  distinct() %>%
  # Calculate OUT degree of trade
  group_by(year, exporter_iso3c) %>%
  tally() %>% 
  rename(all_out_degree = n, iso3c = exporter_iso3c) %>%
  # Calculate and join IN degree of trade
  full_join(artis %>% 
              select(year, exporter_iso3c, importer_iso3c) %>%
              distinct() %>%
              group_by(year, importer_iso3c) %>%
              tally() %>%
              rename(all_in_degree = n, iso3c = importer_iso3c), 
            by = c("year", "iso3c"))

write.csv(all_degree_summary, file.path(outdir, "all_degree_summary.csv"), row.names = FALSE)

# Out and In degree summary by habitat and method
habitat_method_degree_summary <-  artis %>%
  # Getting distinct bilateral trade by habitat and method
  select(year, exporter_iso3c, importer_iso3c, habitat_method) %>%
  distinct() %>%
  # Calculate OUT degree by habitat and method
  group_by(year, exporter_iso3c, habitat_method) %>%
  tally() %>% 
  rename(all_out_degree = n, iso3c = exporter_iso3c) %>%
  # Calculate and join IN degree by habitat and method
  full_join(artis %>% 
              select(year, exporter_iso3c, importer_iso3c, habitat_method) %>%
              distinct() %>%
              group_by(year, importer_iso3c, habitat_method) %>%
              tally() %>%
              rename(all_in_degree = n, iso3c = importer_iso3c), 
            by = c("year", "iso3c", "habitat_method"))

write.csv(habitat_method_degree_summary, file.path(outdir, "habitat_method_degree_summary.csv"), row.names = FALSE)
write.csv(habitat_method_degree_summary, file.path(outdir, "fig_1c_data.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Figure 2

# Fig 2b
# Matrix of trade between regions
artis_region_method <- artis %>%
  filter(!is.na(importer_region), !is.na(exporter_region), 
         importer_region != "Other nei", exporter_region != "Other nei") %>%
  group_by(year, exporter_region, importer_region, habitat_method) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_importer = paste(exporter_region, " to ", importer_region, sep = ""))

write.csv(artis_region_method, file.path(outdir, "artis_region_method.csv"), row.names = FALSE)

# Fig 2a and 2c
# Will create Importers by Region and Exporters by Region plots
artis_region <- artis_region_method %>%
  group_by(year, exporter_region, importer_region) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_importer = paste(exporter_region, " to ", importer_region, sep = ""))

write.csv(artis_region, file.path(outdir, "artis_region.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Figure 3

# Create calculate_supply function (small difference from version in explore_artis)
calculate_supply <- function(artis_data, production_data){
  exports <- artis_data %>%
    group_by(year, exporter_iso3c, dom_source, sciname, method, habitat) %>%
    summarise(live_weight_t = sum(live_weight_t)) %>%
    mutate(dom_source = str_replace(dom_source, " ", "_")) %>%
    pivot_wider(names_from = "dom_source", values_from = "live_weight_t")
  
  if (!("error_export" %in% colnames(exports))) {
    exports$error_export <- NA
  }
  
  imports <- artis_data %>%
    group_by(year, importer_iso3c, sciname, method, habitat) %>%
    summarise(imports_live_weight_t = sum(live_weight_t))
  
  supply <- exports %>%
    full_join(imports, by = c("year", "exporter_iso3c" = "importer_iso3c", 
                              "sciname", "method", "habitat")) %>%
    full_join(production_data %>% 
                rename(production_t = live_weight_t), by = c("year", "exporter_iso3c" = "iso3c", 
                                                             "sciname", "method", "habitat")) 
  
  supply$foreign_export[is.na(supply$foreign_export)] <- 0
  supply$domestic_export[is.na(supply$domestic_export)] <- 0
  supply$error_export[is.na(supply$error_export)] <- 0
  supply$imports_live_weight_t[is.na(supply$imports_live_weight_t)] <- 0
  supply$production_t[is.na(supply$production_t)] <- 0
  
  supply <- supply %>%
    mutate(supply = production_t + imports_live_weight_t - domestic_export - foreign_export - error_export,
           supply_no_error = production_t + imports_live_weight_t - domestic_export - foreign_export, 
           supply_domestic = production_t - domestic_export,
           supply_foreign = imports_live_weight_t - foreign_export) %>%
    rename("iso3c" = "exporter_iso3c") %>%
    ungroup() %>%
    mutate(supply_no_error = replace_na(supply_no_error, 0),
           supply_domestic = replace_na(supply_domestic, 0),
           supply_foreign = replace_na(supply_foreign, 0))
  
  return(supply)
}

# Supply / Consumption data
consumption_dir <- "/Volumes/jgephart/ARTIS/Outputs/consumption/consumption_max_per_capita_100kg"
consumption_dir <- "../ARTIS/consumption/consumption_max_per_capita_100kg"
supply <- read.csv(file.path(consumption_dir, "summary_consumption.csv"))

supply <- supply %>%
  filter(!is.na(habitat) & !is.na(method)) %>%
  mutate(habitat_method = paste(habitat, method, sep = " ")) %>%
  mutate(habitat_method = case_when(
    str_detect(habitat_method, "unknown") ~ "unknown", 
    TRUE ~ habitat_method
  )) %>%
  # Set factor levels
  mutate(habitat_method = factor(habitat_method, levels = c("marine capture", "inland capture",
                                                            "marine aquaculture", "inland aquaculture",
                                                            "unknown" ))) %>%
  rename(supply_domestic = domestic_consumption_t,
         supply_foreign = foreign_consumption_t)

supply <- supply %>%
  left_join(
    pop,
    by = c("iso3c", "year")
  )

global_pop <- pop %>%
  filter(region != "Other nei") %>%
  group_by(year) %>%
  summarize(pop = sum(pop, na.rm = TRUE)) %>%
  ungroup()

global_supply_per_cap <- supply %>%
  group_by(year) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  left_join(
    global_pop,
    by = c("year")
  ) %>%
  mutate(per_cap = 1000 * supply / pop)

# Figure 3a
# Calculate per capita supply by region and source
supply_total <- supply %>%
  filter(!is.na(habitat_method)) %>%
  group_by(iso3c, region, year, habitat_method) %>%
  summarise(supply = sum(supply),
            supply_domestic = sum(supply_domestic),
            supply_foreign = sum(supply_foreign),
            pop = sum(pop)
  ) %>%
  ungroup()

fig3a_data <- supply_total %>%
  filter(!is.na(habitat_method)) %>%
  filter(region != "Other nei") %>%
  group_by(year, habitat_method) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  left_join(
    global_pop %>%
      rename(year_global_pop = pop),
    by = c("year")
  ) %>%
  mutate(supply_per_cap = 1000 * supply / year_global_pop)

write.csv(fig3a_data, file.path(outdir, "fig3a_data.csv"), row.names = FALSE)

# Figure 3b
# fig3b_data <- supply_total %>%
#   filter(region != "Other nei") %>%
#   group_by(year, region, habitat_method) %>%
#   summarise(supply_per_cap = 1000 * (sum(supply, na.rm = TRUE)) / sum(pop, na.rm = TRUE))

regional_pop <- pop %>%
  group_by(region, year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

fig3b_data <- supply %>%
  filter(region != "Other nei") %>%
  group_by(year, region, habitat_method) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  left_join(
    regional_pop,
    by = c("region", "year")
  ) %>%
  mutate(supply_per_cap = 1000 * supply / pop)

write.csv(fig3b_data, file.path(outdir, "fig3b_data.csv"), row.names = FALSE)

# Figure 3c
# Global percent of supply that is domestic/foreign
fig3c_data <- supply %>%
  filter(region != "Other nei") %>%
  group_by(year) %>%
  summarise(supply_domestic = 100 * sum(supply_domestic) / sum(supply),
            supply_foreign = 100 * sum(supply_foreign) / sum(supply)) %>%
  pivot_longer(cols = supply_domestic:supply_foreign, 
               names_to = "supply_source", values_to = "supply_percent") %>%
  mutate(supply_source = gsub("supply_", "", supply_source))

write.csv(fig3c_data, file.path(outdir, "fig3c_data.csv"), row.names = FALSE)

# Figure 3d
# Regional percent of supply that is domestic/foreign
fig3d_data <- supply %>% 
  filter(region != "Other nei") %>%
  group_by(region, year) %>%
  summarise(supply_domestic = 100*sum(supply_domestic)/sum(supply),
            supply_foreign = 100*sum(supply_foreign)/sum(supply)) %>%
  pivot_longer(cols = supply_domestic:supply_foreign, 
               names_to = "supply_source", values_to = "supply_percent") %>%
  mutate(supply_source = gsub("supply_", "", supply_source))

write.csv(fig3d_data, file.path(outdir, "fig3d_data.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Figure 4
# Relationship between trade and supply diversity
diversity_dir <- "/Volumes/jgephart/ARTIS/Outputs/diversity/max_per_capita_100kg"
diversity <- read.csv(file.path(diversity_dir, "diversity.csv"))

# Figure 4a
# Time series of the Shannon diversity of imported blue foods and produced blue foods by region.
fig4a_data <- diversity %>%
  filter(region != "Other nei") %>%
  group_by(year, region) %>%
  summarise(shannon_prod = mean(shannon_prod), 
            shannon_imports = mean(shannon_imports)) %>%
  pivot_longer(cols = shannon_prod:shannon_imports,
               names_to = "source", values_to = "shannon") %>%
  mutate(source = case_when(
    source == "shannon_prod" ~ "Production diversity",
    source == "shannon_imports" ~ "Import diversity"
  ))

write.csv(fig4a_data, file.path(outdir, "fig4a_data.csv"), row.names = FALSE)


# Figure 4b
# Foreign supply Shannon diversity versus domestic supply Shannon diversity for 2018.
# The black line represents the 1-to-1 line, such that points falling below the line 
# indicate countries where the domestic supply diversity is greater than the foreign
# supply diversity.
fig4b_data <- diversity %>%
  filter(year == 2018, region != "Other nei")

write.csv(fig4b_data, file.path(outdir, "fig4b_data.csv"), row.names = FALSE)

# Figure 4c
# Relationship between total imports and overall supply diversity for 2018.
fig4c_data <- diversity %>%
  filter(year == 2018, region != "Other nei")

write.csv(fig4c_data, file.path(outdir, "fig4c_data.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# SUPPLEMENTARY FIGURES
#-------------------------------------------------------------------------------

regional_artis <- artis %>%
  rename(habitat = environment) %>%
  group_by(exporter_iso3c, importer_iso3c, habitat, method, year) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(
    country_metadata %>%
      select(exporter_iso3c = iso3c, exporter_region = owid_region),
    by = c("exporter_iso3c")
  ) %>%
  left_join(
    country_metadata %>%
      select(importer_iso3c = iso3c, importer_region = owid_region),
    by = c("importer_iso3c")
  ) %>%
  group_by(exporter_region, importer_region, habitat, method, year) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  ungroup()

write.csv(regional_artis, file.path(outdir, "regional_artis_by_source.csv"), row.names = FALSE)

# Top Trading Partners old (1996 - 2000) vs recent (2016 - 2020)
# Summarize total imports and exports by habitat and environment
bilateral_habitat_method_summary <- artis %>%
  # Remove if we decide to leave FMFO in
  filter(hs6 != "230120") %>%
  group_by(year, exporter_iso3c, exporter_region, importer_iso3c, importer_region, habitat_method) %>%
  summarise(live_weight_t = sum(live_weight_t))

write.csv(bilateral_habitat_method_summary, file.path(outdir, "bilateral_habitat_method_summary.csv"), row.names = FALSE)

# Calculate percent of pairs with increased trade flows
bilateral_flow_change <- bilateral_habitat_method_summary %>% 
  filter(exporter_region != "Other nei", importer_region != "Other nei",
         year %in% c(1996:2000, 2016:2020)) %>%
  mutate(year_group = case_when(
    year %in% 1996:2000 ~ "beginning", 
    year %in% 2016:2020 ~ "end")) %>%
  group_by(year_group, exporter_iso3c, importer_iso3c) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  pivot_wider(names_from = year_group, values_from = live_weight_t, values_fill = 0) %>% 
  mutate(change = end-beginning) %>%
  arrange(desc(change))

write.csv(bilateral_flow_change, file.path(outdir, "bilateral_flow_change.csv"), row.names = FALSE)

# Top Exporters by habitat and production method
# Year groups 1996 - 2000 and 2016 - 2020
top_exporters_old <- bilateral_habitat_method_summary %>%
  filter(year >= 1996 & year <= 2000 & habitat_method != "unknown") %>%
  group_by(exporter_iso3c, habitat_method) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2000 - 1996)) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_iso3c = reorder_within(exporter_iso3c, ranking, habitat_method))

top_exporters_recent <- bilateral_habitat_method_summary %>%
  filter(year >= 2016 & year <= 2020 & habitat_method != "unknown") %>%
  group_by(exporter_iso3c, habitat_method) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2020 - 2016)) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_iso3c = reorder_within(exporter_iso3c, ranking, habitat_method))

write.csv(top_exporters_old, file.path(outdir, "top_exporters_old.csv"), row.names = FALSE)
write.csv(top_exporters_recent, file.path(outdir, "top_exporters_recent.csv"), row.names = FALSE)

# Top Importers by habitat and production method
# Year groups 1996 - 2000 and 2016 - 2020
top_importers_old <- bilateral_habitat_method_summary %>%
  filter(year >= 1996 & year <= 2000 & habitat_method != "unknown") %>%
  group_by(importer_iso3c, habitat_method) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2000 - 1996)) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(importer_iso3c = reorder_within(importer_iso3c, ranking, habitat_method))

top_importers_recent <- bilateral_habitat_method_summary %>%
  filter(year >= 2016 & year <= 2020 & habitat_method != "unknown") %>%
  group_by(importer_iso3c, habitat_method) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2020 - 2016)) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(importer_iso3c = reorder_within(importer_iso3c, ranking, habitat_method))

write.csv(top_importers_old, file.path(outdir, "top_importers_old.csv"), row.names = FALSE)
write.csv(top_importers_recent, file.path(outdir, "top_importers_recent.csv"), row.names = FALSE)
#-------------------------------------------------------------------------------
# Regional Export and Import changes from 1996-2000 vs 2016-2020

# Top Regional Export changes
top_regional_export_change <- bilateral_habitat_method_summary %>% 
  filter(year %in% c(1996:2000, 2016:2020)) %>%
  mutate(year_group = case_when(
    year %in% 1996:2000 ~ "beginning", 
    year %in% 2016:2020 ~ "end")) %>%
  group_by(year_group, exporter_region, habitat_method) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  pivot_wider(names_from = year_group, values_from = live_weight_t) %>% 
  mutate(change = end-beginning) %>% 
  arrange(desc(change)) %>% 
  filter(habitat_method != "unknown") %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = change) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup() %>%
  mutate(exporter_region = reorder_within(exporter_region, ranking, habitat_method))

write.csv(top_regional_export_change, file.path(outdir, "top_regional_export_change.csv"), row.names = FALSE)

# Top Regional Import changes
top_regional_import_change <- bilateral_habitat_method_summary %>% 
  filter(year %in% c(1996:2000, 2016:2020)) %>%
  mutate(year_group = case_when(
    year %in% 1996:2000 ~ "beginning", 
    year %in% 2016:2020 ~ "end")) %>%
  group_by(year_group, importer_region, habitat_method) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  pivot_wider(names_from = year_group, values_from = live_weight_t) %>% 
  mutate(change = end-beginning) %>% 
  arrange(desc(change)) %>%
  filter(habitat_method != "unknown") %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = change) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup() %>%
  mutate(importer_region = reorder_within(importer_region, change, habitat_method))

write.csv(top_regional_import_change, file.path(outdir, "top_regional_import_change.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Top Export increase and decreases

top_exporter_change <- bilateral_habitat_method_summary %>% 
  filter(year %in% c(1996:2000, 2016:2020)) %>%
  mutate(year_group = case_when(
    year %in% 1996:2000 ~ "beginning", 
    year %in% 2016:2020 ~ "end")) %>%
  group_by(year_group, exporter_iso3c, habitat_method) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  pivot_wider(names_from = year_group, values_from = live_weight_t) %>% 
  mutate(change = end-beginning) %>% 
  arrange(desc(change)) %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup()

write.csv(top_exporter_change, file.path(outdir, "top_exporter_change.csv"), row.names = FALSE)

top_export_increases <- top_exporter_change %>% 
  filter(habitat_method != "unknown") %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = change) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup() %>%
  mutate(exporter_iso3c = reorder_within(exporter_iso3c, change, habitat_method))

write.csv(top_export_increases, file.path(outdir, "top_export_increases.csv"), row.names = FALSE)

top_export_decreases <- top_exporter_change %>% 
  filter(habitat_method != "unknown") %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  slice_min(n = 10, order_by = change) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup() %>%
  mutate(exporter_iso3c = reorder_within(exporter_iso3c, change, habitat_method))

write.csv(top_export_decreases, file.path(outdir, "top_export_decreases.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Top Import increase and decreases

top_importer_change <- bilateral_habitat_method_summary %>% 
  filter(year %in% c(1996:2000, 2016:2020)) %>%
  mutate(year_group = case_when(
    year %in% 1996:2000 ~ "beginning", 
    year %in% 2016:2020 ~ "end")) %>%
  group_by(year_group, importer_iso3c, habitat_method) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  pivot_wider(names_from = year_group, values_from = live_weight_t) %>% 
  mutate(change = end-beginning) %>% 
  arrange(desc(change)) %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup()

write.csv(top_importer_change, file.path(outdir, "top_importer_change.csv"), row.names = FALSE)

top_import_increases <- top_importer_change %>%
  filter(habitat_method != "unknown") %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = change) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup() %>%
  mutate(importer_iso3c = reorder_within(importer_iso3c, change, habitat_method))

write.csv(top_import_increases, file.path(outdir, "top_import_increases.csv"), row.names = FALSE)

top_import_decreases <- top_importer_change %>%
  filter(habitat_method != "unknown") %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  slice_min(n = 10, order_by = change) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(change)) %>%
  ungroup() %>%
  mutate(importer_iso3c = reorder_within(importer_iso3c, change, habitat_method))

write.csv(top_import_decreases, file.path(outdir, "top_import_decreases.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Average Annual Change in Export (1000 t live weight)

artis_region <- artis_region_method %>%
  group_by(year, exporter_region, importer_region) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_importer = paste(exporter_region, " to ", importer_region, sep = ""))

region_slopes <- artis_region %>%
  split(.$exporter_importer) %>% 
  map(~lm(live_weight_t~year, data = .x)) %>% 
  map_df(broom::tidy, .id = 'exporter_importer') %>%
  filter(term == 'year') %>%
  select(exporter_importer, "slope" = "estimate")

artis_region <- artis_region %>%
  left_join(region_slopes, by = "exporter_importer") %>% 
  mutate(intraregion = case_when(
    exporter_region == importer_region ~ "intraregional trade",
    exporter_region != importer_region ~ "interegional trade"))

regional_avg_annual_change <- artis_region %>%
  select(-year, -live_weight_t) %>%
  distinct() %>%
  mutate(exporter_importer = fct_reorder(exporter_importer, slope))

write.csv(regional_avg_annual_change, file.path(outdir, "regional_avg_annual_change.csv"), row.names = FALSE)
#-------------------------------------------------------------------------------
# Net exports from least developed countries as defined by the United Nations
# LDC countries according to UN (https://www.un.org/development/desa/dpad/least-developed-country-category/ldcs-at-a-glance.html)
LDC_countries <- c("Afghanistan", "Angola", "Bangladesh", "Benin", "Bhutan",
                   "Burkina Faso", "Burundi", "Cambodia", "Central African Republic",
                   "Chad", "Comoros", "Democratic Republic of the Congo", "Djibouti",
                   "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Haiti",
                   "Kiribati", "Lao People’s Dem. Republic", "Lesotho", "Liberia",
                   "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique",
                   "Myanmar", "Nepal", "Niger", "Rwanda", "Sao Tome and Principe",
                   "Senegal", "Sierra Leone", "Solomon Islands", "Somalia",
                   "South Sudan", "Sudan", "Timor-Leste", "Togo", "Tuvalu",
                   "Uganda", "United Republic of Tanzania", "Yemen", "Zambia")

LDC_countries <- countrycode(LDC_countries, origin = "country.name", destination = "iso3c")

ldc_habitat_method_summary <- bilateral_habitat_method_summary %>%
  mutate(
    importer_ldc = case_when(
      (importer_iso3c %in% LDC_countries) ~ "LDC",
      !(importer_iso3c %in% LDC_countries) ~ "non-LDC"),
    exporter_ldc = case_when(
      (exporter_iso3c %in% LDC_countries) ~ "LDC",
      !(exporter_iso3c %in% LDC_countries) ~ "non-LDC")) %>%
  group_by(year, exporter_ldc, importer_ldc, habitat_method) %>%
  summarise(live_weight_t = sum(live_weight_t))

ldc_net_habitat_method_summary <- ldc_habitat_method_summary %>%
  ungroup() %>% 
  filter(importer_ldc == "LDC", exporter_ldc == "non-LDC") %>% 
  select(year, habitat_method, "ldc_import" = "live_weight_t") %>%
  left_join(ldc_habitat_method_summary %>%
              ungroup() %>%
              filter(importer_ldc == "non-LDC", exporter_ldc == "LDC") %>% 
              select(year, habitat_method, "ldc_export" = "live_weight_t"),
            by = c("year", "habitat_method")) %>%
  mutate(net_import = ldc_import-ldc_export,
         net_export = ldc_export-ldc_import) %>%
  filter(habitat_method != "unknown")

write.csv(ldc_net_habitat_method_summary, file.path(outdir, "ldc_net_habitat_method_summary.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Concentration of trade by production source
# measured as the number of countries comprising 75%

# Set percent threshold
cumulative_percent_threshold <- 75

# Export concentration
export_concentration_habitat_method <- bilateral_habitat_method_summary %>%
  group_by(year, habitat_method, exporter_iso3c) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(year, habitat_method, desc(live_weight_t)) %>%
  mutate(cumulative_percent = 100 * cumsum(live_weight_t) / sum(live_weight_t))

write.csv(export_concentration_habitat_method, file.path(outdir, "export_concentration.csv"), row.names = FALSE)

export_concentration_n_countries <- bilateral_habitat_method_summary %>%
  group_by(year, exporter_iso3c) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(year, desc(live_weight_t)) %>%
  mutate(cumulative_percent = 100*cumsum(live_weight_t)/sum(live_weight_t)) %>%
  filter(cumulative_percent < cumulative_percent_threshold) %>%
  group_by(year) %>% 
  tally()

write.csv(export_concentration_n_countries, file.path(outdir, "export_concentration_n_countries.csv"), row.names = FALSE)

export_concentration_habitat_method_n_countries <- export_concentration_habitat_method %>%
  filter(cumulative_percent < cumulative_percent_threshold) %>%
  group_by(year, habitat_method) %>% 
  tally() %>%
  filter(habitat_method != "unknown")

write.csv(export_concentration_habitat_method_n_countries, file.path(outdir, "export_concentration_n_countries_by_source.csv"), row.names = FALSE)

# Import concentration
import_concentration_habitat_method <- bilateral_habitat_method_summary %>%
  group_by(year, habitat_method, importer_iso3c) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(year, habitat_method, desc(live_weight_t)) %>%
  mutate(cumulative_percent = 100*cumsum(live_weight_t)/sum(live_weight_t))

write.csv(import_concentration_habitat_method, file.path(outdir, "import_concentration.csv"), row.names = FALSE)

import_concentration_n_countries <- bilateral_habitat_method_summary %>%
  group_by(year, importer_iso3c) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  arrange(year, desc(live_weight_t)) %>%
  mutate(cumulative_percent = 100*cumsum(live_weight_t)/sum(live_weight_t)) %>%
  filter(cumulative_percent < cumulative_percent_threshold) %>%
  group_by(year) %>% 
  tally()

write.csv(import_concentration_n_countries, file.path(outdir, "import_concentration_n_countries.csv"), row.names = FALSE)

import_concentration_habitat_method_n_countries <- import_concentration_habitat_method %>%
  filter(cumulative_percent < cumulative_percent_threshold) %>%
  group_by(year, habitat_method) %>% 
  tally() %>%
  filter(habitat_method != "unknown")

write.csv(import_concentration_habitat_method_n_countries, file.path(outdir, "import_concentration_n_countries_by_source.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Custom ARTIS timeseries (based on different HS versions used)

hs_version_datadir <- "/Volumes/jgephart/ARTIS/Outputs/S_net/snet_20221129/snet"

# Based on snet midpoint estimation
hs_versions <- c("96", "02", "07", "12", "17")

artis_ts <- data.frame()

for (i in 1:length(hs_versions)) {
  curr_hs <- hs_versions[i]
  print(curr_hs)
  curr_fp <- file.path(hs_version_datadir, paste("HS", curr_hs, "/midpoint_artis_habitat_prod_ts_HS", curr_hs, ".csv", sep = ""))
  print(curr_fp)
  curr_artis <- read.csv(curr_fp) %>%
    mutate(hs_version = paste("HS", curr_hs, sep = "")) %>%
    group_by(year, hs_version) %>%
    summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup()

  artis_ts <- artis_ts %>%
    bind_rows(curr_artis)
}

custom_ts <- artis_ts %>%
  filter(
    # Use HS96 from 1996-2003 (inclusive)
    ((hs_version == "HS96") & (year <= 2003)) |
      # Use HS02 from 2004-2009 (inclusive)
      ((hs_version == "HS02") & (year >= 2004 & year <= 2009)) |
      # Use HS07 from 2010-2012 (inclusive)
      ((hs_version == "HS07") & (year >= 2010 & year <= 2012)) |
      # Use HS12 from 2013-2019 (inclusive)
      ((hs_version == "HS12") & (year >= 2013 & year <= 2020))
  ) %>%
  mutate(hs_version = "Custom ARTIS Time series")

artis_ts <- artis_ts %>%
  bind_rows(custom_ts)

write.csv(artis_ts, file.path(outdir, "artis_ts.csv"), row.names = FALSE)
write.csv(custom_ts, file.path(outdir, "custom_ts.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# how many countries report with at least 75% true species

# Producers species_level production in the form of true species
producer_species_level_percent <- prod %>%
  ungroup() %>%
  group_by(iso3c, year, sciname) %>%
  summarize(production_t = sum(production_t, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(species_level = case_when(
    str_detect(sciname, " ") ~ "true_species",
    TRUE ~ "other"
  )) %>%
  group_by(iso3c, year, species_level) %>%
  summarize(production_t = sum(production_t)) %>%
  ungroup() %>%
  group_by(iso3c, year) %>%
  mutate(total_production_t = sum(production_t)) %>%
  ungroup() %>%
  mutate(percent_production = 100 * production_t / total_production_t)

percent_true_species_cutoff <- 75

# Number of producers with at least 75% production in true species
true_species_reporters <- producer_species_level_percent %>%
  group_by(year, iso3c) %>%
  filter(percent_production == max(percent_production)) %>%
  ungroup() %>%
  mutate(producer_category = case_when(
    species_level == "true_species" & percent_production >= percent_true_species_cutoff ~ "true_species_producer",
    TRUE ~ "other_producer"
  )) %>%
  group_by(year, producer_category) %>%
  tally() %>%
  ungroup() %>%
  mutate(producer_category = str_remove(producer_category, "_producer")) %>%
  mutate(producer_category = case_when(
    producer_category == "other" ~ "Other",
    producer_category == "true_species" ~ "True Species",
    TRUE ~ producer_category
  ))


true_species_reporters %>%
  ggplot(aes(x = year, y = n, color = producer_category)) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Number of Producers", color = "Producer Category")

#-------------------------------------------------------------------------------
# Diversity measures for countries that report at least 75% of their production as true species

true_species_producers <- producer_species_level_percent %>%
  group_by(year, iso3c) %>%
  filter(percent_production == max(percent_production)) %>%
  ungroup() %>%
  mutate(producer_category = case_when(
    species_level == "true_species" & percent_production >= percent_true_species_cutoff ~ "true_species_producer",
    TRUE ~ "other_producer"
  )) %>%
  filter(producer_category == "true_species_producer") %>%
  select(iso3c, year) %>%
  distinct()


true_species_trade <- true_species_producers %>%
  left_join(
    artis,
    by = c("iso3c" = "source_country_iso3c", "year")
  )

true_species_prod <- true_species_producers %>%
  left_join(prod,
            by = c("year", "iso3c"))


# Supply / Consumption data
# true_species_supply <- calculate_supply(true_species_trade, 
#                            true_species_prod %>% 
#                              rename(live_weight_t = production_t)) %>%
#   # Add habitat-method column
#   mutate(habitat_method = paste(habitat, method, sep =" ")) %>% 
#   mutate(habitat_method = case_when(
#     str_detect(habitat_method, "unknown") ~ "unknown", 
#     TRUE ~ habitat_method
#   )) %>% 
#   # Set factor levels
#   mutate(habitat_method = factor(habitat_method, levels = c("marine capture", "inland capture",
#                                                             "marine aquaculture", "inland aquaculture",
#                                                             "unknown" ))) %>% 
#   left_join(country_metadata %>% 
#               select(iso3c, "region" = "owid_region"), by = "iso3c") %>% 
#   mutate(supply_no_error = case_when(supply_no_error < 0 ~ 0,
#                                      TRUE ~ supply_no_error))

#-------------------------------------------------------------------------------
# Results
#-------------------------------------------------------------------------------

write.csv(supply, file.path(outdir, "supply.csv"), row.names = FALSE)

global_supply <- supply %>%
  group_by(year) %>%
  summarize(supply = sum(supply) / 1000000) %>%
  ungroup()

write.csv(global_supply, file.path(outdir, "global_supply.csv"), row.names = FALSE)

regional_supply <- supply %>%
  group_by(year, region) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(supply_total = sum(supply)) %>%
  ungroup() %>%
  mutate(percent_supply = 100 * supply / supply_total)

write.csv(regional_supply, file.path(outdir, "regional_supply.csv"), row.names = FALSE)

regional_method_supplies <- supply %>%
  filter(year == 2020) %>%
  group_by(year, region, method) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  group_by(year, region) %>%
  mutate(region_supply = sum(supply)) %>%
  ungroup() %>%
  mutate(percent_supply = 100 * supply / region_supply) %>%
  select(region, region_supply, method, percent_supply) %>%
  pivot_wider(names_from = "method", values_from = "percent_supply")

write.csv(regional_method_supplies, file.path(outdir, "regional_method_supplies.csv"), row.names = FALSE)

method_supplies <- supply %>%
  filter(year == 2020) %>%
  group_by(method) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  mutate(total_supply = sum(supply)) %>%
  mutate(percent_supply = 100 * supply / total_supply)

write.csv(method_supplies, file.path(outdir, "method_supplies.csv"), row.names = FALSE)

habitat_method_supply <- supply %>%
  group_by(year, habitat, method) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(supply_total = sum(supply)) %>%
  ungroup() %>%
  mutate(percent_supply = 100 * supply / supply_total)

write.csv(habitat_method_supply, file.path(outdir, "habitat_method_supply.csv"), row.names = FALSE)

consumption_foreign <- supply %>%
  filter(year == 1996 | year == 2020) %>%
  group_by(year) %>%
  summarize(supply_foreign = sum(supply_foreign),
            supply = sum(supply)) %>%
  ungroup() %>%
  mutate(percent_supply = 100 * supply_foreign / supply)

write.csv(consumption_foreign, file.path(outdir, "consumption_foreign.csv"), row.names = FALSE)

