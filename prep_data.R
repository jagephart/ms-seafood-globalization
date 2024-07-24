
################################################################################
######## NO NEED TO RUN THIS SCRIPT TO REPLICATE RESULTS OR FIGURES ############
################################################################################

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

outdir <- "data"
#-------------------------------------------------------------------------------
# Initial database pulls

# Database connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("DB_NAME"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("DB_USERNAME"),
                 password=Sys.getenv("DB_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# ARTIS dataframe
artis_query <- "SELECT * FROM snet WHERE 
((hs_version = 'HS96' AND year >= 1996 AND year <= 2003) OR
(hs_version = 'HS02' AND year >= 2004 AND year <= 2009) OR
(hs_version = 'HS07' AND year >= 2010 AND year <= 2012) OR
(hs_version = 'HS12' AND year >= 2013 AND year <= 2020))"

artis <- dbGetQuery(con, artis_query) %>%
  select(-record_id) %>%
  filter(!is.na(live_weight_t) & !is.na(product_weight_t)) %>%
  mutate(hs6 = as.character(hs6)) %>%
  mutate(hs6 = case_when(
    str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
    TRUE ~ hs6
  ))

artis_fm <- artis %>% 
  filter(hs6 == "230120") %>% 
  group_by(year) %>%
  summarise(live_weight_t = sum(live_weight_t, na.rm = TRUE),
            product_weight_t = sum(product_weight_t, na.rm = TRUE))

artis_nonfood <- artis %>% 
  # Keep only FMFO, and ornamental species
  filter(hs6 %in% c("230120",  "030110", "030111", "030119"))

artis <- artis %>% 
  # Remove FMFO and ornamental species
  filter(!(hs6 %in% c("230120",  "030110", "030111", "030119")))

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

pop <- dbGetQuery(con, "SELECT * FROM population") %>%
  select(-record_id)

consumption <- dbGetQuery(con, "SELECT * FROM consumption WHERE 
((hs_version = 'HS96' AND year >= 1996 AND year <= 2003) OR
(hs_version = 'HS02' AND year >= 2004 AND year <= 2009) OR
(hs_version = 'HS07' AND year >= 2010 AND year <= 2012) OR
(hs_version = 'HS12' AND year >= 2013 AND year <= 2020))") %>%
  select(-record_id)

# Close database connection
dbDisconnect(con)
rm(list = c("con"))

# Population data with region data
pop <- pop %>%
  left_join(country_metadata, by = c("iso3c")) %>%
  select(iso3c, year, "region" = "owid_region", pop)

# write.csv(artis, file.path(outdir, "artis.csv"), row.names = FALSE)
write.csv(prod, file.path(outdir, "prod.csv"), row.names = FALSE)

# Write out the fishmeal trade data, aggregated by year, for summary stats
write.csv(artis_fm, file.path(outdir, "artis_fm.csv"), row.names = FALSE)

# Write out the bilater non-food trade data to use in supply calculations
write.csv(artis_nonfood, file.path(outdir, "artis_nonfood.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Summarize cleaned FAO production data
prod <- prod %>% 
  group_by(year, iso3c, sciname, method, habitat) %>%
  summarise(production_t = sum(live_weight_t, na.rm = TRUE)) %>%
  ungroup()

artis <- artis %>%
  filter(live_weight_t >= 0.1) %>%
  # Add importer and exporter region
  left_join(country_metadata %>% 
              select("exporter_iso3c" = "iso3c", "exporter_region" = "owid_region"), by = c("exporter_iso3c")) %>%
  left_join(country_metadata %>% 
              select("importer_iso3c" = "iso3c", "importer_region" = "owid_region"), by = c("importer_iso3c")) %>%
  ungroup()

# Update artis data to lowest resolution by comparing code taxa_level and prod taxa_level
artis <- artis %>%
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
rm(list = c("code_max_resolved_taxa"))

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
  filter(dom_source == "domestic") %>%
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

# Concentration of trade by production source
# measured as the number of countries comprising 75%

# Summarize total imports and exports by habitat and environment
bilateral_habitat_method_summary <- artis %>%
  # Remove if we decide to leave FMFO in
  filter(hs6 != "230120") %>%
  group_by(year, exporter_iso3c, exporter_region, importer_iso3c, importer_region, habitat_method, dom_source) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ungroup()

write.csv(bilateral_habitat_method_summary, file.path(outdir, "bilateral_habitat_method_summary.csv"), row.names = FALSE)

# Set percent threshold
cumulative_percent_threshold <- 75

# fig 1d Export concentration

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
write.csv(export_concentration_n_countries,
          file.path(outdir, "fig_1d_totals.csv"),
          row.names = FALSE)

export_concentration_habitat_method_n_countries <- export_concentration_habitat_method %>%
  filter(cumulative_percent < cumulative_percent_threshold) %>%
  group_by(year, habitat_method) %>% 
  tally() %>%
  filter(habitat_method != "unknown")

write.csv(export_concentration_habitat_method_n_countries, file.path(outdir, "export_concentration_n_countries_by_source.csv"), row.names = FALSE)
write.csv(export_concentration_habitat_method_n_countries,
          file.path(outdir, "fig_1d_habitat_method.csv"),
          row.names = FALSE)

# fig 1e Import concentration
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
write.csv(import_concentration_n_countries,
          file.path(outdir, "fig_1e_totals.csv"),
          row.names = FALSE)

import_concentration_habitat_method_n_countries <- import_concentration_habitat_method %>%
  filter(cumulative_percent < cumulative_percent_threshold) %>%
  group_by(year, habitat_method) %>% 
  tally() %>%
  filter(habitat_method != "unknown")

write.csv(import_concentration_habitat_method_n_countries, file.path(outdir, "import_concentration_n_countries_by_source.csv"), row.names = FALSE)
write.csv(import_concentration_habitat_method_n_countries,
          file.path(outdir, "fig_1e_habitat_method.csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
# Consumption processing

# Standardize consumption to at most 100 kg per capita per year
consumption_percap_max <- 100
# finding all countries for which consumption per capita exceeds max threshold
consumption_outliers <- consumption %>%
  # summarize consumption per country and year (tonnes)
  group_by(consumer_iso3c, year) %>%
  summarize(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup() %>%
  # consumption from tonnes to kg
  mutate(consumption_kg = consumption_live_t * 1000) %>%
  # add population data
  left_join(
    pop %>%
      select(-region),
    by = c("consumer_iso3c"="iso3c", "year")
  ) %>%
  # calculate per capita consumption kg per person per year
  mutate(consumption_percap_kg = consumption_kg / pop) %>%
  # isolate countries and years where consumption per capita exceed threshold
  filter(consumption_percap_kg > consumption_percap_max) %>%
  # calculate per capita consumption for outliers is scaled to be
  # exactly the maximum per capita consumption threshold in kg
  mutate(corrected_consumption_kg = pop * consumption_percap_max) %>%
  # transform corrected consumption from kg to tonnes
  mutate(corrected_consumption_t = corrected_consumption_kg / 1000) %>%
  select(consumer_iso3c, year, corrected_consumption_t)

# distribute scaled consumption of outliers across all intermediates
consumption_scaled <- consumption_outliers %>%
  left_join(
    # calculating proportion of consumption across all intermediates
    # by consumer and year
    consumption %>%
      group_by(consumer_iso3c, year) %>%
      mutate(prop = consumption_live_t / sum(consumption_live_t)) %>%
      ungroup(),
    by = c("consumer_iso3c", "year")
  ) %>%
  # calculated scaled consumption of outliers 
  mutate(consumption_live_t = corrected_consumption_t * prop) %>%
  select(-c(prop, corrected_consumption_t)) %>%
  mutate(consumer_year = paste(consumer_iso3c, year, sep = "_"))

consumption_percap_adjusted <- consumption %>%
  mutate(consumer_year = paste(consumer_iso3c, year, sep = "_")) %>%
  filter(!(consumer_year %in% consumption_scaled$consumer_year)) %>%
  bind_rows(consumption_scaled)

consumption_percap_adjusted <- consumption_percap_adjusted %>%
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

write.csv(consumption_percap_adjusted, file.path(outdir, "consumption_percap_adjusted.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Figure 2 Foreign consumption (2019) top species and trade flows

# Summarized foreign consumption data for habitat method maps 2019
foreign_consumption_2019 <- consumption_percap_adjusted %>%
  filter(year == 2019 &
           consumption_source == "foreign" &
           source_country_iso3c != "unknown") %>%
  group_by(consumer_iso3c, source_country_iso3c, habitat, method) %>%
  summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
  ungroup()

# Marine capture Map data foreign consumption 2019
marine_cap_foreign_consumption <- foreign_consumption_2019 %>%
  filter(habitat == "marine", 
         method == "capture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup()

write.csv(marine_cap_foreign_consumption, file.path(outdir, "marine_capture_foreign_consumption.csv"), row.names = FALSE)
write.csv(marine_cap_foreign_consumption,
          file.path(outdir, "fig_2_marine_capture.csv"),
          row.names = FALSE)

# Marine aquaculture Map data foreign consumption 2019
marine_aqua_foreign_consumption <- foreign_consumption_2019 %>%
  filter(habitat == "marine", 
         method == "aquaculture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup()

write.csv(marine_aqua_foreign_consumption,
          file.path(outdir, "marine_aquaculture_foreign_consumption.csv"),
          row.names = FALSE)
write.csv(marine_aqua_foreign_consumption,
          file.path(outdir, "fig_2_marine_aquaculture.csv"),
          row.names = FALSE)

# Inland capture Map data foreign consumption 2019
inland_cap_foreign_consumption <- foreign_consumption_2019 %>%
  filter(habitat == "inland", 
         method == "capture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup()

write.csv(inland_cap_foreign_consumption,
          file.path(outdir, "inland_capture_foreign_consumption.csv"),
          row.names = FALSE)
write.csv(inland_cap_foreign_consumption,
          file.path(outdir, "fig_2_inland_capture.csv"),
          row.names = FALSE)


# Inland aquaculture Map data foreign consumption 2019
inland_aqua_foreign_consumption <- foreign_consumption_2019 %>%
  filter(habitat == "inland", 
         method == "aquaculture") %>%
  group_by(consumer_iso3c, source_country_iso3c) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  ungroup()

write.csv(inland_aqua_foreign_consumption,
          file.path(outdir, "inland_aquaculture_foreign_consumption.csv"),
          row.names = FALSE)
write.csv(inland_aqua_foreign_consumption,
          file.path(outdir, "fig_2_inland_aquaculture.csv"),
          row.names = FALSE)

# Foreign consumption top species
top_scinames_foreign_consumed <- consumption_percap_adjusted %>%
  filter(year == 2019, 
         consumption_source == "foreign", 
         source_country_iso3c != "unknown") %>%
  mutate(habitat_method = paste(habitat, method, sep = " ")) %>%
  group_by(sciname_hs_modified, habitat_method) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  group_by(habitat_method) %>%
  slice_max(order_by = consumption_live_t, n = 5) %>%
  select(sciname_hs_modified, habitat_method)

top_scinames_source_consumer <- top_scinames_foreign_consumed %>% 
  left_join(consumption_percap_adjusted %>%
              filter(year == 2019, 
                     consumption_source == "foreign", 
                     source_country_iso3c != "unknown") %>%
              mutate(habitat_method = paste(habitat, method, sep = " ")),
            by = c("sciname_hs_modified", "habitat_method")) %>%
  rename("Consumer" = "consumer_region", "Producer" = "source_region") %>%
  group_by(common_name, habitat_method, Producer, Consumer) %>%
  summarise(consumption_live_t = sum(consumption_live_t)) %>%
  pivot_longer(cols = Consumer:Producer, names_to = "flow", values_to = "region") 

marine_cap_top_scinames <- top_scinames_source_consumer %>%
  filter(habitat_method == "marine capture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total)))

write.csv(marine_cap_top_scinames,
          file.path(outdir, "marine_capture_top_scinames_foreign_consumption.csv"),
          row.names = FALSE)
write.csv(marine_cap_top_scinames,
          file.path(outdir, "fig_2_marine_capture_scinames.csv"),
          row.names = FALSE)

marine_aqua_top_scinames <- top_scinames_source_consumer %>%
  filter(habitat_method == "marine aquaculture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total)))

write.csv(marine_aqua_top_scinames,
          file.path(outdir, "marine_aquaculture_top_scinames_foreign_consumption.csv"),
          row.names = FALSE)
write.csv(marine_aqua_top_scinames,
          file.path(outdir, "fig_2_marine_aquaculture_scinames.csv"),
          row.names = FALSE)

inland_cap_top_scinames <- top_scinames_source_consumer %>%
  filter(habitat_method == "inland capture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total)))

write.csv(inland_cap_top_scinames,
          file.path(outdir, "inland_capture_top_scinames_foreign_consumption.csv"),
          row.names = FALSE)
write.csv(inland_cap_top_scinames,
          file.path(outdir, "fig_2_inland_capture_scinames.csv"),
          row.names = FALSE)

inland_aqua_top_scinames <- top_scinames_source_consumer %>%
  filter(habitat_method == "inland aquaculture") %>%
  group_by(common_name) %>%
  mutate(total = sum(consumption_live_t)) %>%
  ungroup() %>%
  mutate(common_name = reorder(common_name, desc(total)))

write.csv(inland_aqua_top_scinames,
          file.path(outdir, "inland_aquaculture_top_scinames_foreign_consumption.csv"),
          row.names = FALSE)
write.csv(inland_aqua_top_scinames,
          file.path(outdir, "fig_2_inland_aquaculture_scinames.csv"),
          row.names = FALSE)


#-------------------------------------------------------------------------------
# Figure 3 Trade patterns

# Matrix of trade between regions
artis_region_method <- artis %>%
  filter(!is.na(importer_region), !is.na(exporter_region), 
         importer_region != "Other nei", exporter_region != "Other nei") %>%
  group_by(year, exporter_region, importer_region, habitat_method) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_importer = paste(exporter_region, " to ", importer_region, sep = ""))

write.csv(artis_region_method, file.path(outdir, "artis_region_method.csv"), row.names = FALSE)
write.csv(artis_region_method,
          file.path(outdir, "fig_3_data.csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
# Figure 4

# Edit to focus on consumption from domestic vs foreign sources
supply <- consumption_percap_adjusted %>%
  mutate(consumption_source = case_when(
    consumption_source == "domestic" ~ "domestic_consumption_t",
    consumption_source == "foreign" ~ "foreign_consumption_t",
    TRUE ~ "error"
  )) %>%
  group_by(consumer_iso3c, habitat, method, consumption_source, year) %>%
  summarize(supply = sum(consumption_live_t, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = consumption_source, values_from = supply) %>%
  replace_na(list(domestic_consumption_t = 0, foreign_consumption_t = 0)) %>%
  mutate(supply = domestic_consumption_t + foreign_consumption_t) %>%
  rename(iso3c = consumer_iso3c)

supply <- supply %>%
  # Remove observations where supply is 0
  filter(supply > 0) %>%
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

write.csv(global_supply_per_cap, file.path(outdir, "global_supply_per_cap.csv"), row.names = FALSE)

# Figure 4a
# Calculate per capita supply by region and source
supply_total <- supply %>%
  group_by(iso3c, region, year, habitat_method) %>%
  summarise(supply = sum(supply),
            supply_domestic = sum(supply_domestic),
            supply_foreign = sum(supply_foreign),
            pop = sum(pop)
  ) %>%
  ungroup()

fig4a_data <- supply_total %>%
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

write.csv(fig4a_data, file.path(outdir, "fig_4a_data.csv"), row.names = FALSE)

# Figure 4b

regional_pop <- pop %>%
  group_by(region, year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

fig4b_data <- supply %>%
  filter(region != "Other nei") %>%
  group_by(year, region, habitat_method) %>%
  summarize(supply = sum(supply)) %>%
  ungroup() %>%
  left_join(
    regional_pop,
    by = c("region", "year")
  ) %>%
  mutate(supply_per_cap = 1000 * supply / pop)

write.csv(fig4b_data, file.path(outdir, "fig_4b_data.csv"), row.names = FALSE)

# Figure 4c
# Global percent of supply that is domestic/foreign
supply_year_summary <- supply %>%
  filter(region != "Other nei") %>%
  group_by(year) %>%
  summarise(supply_domestic = 100 * sum(supply_domestic) / sum(supply),
            supply_foreign = 100 * sum(supply_foreign) / sum(supply)) %>%
  pivot_longer(cols = supply_domestic:supply_foreign, 
               names_to = "supply_source", values_to = "supply_percent") %>%
  mutate(supply_source = gsub("supply_", "", supply_source))

fig4c_data <- supply_year_summary

write.csv(fig4c_data, file.path(outdir, "fig_4c_data.csv"), row.names = FALSE)

# Figure 4d
# Regional percent of supply that is domestic/foreign
fig4d_data <- supply %>% 
  filter(region != "Other nei") %>%
  group_by(region, year) %>%
  summarise(supply_domestic = 100*sum(supply_domestic)/sum(supply),
            supply_foreign = 100*sum(supply_foreign)/sum(supply)) %>%
  pivot_longer(cols = supply_domestic:supply_foreign, 
               names_to = "supply_source", values_to = "supply_percent") %>%
  mutate(supply_source = gsub("supply_", "", supply_source))

write.csv(fig4d_data, file.path(outdir, "fig_4d_data.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# SUPPLEMENTARY FIGURES
#-------------------------------------------------------------------------------

# SI Figure 1
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
write.csv(regional_artis,
          file.path(outdir, "si_fig_1_data.csv"),
          row.names = FALSE)

# Calculate percent of pairs with increased trade flows
bilateral_flow_change <- bilateral_habitat_method_summary %>% 
  filter(exporter_region != "Other nei", importer_region != "Other nei",
         year %in% c(1996:2000, 2016:2020)) %>%
  mutate(year_group = case_when(
    year %in% 1996:2000 ~ "beginning", 
    year %in% 2016:2020 ~ "end")) %>%
  group_by(year_group, exporter_iso3c, importer_iso3c) %>% 
  summarise(live_weight_t = sum(live_weight_t)) %>%
  ungroup() %>%
  pivot_wider(names_from = year_group, values_from = live_weight_t, values_fill = 0) %>% 
  mutate(change = end-beginning) %>%
  arrange(desc(change))

write.csv(bilateral_flow_change, file.path(outdir, "bilateral_flow_change.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Top Trading Partners old (1996 - 2000) vs recent (2016 - 2020)
# SI Figure 2

# Top Exporters by habitat and production method

# Top exporters by habitat and production method fill by dom source (domestic/foreign)
si_top_exporters_old <- bilateral_habitat_method_summary %>%
  filter(year >= 1996 & year <= 2000 & habitat_method != "unknown") %>%
  group_by(exporter_iso3c, habitat_method, dom_source) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2000 - 1996)) %>%
  ungroup() %>%
  pivot_wider(names_from="dom_source", values_from = "live_weight_t", values_fill = 0) %>%
  mutate(live_weight_t = domestic + foreign) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_iso3c = reorder_within(exporter_iso3c, ranking, habitat_method))

write.csv(si_top_exporters_old, file.path(outdir, "si_fig_2_exporters_old.csv"), row.names = FALSE)

si_top_exporters_new <- bilateral_habitat_method_summary %>%
  filter(year >= 2016 & year <= 2020 & habitat_method != "unknown") %>%
  group_by(exporter_iso3c, habitat_method, dom_source) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2000 - 1996)) %>%
  ungroup() %>%
  pivot_wider(names_from="dom_source", values_from = "live_weight_t", values_fill = 0) %>%
  mutate(live_weight_t = domestic + foreign) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(exporter_iso3c = reorder_within(exporter_iso3c, ranking, habitat_method))

write.csv(si_top_exporters_new, file.path(outdir, "si_fig_2_exporters_recent.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# SI Figure 3
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

si_top_importers_old <- bilateral_habitat_method_summary %>%
  filter(year >= 1996 & year <= 2000 & habitat_method != "unknown") %>%
  group_by(importer_iso3c, habitat_method, dom_source) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2000 - 1996)) %>%
  ungroup() %>%
  pivot_wider(names_from="dom_source", values_from = "live_weight_t", values_fill = 0) %>%
  mutate(live_weight_t = domestic + foreign) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(importer_iso3c = reorder_within(importer_iso3c, ranking, habitat_method))

write.csv(si_top_importers_old, file.path(outdir, "si_fig_3_importers_old.csv"), row.names = FALSE)

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

si_top_importers_new <- bilateral_habitat_method_summary %>%
  filter(year >= 2016 & year <= 2020 & habitat_method != "unknown") %>%
  group_by(importer_iso3c, habitat_method, dom_source) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE) / (2000 - 1996)) %>%
  ungroup() %>%
  pivot_wider(names_from="dom_source", values_from = "live_weight_t", values_fill = 0) %>%
  mutate(live_weight_t = domestic + foreign) %>%
  group_by(habitat_method) %>%
  slice_max(n = 10, order_by = live_weight_t) %>%
  ungroup() %>%
  group_by(habitat_method) %>%
  mutate(ranking = rank(live_weight_t)) %>%
  ungroup() %>%
  mutate(importer_iso3c = reorder_within(importer_iso3c, ranking, habitat_method))

write.csv(si_top_importers_new, file.path(outdir, "si_fig_3_importers_recent.csv"), row.names = FALSE)
#-------------------------------------------------------------------------------
# SI Figure 4
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
write.csv(regional_avg_annual_change,
          file.path(outdir, "si_fig_4_data.csv"),
          row.names = FALSE)
#-------------------------------------------------------------------------------
# SI Figure 5

# SI Fig 5a
dom_source_artis <- artis %>%
  filter(dom_source != "error")

dom_source_ts <- artis %>%
  group_by(dom_source, year) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  ungroup()

write.csv(dom_source_ts, file.path(outdir, "dom_source_ts.csv"), row.names = FALSE)
write.csv(dom_source_ts,
          file.path(outdir, "si_fig_5a_data.csv"),
          row.names = FALSE)

# SI Fig 5b
dom_source_by_habitat_method <- dom_source_artis %>%
  group_by(dom_source, habitat_method, year) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  ungroup()

write.csv(dom_source_by_habitat_method, file.path(outdir, "dom_source_by_habitat_method.csv"), row.names = FALSE)
write.csv(dom_source_by_habitat_method,
          file.path(outdir, "si_fig_5b_data.csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
# SI Fig 6

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
write.csv(ldc_net_habitat_method_summary,
          file.path(outdir, "si_fig_6_data.csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
# SI Fig 7

# Database connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("DB_NAME"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("DB_USERNAME"),
                 password=Sys.getenv("DB_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

artis_ts_query <- "SELECT year, hs_version, SUM(product_weight_t) AS product_weight_t FROM snet GROUP BY year, hs_version"

artis_ts <- dbGetQuery(con, artis_ts_query)

dbDisconnect(con)

# artis_ts <- read.csv(file.path(outdir, "artis_ts.csv"))

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

write.csv(artis_ts,
          file.path(outdir, "si_fig_7_artis_ts.csv"),
          row.names = FALSE)
write.csv(custom_ts,
          file.path(outdir, "si_fig_7_custom_ts.csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
# Results
#-------------------------------------------------------------------------------

write.csv(supply, file.path(outdir, "supply.csv"), row.names = FALSE)

global_supply <- supply %>%
  group_by(year) %>%
  summarize(supply = sum(supply) / 1000000) %>%
  ungroup()

write.csv(global_supply, file.path(outdir, "global_supply.csv"), row.names = FALSE)

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


