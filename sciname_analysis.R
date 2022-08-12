
library(tidyverse)
library(DBI)
library(rmarkdown)
library(countrycode)

# Establish connection to database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("localhost"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USERNAME"),
  password = Sys.getenv("DB_PASSWORD")
)

# Get initial data pull from database
artis <- dbGetQuery(con,
'SELECT hs_version, year, sciname, SUM(live_weight_t)
FROM snet WHERE (snet_est = \'mid\')
GROUP BY hs_version, year, sciname')

# Production Data
prod <- dbGetQuery(con,
                   'SELECT * FROM production')

# Species / Species Groups metadata
sciname <- dbGetQuery(con,
                      'SELECT * FROM sciname')

# Disconnect from database
dbDisconnect(con)

# Threshold for amount traded or produced
threshold <- 1000

trade_sciname <- artis %>%
  rename(live_weight_t = sum) %>%
  filter(live_weight_t >= threshold)

prod_sciname <- prod %>%
  group_by(year, sciname) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
  filter(live_weight_t >= threshold) %>%
  rename(production = live_weight_t) %>%
  pivot_longer(cols = c(production), names_to = "hs_version", values_to = "live_weight_t")

sciname_summary <- trade_sciname %>%
  full_join(prod_sciname) %>%
  rename("Source" = hs_version)

sciname_counts <- sciname_summary %>%
  group_by(year, Source) %>%
  tally() %>%
  pivot_wider(names_from = Source, values_from = n)

write.csv(sciname_summary, "sciname_counts_summary.csv", row.names = FALSE)

sciname_counts %>%
  pivot_longer(-year, names_to = "Source", values_to = "n") %>%
  ggplot(aes(x = year, y = n, color = Source)) +
  geom_line() +
  labs(x = "Year", y = "")


sciname_percents <- sciname_counts %>%
  mutate_at(.vars = vars(HS96, HS02, HS07, HS12, HS17),
            .funs = list(~((. / production) * 100)))

write.csv(sciname_percents, "sciname_percent_summary.csv", row.names = FALSE)

sciname_percents %>%
  pivot_longer(-year, names_to = "Source", values_to = "percent_scinames") %>%
  filter(Source != "production") %>%
  ggplot(aes(x = year, y = percent_scinames, color = Source)) +
  geom_line() +
  labs(x = "Year", y = "Percent of Species / Species Groups exported", color = "HS Version") +
  theme_bw()

