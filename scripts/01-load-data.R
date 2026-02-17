library(here)
library(tidyverse)
library(stringr)

source("R/data_loading.R")

# Read all years of data into one df
years <- c(2006:2012)
for (year in years) {
  df <- read_sqf(year)
  assign(paste0("sqf_", year), df)
}
dfs <- mget(paste0("sqf_", years))
names(dfs) <- as.character(years)
sqf <- dfs %>% 
  bind_rows(.id = "year")

# Keep only consolidated df and years vector in environment
rm(list = setdiff(ls(), c("sqf", "years")))

# Save raw data as RDS
saveRDS(sqf, file = here("data/sqf_raw.rds"))

# Output initial summary table by year
summary_table <- sqf %>%
  mutate(pf_any = if_any(starts_with("pf_"), ~ . == "Y")) %>%
  group_by(year) %>%
  summarise(
    total_stops = n(),
    pct_with_force = 100 * mean(pf_any),
    .groups = "drop"
  ) %>%
  arrange(year)
summary_table
