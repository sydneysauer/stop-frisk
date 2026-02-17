### PROJECT SETUP
library(here)
library(tidyverse)
library(lubridate)
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
combined <- dfs %>% 
  bind_rows(.id = "year")



### INITIAL SUMMARY TABLE

# summary by year: # events and % with any type of force
summary_table <- combined %>%
  mutate(pf_any = if_any(starts_with("pf_"), ~ . == "Y")) %>%
  group_by(year) %>%
  summarise(
    total_stops = n(),
    pct_with_force = 100 * mean(pf_any),
    .groups = "drop"
  ) %>%
  arrange(year)

summary_table

