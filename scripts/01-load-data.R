### PROJECT SETUP
library(here)
library(tidyverse)
library(lubridate)

source("R/data_recoding.R")

### DATA LOADING
# load individual data files
sqf_2006 <- read_csv(here("data", "raw", "2006.csv"), col_types = cols(.default = col_character()))
sqf_2007 <- read_csv(here("data", "raw", "2007.csv"), col_types = cols(.default = col_character()))
sqf_2008 <- read_csv(here("data", "raw", "2008.csv"), col_types = cols(.default = col_character()))
sqf_2009 <- read_csv(here("data", "raw", "2009.csv"), col_types = cols(.default = col_character()))
sqf_2010 <- read_csv(here("data", "raw", "2010.csv"), col_types = cols(.default = col_character()))
sqf_2011 <- read_csv(here("data", "raw", "2011.csv"), col_types = cols(.default = col_character()))
sqf_2012 <- read_csv(here("data", "raw", "2012.csv"), col_types = cols(.default = col_character()))

# combine all objects into one tibble with a year column
yrs <- 2006:2012
dfs <- mget(paste0("sqf_", yrs))
names(dfs) <- as.character(yrs)
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

