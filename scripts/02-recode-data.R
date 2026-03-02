library(tidyverse)
library(lubridate)
library(stringr)
library(here)

source("R/data_recoding.R")

# Load raw data
sqf_raw <- read_rds(here("data/sqf_raw.rds"))

# Recode each year
sqf_clean <- sqf_raw %>%
  group_split(year) %>%
  map_dfr(~ recode_sqf_year(.x, as.integer(unique(.x$year))))

# Print summary
message(sprintf("Recoded %s observations", format(nrow(sqf_clean), big.mark = ",")))

# Save
write_rds(sqf_clean, here("data/sqf_clean.rds"), compress = "gz")