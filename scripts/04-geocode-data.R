library(tidyverse)
library(sf)
library(tigris)
library(here)
source(here("R/spatial_functions.R"))

# Load cleaned data (from Assignment 2)
sqf_clean <- read_rds(here("data/sqf_clean.rds"))
# Step 1: Download Census tract boundaries
tracts <- get_nyc_tracts()

# Step 2: Convert SQF data to spatial points
sqf_spatial <- make_spatial(sqf_clean)

# Step 3: Spatial join -- assign each stop to a Census tract
sqf_geocoded <- spatial_join(sqf_spatial, tracts)

# Print summary
message(sprintf(
  "Geocoded %s of %s stops (%0.1f%%)",
  format(nrow(sqf_geocoded), big.mark = ","),
  format(nrow(sqf_clean), big.mark = ","),
  100 * nrow(sqf_geocoded) / nrow(sqf_clean)
))

# Save results
write_rds(sqf_geocoded, here("data/sqf_geocoded.rds"), compress = "gz")
write_rds(tracts, here("data/nyc_tracts.rds"), compress = "gz")