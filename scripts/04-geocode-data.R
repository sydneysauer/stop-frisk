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

# Aggregate by tract for each year
# Note: I chose to modify the function to aggregate by tract and year rather than use map, for simplicity.
tract_by_year <- aggregate_by_tract_yr(sqf_geocoded)

# Rejoin tract summaries to geometry for mapping
tracts_summary <- tracts %>%
  left_join(tract_by_year, by = "ct_code") %>%
  st_as_sf() # Ensure it remains an sf object

# Create maps
# Map 1: Total stops by tract
map_1 <- map_tracts(tracts_summary, fill_var = "total_stops", 
                    log=TRUE, title = "Total SQF Stops by Census Tract")
ggsave(here("output/map_total_stops.png"), map_1, width = 10, height = 8)
# Map 2: Percent of stops where force used
map_2 <- map_tracts(tracts_summary, fill_var = "pct_force", 
                    title = "Percent of SQF Stops with Force Used by Census Tract")
ggsave(here("output/map_pct_force.png"), map_2, width = 10, height = 8)
