# NYC Stop and Frisk Analysis (2006-2012)

Analysis of NYPD Stop, Question, and Frisk data.

## Data Source

Data from [NYPD Stop, Question and Frisk Database](https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page)

## Data Processing Pipeline

1. Download 2006-2012 data files and place in `data/raw/`
2. Install required packages (listed below)
3. Run the numbered scripts, in order:
    `scripts/01-load-data.R`
    `scripts/02-recode-data.R`
    `scripts/03-validate-data.R`
    `scripts/04-geocode-data.R`

## Required Packages

- tidyverse
- lubridate
- here
- stringr
- lubridate
- sf
- tigris

## Geocoding Pipeline 

The coordinate reference system (CRS) used for spatial data is EPSG:2263 (NAD83 / New York Long Island). This is because the data from NYPD are provided in this coordinate system, which is suitable for mapping and spatial analysis in New York City.

The geocoding pipeline proceeds in the following steps:
1. Load cleaned SQF data.
2. Download NYC Census tract boundaries using `get_nyc_tracts()`.
3. Convert SQF data to spatial points using `make_spatial()`.
4. Perform a spatial join to assign each stop to a Census tract using `spatial_join()`.
5. Aggregate stop-level data by tract and year using `aggregate_by_tract_yr()`.
6. Create choropleth maps of SQF data by Census tract using `map_tracts()`.

## Other Functions 

`read_sqf(year)` - Reads SQF file.

`recode_race(race_raw)` — Maps raw race codes (W,B,P,Q,A,I,Z) into an ordered factor (White, Black, Hispanic, Asian, Other).

`clean_age(age_raw)` — Converts age to integer and converts sentinel values (99, 377, 999) or invalid values (>100 or <0) to NA.

`parse_sqf_datetime(datestop, timestop, date_format = "Ymd")` — Combines datestop + timestop into POSIXct object, handles multiple date formats and sentinel dates.

`recode_sqf_year(data_raw, year)` — Orchestrates per-year recoding; returns standardized columns: id, date, time, year, race, female, age, police_force, precinct, xcoord, ycoord

`validate_sqf_data(data)` - Validates data and produces report of any issues.

## Variable Validation

The validation for each variable is as follows:

`id`: No duplicates
`year`: In valid project range (2006-2012)
`race`: Valid category based on project codebook
`female`: Ensures logical 
`age`: Integer, between 0 and 100
`xcoord`: Ensure not all NA
`ycoord`: Ensure not all NA