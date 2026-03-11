
#' Download and prepare NYC Census tract boundaries
#'
#' Downloads tract shapefiles from the US Census Bureau via the tigris
#' package and standardizes column names. NYC spans five counties
#' (Manhattan=061, Brooklyn=047, Queens=081, Bronx=005, Staten Island=085).
#'
#' @param year Integer, Census year for tract boundaries (default: 2010)
#' @param crs Integer, EPSG code for coordinate reference system (default: 2263)
#' @return sf object with columns: ct_code, area_land, area_water, geometry
#'
#' @examples
#' tracts <- get_nyc_tracts()
#' plot(st_geometry(tracts))
get_nyc_tracts <- function(year = 2010, crs = 2263) {
  codes <- c("061", "047", "081", "005", "085")
  tracts_list <- lapply(codes, function(code) {
    tigris::tracts(state = "NY", county = code, year = year, class = "sf") %>%
      select(ct_code = GEOID10, area_land = ALAND10, area_water = AWATER10) %>%
      st_transform(crs)
  })
  do.call(rbind, tracts_list)
}

#' Convert SQF data to spatial points
#'
#' Takes a data frame with xcoord and ycoord columns and converts it
#' to an sf POINT object. Rows with missing coordinates are dropped.
#'
#' @param data Tibble with xcoord and ycoord columns
#' @param crs Integer, EPSG code matching the coordinate system (default: 2263)
#' @return sf object with POINT geometry
#'
#' @examples
#' sqf_spatial <- make_spatial(sqf_clean)
make_spatial <- function(data, crs = 2263) {
  # Validation: xcoord and ycoord exist in the data 
  if (!all(c("xcoord", "ycoord") %in% colnames(data))) {
    stop("Input data must contain 'xcoord' and 'ycoord' columns.")
  }
  # Perform the operation
  spatial <- data %>%
    filter(!is.na(xcoord) & !is.na(ycoord)) %>%
    st_as_sf(coords = c("xcoord", "ycoord"), crs = crs)
  dropped <- nrow(data) - nrow(spatial)
  if (dropped > 0) {
    message(sprintf("Dropped %s rows with missing coordinates", format(dropped, big.mark = ",")))
  }
  return(spatial)
}

#' Join spatial points to polygons
#'
#' Performs a spatial join to determine which polygon (e.g., Census tract)
#' each point falls within. Points that don't fall within any polygon
#' are dropped.
#'
#' @param points sf object with POINT geometry (e.g., SQF stops)
#' @param polygons sf object with POLYGON geometry (e.g., Census tracts)
#' @return sf object with point data joined to polygon attributes
#'
#' @examples
#' sqf_geocoded <- spatial_join(sqf_spatial, tracts)
spatial_join <- function(points, polygons) {
  # Validation: Check that CRS matches between points and polygons
  if (st_crs(points) != st_crs(polygons)) {
    stop("CRS of points and polygons do not match. Please ensure they are the same.")
  }
  
  geocoded <- st_join(points, polygons, join = st_within)
  unmatched <- sum(is.na(geocoded$ct_code))

  # Validation: At least one point should match a polygon
  if (unmatched == nrow(points)) {
    stop("No points matched any polygons. Check your data and CRS.")
  } else if (unmatched > 0) {
    message(sprintf("%s points did not match any polygon", format(unmatched, big.mark = ",")))
  } else {
    message("All points successfully matched to polygons.")
  }
  return(geocoded %>% filter(!is.na(ct_code))) # Drop points that don't match any polygon
}

#' Aggregate SQF stops by Census tract and year
#'
#' Computes summary statistics for each Census tract by year from
#' geocoded stop-level data.
#'
#' @param geocoded_data sf object, geocoded SQF data (from spatial_join)
#' @return Tibble with one row per tract-year: ct_code, year, total_stops,
#'   stops_black, stops_hispanic, stops_white, pct_black, pct_force
#'
#' @examples
#' tract_summary <- aggregate_by_tract_yr(sqf_geocoded)
aggregate_by_tract_yr <- function(geocoded_data) {
  aggregate <- geocoded_data %>%
    st_set_geometry(NULL) %>%
    group_by(ct_code, year) %>%
    summarise(
      ct_code = first(ct_code),
      year = first(year),
      total_stops = n(),
      stops_black = sum(race == "Black", na.rm = TRUE),
      stops_hispanic =  sum(race == "Hispanic", na.rm = TRUE),
      stops_white = sum(race == "White", na.rm = TRUE), 
      pct_black = stops_black / total_stops * 100,
      pct_force = sum(police_force == TRUE, na.rm = TRUE) / total_stops * 100
    ) %>%
    ungroup()
  return(aggregate)
}

#' Create a choropleth map of SQF data by Census tract
#'
#' @param tracts sf object with tract geometries and summary data
#' @param fill_var Character, name of the variable to map
#' @param trans Character, transformation for the fill scale (e.g., "log10")
#' @param title Character, plot title
#' @return ggplot object
map_tracts <- function(tracts, fill_var, log = FALSE, title = "") {
  trans <- ifelse(log, "log10", "identity")
  map <- ggplot(tracts) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "white", linewidth = 0.05) +
    scale_fill_viridis_c(trans = trans, na.value = "white") +
    theme_void() +
    labs(fill = fill_var, title = title) 
  return(map)
}