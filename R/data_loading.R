#' Load sqf file for a given year
#'
#' Reads the raw CSV file for the specified year and returns a tibble.
#'
#' @param year Integer, year of the data to load (e.g., 2006)
#' @return Tibble containing the data for the specified year
#'
read_sqf <- function(year) {
  path <- here("data", "raw", paste0(year, ".csv"))
  clean <- read_csv(path, col_types = cols(.default = col_character()))
  return(clean)
}
