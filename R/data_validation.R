#' Validate cleaned SQF data
#'
#' Performs comprehensive checks on cleaned SQF data to ensure
#' quality and catch potential data issues early.
#'
#' @param data Tibble, cleaned SQF data
#' @return List with validation results and any issues found
#'
#' @examples
#' sqf_clean <- read_rds("data/sqf_clean.rds")
#' validation <- validate_sqf_data(sqf_clean)
#' print(validation)
validate_sqf_data <- function(data) {
  # Create empty list to populate with any issues
  issues <- list()
  
  # Check 1: Required columns exist
  required_cols <- c("id", "date", "time", "year", "race", "female", 
                     "age", "police_force", "precinct", "xcoord", "ycoord")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }
  
  # Check 2: Year in valid range
  invalid_years <- data %>%
    filter(year < 2006 | year > 2012) %>%
    nrow()
  if (invalid_years > 0) {
    issues$invalid_years <- sprintf("%d rows with year outside 2006-2012", invalid_years)
  }
  
  # Check 3: Age in plausible range (0-100, allowing NA)
  invalid_age <- data %>%
    filter(age < 0 | age > 100) %>%
    nrow()
  if (invalid_age > 0) {
    issues$invalid_age <- sprintf("%d rows with age outside 0-100", invalid_age)
  }
  
  # Check 4: Race categories valid
  invalid_race <- data %>%
    filter(!race %in% c("White", "Black", "Hispanic", "Asian", "Other", NA_character_)) %>%
    nrow()
  if (invalid_race > 0) {
    issues$invalid_race <- sprintf("%d rows with invalid race category", invalid_race)
  }

  # Check 5: Female is logical (TRUE/FALSE/NA only)
  if (class(data$female) != "logical") {
    issues$invalid_female <- "Female column is not logical type"
  }
  
  # Check 6: Coordinates present (xcoord and ycoord should not be all NA)
  missing_coords <- data %>%
    filter(is.na(xcoord) & is.na(ycoord)) %>%
    nrow()
  if (missing_coords == length(data)) {
    issues$missing_coordinates <- "All rows missing coordinates"
  }

  # Check 7: IDs are unique
  duplicate_ids <- data %>%
    group_by(id) %>%
    filter(n() > 1) %>%
    nrow()
  if (duplicate_ids > 0) {
    issues$duplicate_ids <- sprintf("%d duplicate IDs found", duplicate_ids)
  }
  
  # Return results
  list(
    passed = length(issues) == 0,
    n_issues = length(issues),
    issues = issues,
    n_rows = nrow(data),
    n_cols = ncol(data)
  )
}