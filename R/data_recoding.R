#' Recode raw race codes to standardized categories
#'
#' Converts NYPD's single-letter race codes to full category names.
#' Based on NYPD Stop, Question and Frisk Database codebook.
#'
#' @param race_raw Character vector of raw race codes (W, B, P, Q, A, I, Z)
#' @return Factor with levels: White, Black, Hispanic, Asian, Other
#'
#' @details
#' Mapping:
#' - W = White
#' - B = Black  
#' - P, Q = Hispanic
#' - A = Asian
#' - I, Z = Other
#' - Invalid codes = NA
#'
#' @examples
#' recode_race(c("W", "B", "P", "A"))
#' recode_race(c("Q", "X", "Z"))  # X becomes NA
recode_race <- function(race_raw) {
  return(factor(case_when(
      race_raw == "W" ~ "White",
      race_raw == "B" ~ "Black",
      race_raw %in% c("P", "Q") ~ "Hispanic",
      race_raw == "A" ~ "Asian",
      race_raw %in% c("I", "Z") ~ "Other",
      TRUE ~ NA_character_
  ), levels = c("White", "Black", "Hispanic", "Asian", "Other")))
}

#' Parse SQF date and time fields
#'
#' Combines separate date and time fields into a single datetime.
#' Handles multiple date formats used across different years.
#'
#' @param datestop Character vector of dates (various formats)
#' @param timestop Character/numeric vector of times (24-hour, 0-padded)
#' @param date_format Character, format hint: "Ymd" or "mdY"
#' @return POSIXct datetime vector
#'
#' @examples
#' parse_sqf_datetime("2006-01-15", "1430", "Ymd")
#' parse_sqf_datetime("01152007", "830", "mdY")
parse_sqf_datetime <- function(datestop, timestop, date_format = "Ymd") {
  # TODO: Return to the sentinel dates after validation
      # Remove invalid sentinel dates (e.g., "1900-12-31", "12311900")
      #datestop <- if_else(datestop %in% c("1900-12-31", "12311900"), NA_character_, datestop)
  
  # Pad timestop to 4 characters
  timestop <- str_pad(timestop, width = 4, pad = "0")
  # Combine date and time strings
  datetime <- paste(datestop, timestop, sep=" ")
  order <- paste(date_format, " HM")
  # Parse with lubridate::parse_date_time()
  return(parse_date_time(datetime, order, "EST"))
}

#' Clean age variable
#'
#' Converts age to integer and replaces invalid sentinel values with NA.
#' NYPD used various codes for missing/invalid age.
#'
#' @param age_raw Character or numeric vector of raw ages
#' @return Integer vector with invalid values as NA
#'
#' @details
#' Sentinel values replaced with NA:
#' - 99 (missing)
#' - 377 (invalid)
#' - 999 (unknown)
#'
#' @examples
#' clean_age(c("25", "30", "99", "377"))
clean_age <- function(age_raw) {
  age_int <- as.integer(age_raw)
  age_int <- na_if(age_int, 99)
  age_int <- na_if(age_int, 377)
  age_int <- na_if(age_int, 999)
  return(age_int)
}

