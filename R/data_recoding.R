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
  factor(case_when(
      race_raw == "W" ~ "White",
      race_raw == "B" ~ "Black",
      race_raw %in% c("P", "Q") ~ "Hispanic",
      race_raw == "A" ~ "Asian",
      race_raw %in% c("I", "Z") ~ "Other",
      TRUE ~ NA_character_
  ), levels = c("White", "Black", "Hispanic", "Asian", "Other"))
}

