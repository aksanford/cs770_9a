# Helper functions for the MedHelper survey analysis.
# Load with: source(here::here("scripts", "functions.R"))

library(tidyverse)

#' Pivot cleaned survey data to long format
#'
#' Turns the wide cleaned data (one row per participant) into one row per
#' participant per vignette. Per-vignette columns (`v1_trust_score`,
#' `check_v1`, ...) become a `vignette` column plus one column per measure
#' (`trust_score`, `check`, ...). Participant-level columns (`ResponseId`,
#' `consent`, demographics) are repeated on each of that participant's rows.
#'
#' @param data Cleaned wide data, as produced by `clean_survey.qmd`.
#' @return A tibble with one row per participant per vignette.
pivot_vignettes_long <- function(data) {
  data |>
    # Put check columns in the same `v{n}_` prefix form as the rest
    rename_with(
      \(x) str_replace(x, "^check_v([1-3])$", "v\\1_check"),
      matches("^check_v[1-3]$")
    ) |>
    pivot_longer(
      matches("^v[1-3]_"),
      names_to = c("vignette", ".value"),
      names_pattern = "^v([1-3])_(.+)$",
      names_transform = list(vignette = as.integer)
    )
}
