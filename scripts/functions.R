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

#' Descriptive statistics for the outcome measures
#'
#' Summarises trust, acceptance and satisfaction, optionally within groups.
#'
#' @param data Long data with one row per participant per vignette, with
#'   `trust_score`, `accept_score` and `satisfaction` columns.
#' @param ... Grouping columns (unquoted). Leave empty for the whole sample.
#' @param digits Number of decimal places to round statistics to.
#' @return A tibble with one row per measure per group.
describe_outcomes <- function(data, ..., digits = 2) {
  data |>
    pivot_longer(
      c(trust_score, accept_score, satisfaction),
      names_to = "measure",
      values_to = "value"
    ) |>
    mutate(
      measure = factor(
        measure,
        levels = c("trust_score", "accept_score", "satisfaction"),
        labels = c("Trust", "Acceptance", "Satisfaction")
      )
    ) |>
    group_by(measure, ...) |>
    summarise(
      n = sum(!is.na(value)),
      missing = sum(is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      se = sd / sqrt(n),
      median = median(value, na.rm = TRUE),
      q1 = quantile(value, 0.25, na.rm = TRUE),
      q3 = quantile(value, 0.75, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(mean:max, \(x) round(x, digits)))
}

#' Pairwise correlations among the outcome measures
#'
#' Computes Pearson correlations (with Fisher-z 95% CIs) and Spearman
#' correlations for each pair of trust, acceptance and satisfaction, optionally
#' within groups. Observations are treated as independent; use
#' `rmcorr_outcomes()` for within-person correlations.
#'
#' @param data Long data with `trust_score`, `accept_score` and `satisfaction`.
#' @param ... Grouping columns (unquoted).
#' @param digits Number of decimal places to round to.
#' @return A tibble with one row per pair per group.
cor_outcomes <- function(data, ..., digits = 2) {
  cor_one <- function(df) {
    map(outcome_pairs(), \(p) {
      x <- df[[outcome_measures[[p[1]]]]]
      y <- df[[outcome_measures[[p[2]]]]]
      keep <- complete.cases(x, y)
      x <- x[keep]
      y <- y[keep]
      n <- length(x)
      r <- cor(x, y)
      se <- 1 / sqrt(n - 3)
      tibble(
        pair = paste(p, collapse = " – "),
        n = n,
        df = n - 2,
        pearson = r,
        conf_low = tanh(atanh(r) - qnorm(0.975) * se),
        conf_high = tanh(atanh(r) + qnorm(0.975) * se),
        spearman = cor(x, y, method = "spearman")
      )
    }) |>
      list_rbind()
  }

  data |>
    group_by(...) |>
    group_modify(\(df, key) cor_one(df)) |>
    ungroup() |>
    mutate(
      pair = factor(pair, levels = outcome_pair_labels()),
      across(c(pearson, conf_low, conf_high, spearman), \(x) round(x, digits))
    )
}

#' Within-person correlations among the outcome measures
#'
#' Repeated-measures correlations (Bakdash & Marusich, 2017) via
#' `rmcorr::rmcorr()` for each pair of trust, acceptance and satisfaction,
#' optionally within groups. The correlation is stored in `pearson` so the
#' output can be combined with `cor_outcomes()`.
#'
#' @param data Long data with one row per participant per vignette.
#' @param ... Grouping columns (unquoted).
#' @param id Participant ID column name (string).
#' @param digits Number of decimal places to round to.
#' @return A tibble with one row per pair per group, including the number of
#'   participants (`k`), observations (`n`), df, r, 95% CI and p-value.
rmcorr_outcomes <- function(data, ..., id = "ResponseId", digits = 2) {
  rm_one <- function(df) {
    df <- as.data.frame(df)
    df[[id]] <- factor(df[[id]])

    map(outcome_pairs(), \(p) {
      fit <- rmcorr::rmcorr(
        participant = id,
        measure1 = outcome_measures[[p[1]]],
        measure2 = outcome_measures[[p[2]]],
        dataset = df
      )
      tibble(
        pair = paste(p, collapse = " – "),
        k = n_distinct(df[[id]]),
        n = nrow(df),
        df = fit$df,
        pearson = fit$r,
        conf_low = fit$CI[1],
        conf_high = fit$CI[2],
        p_value = fit$p
      )
    }) |>
      list_rbind()
  }

  data |>
    group_by(...) |>
    group_modify(\(df, key) rm_one(df)) |>
    ungroup() |>
    mutate(
      pair = factor(pair, levels = outcome_pair_labels()),
      across(c(pearson, conf_low, conf_high), \(x) round(x, digits)),
      p_value = signif(p_value, digits)
    )
}

outcome_measures <- c(
  Trust = "trust_score",
  Acceptance = "accept_score",
  Satisfaction = "satisfaction"
)

outcome_pairs <- function() {
  combn(names(outcome_measures), 2, simplify = FALSE)
}

outcome_pair_labels <- function() {
  map_chr(outcome_pairs(), \(p) paste(p, collapse = " – "))
}

#' Bar chart of counts for one categorical variable
#'
#' @param data A data frame.
#' @param var Column to plot (unquoted). Factor level order is kept, so set
#'   levels beforehand for ordinal variables.
#' @param title Plot title.
#' @return A ggplot object.
plot_counts <- function(data, var, title = NULL) {
  ggplot(data, aes(y = fct_rev({{ var }}))) +
    geom_bar() +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = -0.3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = title, x = "Participants", y = NULL)
}
