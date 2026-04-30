library(tidyverse)

#' Purpose: Derive TTHM and arsenic medians, by EPA Public Water System
#' Data we're pulled down using {get_6yr_rev_arsenic_tthm.R}
#' From - https://www.epa.gov/dwsixyearreview/six-year-review-3-compliance-monitoring-data-2006-2011

## =======================================================================
# READ files  ----
## =======================================================================

arsenic_file <-
  read_csv(
    here::here(
      'data',
      'epa_6yr_review',
      'arsenic_clean.csv'
    )
  )

tthm_file <-
  read_csv(
    here::here(
      'data',
      'epa_6yr_review',
      'TTHM_clean.csv'
    )
  )


## =======================================================================
# Derive Quick summaries for requested contaminants  -----
## =======================================================================
arsenic_pwsid <-
  arsenic_file |>
  mutate(
    value = replace_na(value, 0)
  ) |>
  summarise(
    min = min(value, na.rm = TRUE),
    arsenic_median = median(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .by = pwsid
  ) |>
  mutate(
    contam = 'arsenic',
    arsenic_unit = 'mg_l'
  )


tthm_pwsid <-
  tthm_file |>
  mutate(
    value = replace_na(value, 0)
  ) |>
  summarise(
    min = min(value, na.rm = TRUE),
    tthm_median = median(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .by = pwsid
  ) |>
  mutate(
    contam = 'tthm',
    tthm_unit = 'ug_l'
  )

#' Combine medians (request) into 2 columns, by pwsid ---------------------

arsenic_tthm_combo <-
  arsenic_pwsid |>
  select(pwsid, arsenic_median, arsenic_unit) |>
  left_join(
    tthm_pwsid |> select(pwsid, tthm_median, tthm_unit),
    by = c('pwsid')
  )

## =======================================================================
# Write summarized results  ----
## =======================================================================
