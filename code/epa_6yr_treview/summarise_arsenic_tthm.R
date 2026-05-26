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
      'source_data',
      'epa_water',
      'epa_6yr_review',
      'arsenic_clean.csv'
    )
  )

tthm_file <-
  read_csv(
    here::here(
      'data',
      'source_data',
      'epa_water',
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
    detection_limit_value = if_else(
      is.na(detection_limit_value),
      0,
      detection_limit_value
    ),
    value = if_else(is.na(value), (detection_limit_value / sqrt(2)), value)
  ) |>
  summarise(
    min = min(value, na.rm = TRUE),
    arsenic_median = median(value, na.rm = TRUE),
    arsenic_detect_level_med = median(detection_limit_value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .by = pwsid
  ) |>
  mutate(
    contam = 'arsenic',
    arsenic_unit = 'mg_l',
    arsenic_detect_unit = 'mg_l'
  )


tthm_pwsid <-
  tthm_file |>
  mutate(
    detection_limit_value = if_else(
      is.na(detection_limit_value),
      0,
      detection_limit_value
    ),
    value = if_else(is.na(value), (detection_limit_value / sqrt(2)), value)
  ) |>
  summarise(
    min = min(value, na.rm = TRUE),
    tthm_median = median(value, na.rm = TRUE),
    tthm_detect_level_med = median(detection_limit_value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .by = pwsid
  ) |>
  mutate(
    contam = 'tthm',
    tthm_unit = 'ug_l',
    tthm_detect_unit = 'ug_l'
  )

#' Combine medians (request) into 2 columns, by pwsid ---------------------

arsenic_tthm_combo <-
  arsenic_pwsid |>
  select(pwsid, arsenic_median, arsenic_unit, arsenic_detect_level_med) |>
  left_join(
    tthm_pwsid |> select(pwsid, tthm_median, tthm_unit, tthm_detect_level_med),
    by = c('pwsid')
  )

## =======================================================================
# Write summarized results  ----
## =======================================================================

write_csv(
  arsenic_tthm_combo,
  'data/covariates/epa_6yr_review/arsenic_tthm_medians.csv'
)
