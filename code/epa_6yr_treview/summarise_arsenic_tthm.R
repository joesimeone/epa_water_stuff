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


arsenic_syr4_file <-
  read_csv(
    here::here(
      'data',
      'source_data',
      'epa_water',
      'epa_6yr_review',
      'arsenic_clean_syr4.csv'
    )
  )

tthm_syr4_file <-
  read_csv(
    here::here(
      'data',
      'source_data',
      'epa_water',
      'epa_6yr_review',
      'TTHM_clean_syr4.csv'
    )
  )


## =======================================================================
# Derive Quick summaries for requested contaminants  -----
## =======================================================================

summ_contams <- function(dat, contam_name, contam_unit) {
  syr_dat <-
    dat |>
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
      median = median(value, na.rm = TRUE),
      detect_level_med = median(detection_limit_value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .by = pwsid
    ) |>
    mutate(
      contam = {{ contam_name }},
      unit = contam_unit
    )

  return(syr_dat)
}


syr_dfs <-
  list(
    'arsenic_syr4' = arsenic_syr4_file,
    'tthm_syr4' = tthm_syr4_file,
    'arsenic_syr3' = arsenic_file,
    'tthm_syr3' = tthm_file
  )

#' Derive summary stats for each year of arsenic and contaminant data
syr_pwsid_contam <-
  map2(
    syr_dfs,
    list(
      'arsenic',
      'tthm',
      'arsenic',
      'tthm'
    ),
    function(contam_dat, contam_str) {
      summ_contams(
        dat = contam_dat,
        contam_name = contam_str,
        contam_unit = 'ug_l'
      )
    }
  )

#' If a contaminant was detected at any point in time, add a dummy that says 1,
#' else 0
pwsid_detect_codes <-
  map(
    syr_dfs,
    function(contam_dat) {
      contam_dat |>
        group_by(pwsid) |>
        summarise(
          detect_dummy = if_else(any(detect == 1), 1, 0)
        )
    }
  )

#' Add list name to dummy columns so they don't collide
pwsid_detect_codes_fin <-
  pwsid_detect_codes |>
  imap(\(df, nm) {
    df |>
      rename(!!sym(str_glue('detect_dummy_{nm}')) := detect_dummy)
  }) |>
  reduce(full_join, by = "pwsid")

#' Add list name to contaminants summary data so they don't collide
syr_pwsid_contam_fin <-
  syr_pwsid_contam |>
  imap(\(df, nm) {
    df |>
      select(-contam, -unit) |>
      rename_with(\(col) paste0(col, "_", nm), .cols = -pwsid)
  }) |>
  reduce(full_join, by = "pwsid")


#' Combine contaminants medians and detection flags into one data frame.
pwsid_syr_fin <-
  syr_pwsid_contam_fin |>
  left_join(
    pwsid_detect_codes_fin,
    by = join_by(pwsid)
  )


## =======================================================================
# Stich SYR Rounds  ----
## =======================================================================
#' Simple rules:
#'   1. If both medians for arsenic of TTHM are available take a colwise
#'      median.
#'   2. If only avaialble during 1 SYR round, take the non-missing value
#' =======================================================================

arsenic_cols <- c("median_arsenic_syr3", "median_arsenic_syr4")
tthm_cols <- c("median_tthm_syr3", "median_tthm_syr4")

pwsid_syr_fin <-
  pwsid_syr_fin |>
  rowwise() |>
  mutate(
    arsenic_fin_median = median(c_across(all_of(arsenic_cols)), na.rm = TRUE),
    tthm_fin_median = median(c_across(all_of(tthm_cols)), na.rm = TRUE),
  ) |>
  ungroup()


pwsid_syr_fin <-
  pwsid_syr_fin |>
  select(pwsid, arsenic_fin_median, tthm_fin_median, contains(c('detect'))) |>
  mutate(
    arsenic_detect_flag = coalesce(
      detect_dummy_arsenic_syr3,
      detect_dummy_arsenic_syr4
    ),
    tthm_detect_flag = coalesce(detect_dummy_tthm_syr3, detect_dummy_tthm_syr4)
  ) |>
  select(
    pwsid,
    arsenic_fin_median,
    tthm_fin_median,
    arsenic_detect_flag,
    tthm_detect_flag
  )


## =======================================================================
# Write summarized results  ----
## =======================================================================

write_csv(
  pwsid_syr_fin,
  'data/covariates/epa_6yr_review/arsenic_tthm_medians.csv'
)
