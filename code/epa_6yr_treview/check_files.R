library(tidyverse)

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

#' ==========================================================
#' Do States, systems match what's in the readme file in
#' in the resources folder???
#'
#' Theres one NA State in Arsenic associated with Ramah-Jacobs Well - RUA
#' The join will deal with it downstream
#' ==========================================================

#! States = YES (52 | 54,845)
n_distinct(tthm_file$state_code)
n_distinct(arsenic_file$state_code)

#! Systems = YES (46 | 36,691)
n_distinct(tthm_file$pwsid)
n_distinct(arsenic_file$pwsid)

#' What states / pwsids are missing from tthm?

#! IM MA NH MD DC
arsenic_file |>
  distinct(state_code) |>
  anti_join(tthm_file |> distinct(state_code))

#' ===================================================
#' Do detection limit values vary across place and system
#' sample. This is likely a small difference, but if it exists,
#' I feel like it's worth mentioning
#' ===================================================

ok <-
  arsenic_file |>
  summarise(
    med_detect_value = median(detection_limit_value),
    .by = pwsid
  )

summary(ok$med_detect_value)
