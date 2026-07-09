library(tidyverse)


## =======================================================================
# 1. Imports ----
## =======================================================================

# 1.1 Main Datasets ----------------------------------------------------------
contam_tract_level_f <-
  read_csv(
    here::here(
      'data',
      'results',
      'weighted_tract_level_ucmr_contam_wide.csv'
    )
  )

# contam_pws_level <-
# read_csv(
#   here::here(
#     'data',
#     'results',
#     'weighted_pws_level_ucmr_contam_wide.csv'
#   )
# )

# 1.2 ACS Covariates ---------------------------------------------------------
acs_covars <-
  read_csv(
    here::here(
      'data',
      'covariates',
      'acs5_2010',
      'acs_2010_covariates.csv'
    )
  )


## 1.3 pm25 Covariates --------------------------------------------------------

pm25_ds <-
  read_csv(
    here::here(
      'data',
      'covariates',
      'pm25_downscaler',
      'pm25_2008_tract10.csv'
    )
  )


# 1.4 Tree Canopy Cover --------------------------------------------------

tree_can <-
  read_csv(
    here::here(
      'data',
      'covariates',
      'mlrc_land_cover',
      'tract10_tree_canopy_08.csv'
    )
  )


# 1.5 Smoking Prevalence -------------------------------------------------

smoking_prev <-
  read_csv(
    here::here(
      'data',
      'covariates',
      'smoking_prevalence',
      'clean_county_smoking_estimates.csv'
    )
  )


## =======================================================================
# 2. Apply coverage flags / misc. filters to water system data ----
## =======================================================================
#' For our data, we apply a 10% population coverage threshold for the
#' tract level data. This corresponds to:
#'    Does the population served by a water system serve at least 10% of
#'    a tract's total population?
#'
#' Misc filters:
#'   Additionally, we want to the detection flags in the data for... something?
#?   Need Clarification... but here's how I have it currently
#'   1. Return all tracts where detection flag == 1 for any of the UCMR
#'      contaminants
#'   3. Could also just go contaminant by contaminant and not apply constant
#'      filter rule.
#' ===========================================================================

#' In English, is at least 10% of this tract's total population served by
#' a water system?

#' Excludes 6,243 tracts
contam_tract_level_f |>
  filter(pws_coverage_proportion <= .1) |>
  nrow()

#' In english, keep all tracts where at least one of the detect flags == 1
#' This excludes 2,157 tracts
contam_tract_level |>
  filter(!if_any(contains('detectpws'), ~ . == 1)) |>
  nrow()


# 2.1 Apply filters ------------------------------------------------------

#' Applying both filters we go from 67,102 tracts to
#' 59,741 tracts

contam_tract_fin <-
  contam_tract_level |>
  filter(pws_coverage_proportion >= .1) #|>
# filter(if_any(contains('detectpws'), ~ . == 1))

# 2.2 Sidequest - compare filtered data to original dat for coverage -----

st_tract_totals <-
  contam_tract_level |>
  mutate(
    st_fips = substr(tract_geoid, 1, 2)
  ) |>
  summarise(
    n_og = n(),
    .by = st_fips
  ) |>
  arrange(st_fips) |>
  print(n = Inf)

st_fin_cov <-
  contam_tract_fin |>
  mutate(
    st_fips = substr(tract_geoid, 1, 2)
  ) |>
  summarise(
    n_post_filter = n(),
    .by = st_fips
  ) |>
  arrange(st_fips) |>
  print(n = Inf)

st_fin_cov |>
  left_join(st_tract_totals) |>
  mutate(pct_included = n_post_filter / n_og) |>
  print(n = Inf)


## =======================================================================
# 3 Join covariates ----
## =======================================================================
#' Here, we'll join tract level covariates -  acs variables, tree canopy cover,
#' pm25 exposure. Then we'll join county level variable (smoking prevalence)

contam_tract_covars <-
  contam_tract_fin |>
  left_join(
    acs_covars,
    by = join_by(tract_geoid == GEOID)
  ) |>
  left_join(
    pm25_ds,
    by = join_by(tract_geoid == GEOID10)
  ) |>
  left_join(
    tree_can,
    by = join_by(tract_geoid == GEOID10)
  )

why <-
  contam_tract_covars |>
  filter(is.na(MEDHHINC)) |>
  mutate(
    tract_num = as.integer(substr(tract_geoid, 6, 11)),
    special_use = tract_num >= 980000, # 9800+ special-use & water
    tribal_pos = tract_num >= 940000 & tract_num < 950000
  )

pm25_ds |>
  filter(GEOID10 == '02020000101')

contam_tract_fin |>
  anti_join(
    pm25_ds,
    by = c('tract_geoid' = 'GEOID10')
  ) |>
  select(tract_geoid) |>
  mutate(
    st_fips = substr(tract_geoid, 1, 2)
  ) |>
  distinct(st_fips)

dawg <-
  acs_covars |>
  filter(is.na(MEDHHINC)) |>
  pull(GEOID)


ct_pop_joe |>
  filter(
    GEOID %in% dawg
  ) |>
  view()
