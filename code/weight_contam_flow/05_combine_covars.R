#' Purpose:
#'    To join tract level UCMR contaminant exposure estimates with covariates
#'    from Anneclaire's R21 proposal. Proposal will be in root of directory I think
#'
#' Covariate Sources:
#'    1. ACS 5-year ending in 2010 (2008 as midpoint) | {tidycensus} | tract level
#'    2. Arsenic and TTHM from EPA 6-year review data 2-4 | tract level:
#'       https://www.epa.gov/dwsixyearreview/six-year-review-3-compliance-monitoring-data-2006-2011
#'    3. Tree canopy cover from MLRC Land cover dataset | tract level:
#'       https://www.mrlc.gov/
#'    4. NDVI Green space measure -- pulled down using Michelle's GEE code | tract
#'       See companion documentation in data's subdirectory for more information
#'    5. pm25 downscaler estimates from EPA | tract
#'       https://www.epa.gov/hesc/rsig-related-downloadable-data-files
#'    6. smoking prevalence estimates from Dwyer-Lindgren et al | county
#'       retrieved from https://pmc.ncbi.nlm.nih.gov/articles/PMC3987818/#_ad93_
#'
#' For more information, see readme file in root of directory and scripts used
#' to clean each of these measures. In code/covariates_prep
#' ==========================================================================

library(tidyverse)


## =======================================================================
# 1. Imports ----
## =======================================================================
covariate_dir <- here::here("data", "covariates")

files <-
  list.dirs(covariate_dir, recursive = FALSE, full.names = TRUE) |>
  purrr::map(list.files, pattern = "\\.csv$", full.names = TRUE) |>
  purrr::list_c()

files <-
  files[grep('cdc_env_tracking|arsenic_tthm_medians.csv', files, invert = TRUE)]

covar_ds <-
  map(files, read_csv) |>
  set_names(basename(files))


# 1.1 Main Datasets ----------------------------------------------------------
contam_tract_level <-
  read_csv(
    here::here(
      'data',
      'results',
      'weighted_tract_level_ucmr_contam_wide.csv'
    )
  )


# 1.2 covariates ---------------------------------------------------------
covariate_dir <- here::here("data", "covariates")

files <-
  list.dirs(covariate_dir, recursive = FALSE, full.names = TRUE) |>
  purrr::map(list.files, pattern = "\\.csv$", full.names = TRUE) |>
  purrr::list_c()

files <-
  files[grep('cdc_env_tracking|arsenic_tthm_medians.csv', files, invert = TRUE)]

covar_ds <-
  map(files, read_csv) |>
  set_names(basename(files))

## =======================================================================
# 2. Apply coverage flags / misc. filters to water system data ----
## =======================================================================
#' For our data, we apply a 10% population coverage threshold for the
#' tract level data. This corresponds to:
#'    Does the population served by a water system serve at least 10% of
#'    a tract's total population?
#' ===========================================================================

#' In English, is at least 10% of this tract's total population served by
#' a water system?

# 2.1 Apply filters ------------------------------------------------------

#' Applying both filters we go from 67,102 tracts to
#' 53,282 tracts. (Roughly 80% of initial coverage)

contam_tract_fin <-
  contam_tract_level |>
  filter(pws_coverage_proportion >= .5)


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

#' In each state, what proportion of tracts do we keep in
#' the dataset post 50% threshold filter?
st_fin_cov |>
  left_join(st_tract_totals) |>
  mutate(pct_included = n_post_filter / n_og) |>
  print(n = Inf)


# remove special use / water tacts ---------------------------------------
#' How many special use and water tracts are in our dataset after it's been
#' filtered down?

contam_tract_fin |>
  mutate(
    tract_code = as.numeric(str_sub(tract_geoid, 6, 11)),
    tract_type = case_when(
      tract_code >= 990000 & tract_code <= 999800 ~ "water",
      tract_code >= 980000 & tract_code <= 989999 ~ "special_land_use",
      tract_code >= 940000 & tract_code <= 949999 ~ "american_indian_area",
      TRUE ~ "standard"
    )
  ) |>
  summarise(
    n = n(),
    .by = tract_type
  )

#' Remove the special use tracts
contam_tract_fin <-
  contam_tract_fin |>
  mutate(
    tract_code = as.numeric(str_sub(tract_geoid, 6, 11)),
    tract_type = case_when(
      tract_code >= 990000 & tract_code <= 999800 ~ "water",
      tract_code >= 980000 & tract_code <= 989999 ~ "special_land_use",
      tract_code >= 940000 & tract_code <= 949999 ~ "american_indian_area",
      TRUE ~ "standard"
    )
  ) |>
  filter(tract_type != 'special_land_use') |>
  select(-tract_type, -tract_code)


## =======================================================================
# 3 Clean up covariates list ----
## =======================================================================
#' Here, we'll join tract level covariates -  acs variables, tree canopy cover,
#' pm25 exposure. Then we'll join county level variable (smoking prevalence)

#' Line up Michelle's ndvi tract GISJOIN code with regular tract identifier
covar_ds$`tract2010_L5L7C2L2_NDVI_mean_median_max_2008_watermask (1).csv` <-
  covar_ds$`tract2010_L5L7C2L2_NDVI_mean_median_max_2008_watermask (1).csv` |>
  mutate(
    state_fips = str_sub(GISJOIN, 2, 3), # chars 2-3 (after dropping "G")
    county_fips = str_sub(GISJOIN, 5, 7), # chars 5-7 (skip the extra "0" at position 4)
    tract_fips = str_sub(GISJOIN, 9, 14), # remaining 6 digits = tract code
    GEOID = str_c(state_fips, county_fips, tract_fips)
  )

#' Rename county smoking codes to something else not clash with tracts

covar_ds$clean_county_smoking_estimates.csv <-
  covar_ds$clean_county_smoking_estimates.csv |>
  rename(co_geoid = GEOID10)

#' Store so we can deal with joins separately
county_smoking_csv <- covar_ds$clean_county_smoking_estimates.csv

#' 3.1 Line up tract_geoid names to make reduce / left join simpler ----

covar_ds_tract <- covar_ds[
  names(covar_ds) != 'clean_county_smoking_estimates.csv'
]

tract_code_names <-
  c('GEOID', 'tract_geoid', 'GEOID10', 'GEOID', 'GEOID10')

covar_ds_tract <-
  map2(covar_ds_tract, tract_code_names, function(dat, tract_rename) {
    dat |>
      rename(tract_geoid = !!sym(tract_rename))
  })


# 3.2 Join covariates into one dataset  --------------------------------

#' Join tract level covariates into one dataframe
tract_covars_ds <-
  reduce(covar_ds_tract, ~ left_join(.x, .y, by = join_by(tract_geoid))) |>
  select(-year.x, -year.y, -state_fips, -tract_fips, -county_fips, -st_fips) |>
  mutate(co_geoid = substr(tract_geoid, 1, 5))

#' Join smoking prevalence county data to tract measures
covars_fin <-
  tract_covars_ds |>
  left_join(county_smoking_csv, by = join_by(co_geoid))


# 3.3 Deal with water tracts and special use -----------------------------

#' How many special use tracts?
covars_fin |>
  mutate(
    tract_code = as.numeric(str_sub(tract_geoid, 6, 11)),
    tract_type = case_when(
      tract_code >= 990000 & tract_code <= 999800 ~ "water",
      tract_code >= 980000 & tract_code <= 989999 ~ "special_land_use",
      tract_code >= 940000 & tract_code <= 949999 ~ "american_indian_area",
      TRUE ~ "standard"
    )
  ) |>
  summarise(n = n(), .by = tract_type)

covars_fin <-
  covars_fin |>
  mutate(
    tract_code = as.numeric(str_sub(tract_geoid, 6, 11)),
    tract_type = case_when(
      tract_code >= 990000 & tract_code <= 999800 ~ "water",
      tract_code >= 980000 & tract_code <= 989999 ~ "special_land_use",
      tract_code >= 940000 & tract_code <= 949999 ~ "american_indian_area",
      TRUE ~ "standard"
    )
  ) |>
  filter(!tract_type %in% c('water', 'special_land_use'))


# 4 Join main dataset to covariates  --------------------------------------

ucmr_exposure_fin <-
  contam_tract_fin |>
  left_join(covars_fin, by = join_by(tract_geoid)) |>
  janitor::clean_names()

ucmr_5_patterns <-
  c('fts62', 'pfba', 'pfhxa', 'pfpea')

ucmr_5_regex <- str_c("(", str_c(ucmr_5_patterns, collapse = "|"), ")")

ucmr_exposure_fin <-
  ucmr_exposure_fin |>
  rename_with(
    \(x) str_replace(x, ucmr_5_regex, "\\1_5"),
    .cols = matches(ucmr_5_regex)
  ) |>
  rename_with(\(x) str_replace(x, 'pws', 'cws'), .cols = everything())


# 5 export ---------------------------------------------------------------
write_csv(
  ucmr_exposure_fin,
  'data/results/ucmr_tract_exposure_covariates.csv',
  na = ""
)
