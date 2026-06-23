#' PURPOSE:
#'   We have a pretty messy xlsx table from the data associated with this
#'   paper on deriving county level estimates of smoking prevalence:
#'   https://pmc.ncbi.nlm.nih.gov/articles/PMC3987818/#_ad93_
#'
#'   This script cleans them up and adds county geoids so that
#'   we can add them to our water contaminants tract level data
#'  ======================================================================

library(tidyverse)
library(readxl)
library(tigris)


# Restart R here
options(tigris_use_cache = TRUE)

co_2010_info <-
  counties(year = 2010) |>
  sf::st_drop_geometry()

co_smoking_raw <-
  readxl::read_xlsx(
    here::here(
      'data',
      'covariates',
      'smoking_prevalence',
      'county_modeled_total_smoking_prevalence.xlsx'
    ),
    sheet = 'appendix_table_3_all_smoking_re',
    skip = 3
  ) |>
  janitor::clean_names()


## =======================================================================
# Extract point estimates & confidence intervals  -----
## =======================================================================
#' Estimates are stored in a string that corresponds to a point estimate,
#' upper confidence interval and lower confidence interval. We want these
#' as numbers, this step extracts them
#' =======================================================================

co_smoking <-
  co_smoking_raw |>
  select(
    county,
    sex,
    x2008
  ) |>
  fill(county) |>
  filter(
    sex == 'Both'
  ) |>
  mutate(
    smoking_prev = str_extract(x2008, "\\d+\\.?\\d*(?=\\s*\\()"),
    smoking_prev_lower = str_extract(x2008, "(?<=\\()\\d+\\.?\\d*(?=,)"),
    smoking_prev_upper = str_extract(x2008, "(?<=,)\\d+\\.?\\d*(?=\\))")
  ) |>
  mutate(
    smoking_prev = as.numeric(smoking_prev),
    smoking_prev_lower = as.numeric(smoking_prev_lower),
    smoking_prev_upper = as.numeric(smoking_prev_upper),
    state_abb = str_extract(county, "[A-Z]{2}$")
  ) |>
  select(
    -sex,
    -x2008
  )

## =======================================================================
# Get GEOIDs for each named county ----
## =======================================================================
#' 2 Other mild issues to work throug with the data
#'    1. The paper's dataset doesn't have GEOIDs corresponding to county names.
#'       Closest corresponding field in {tigris} data is NAMELSAD10, which
#'       we'll use to get GEOID in the data
#'    2. There are a handful of counties that are combined in the dataset
#'       We'll deal with this crudely, by assigning each county the same exposure
#' ========================================================================

#' Join non-problematic counties | Fix up join key
co_smoking <-
  co_smoking |>
  mutate(
    county_name_clean = str_remove(county, ','),
    county_name_clean = str_remove(county_name_clean, state_abb),
    county_name_clean = trimws(county_name_clean)
  )

#' Add fips code and state abbreviation to tigris data
state_fips <- fips_codes |>
  as_tibble() |>
  distinct(state, state_code, state_name) |>
  rename(state_abb = state, fips = state_code)

#' Fix a few problematic keys
co_2010_info <-
  co_2010_info |>
  left_join(
    state_fips,
    by = c('STATEFP10' = 'fips')
  ) |>
  mutate(
    NAMELSAD10 = str_replace(NAMELSAD10, 'city', 'City'),
    NAMELSAD10 = str_replace(
      NAMELSAD10,
      "Do\xf1a Ana County",
      "Dona Ana County"
    )
  )


#' Join smoking prevalence with tigris data
co_smoking_indep_co <-
  co_smoking |>
  filter(
    !str_detect(county, '/')
  ) |>
  left_join(
    co_2010_info,
    by = join_by(county_name_clean == NAMELSAD10, state_abb == state_abb)
  )


#' Fix the combined counties  -----
#'
#' First, we separate each county into separate columns around the / delimiter
co_stacked <-
  co_smoking |>
  filter(str_detect(county, '/')) |>
  separate_wider_delim(
    county,
    delim = '/',
    names = c('co1', 'co2', 'co3', 'co4'),
    too_many = 'debug',
    too_few = 'align_start'
  )


#' 1. Pivot those columns long and keep county column and
#' smoking prevalence estimates
#'
#' 2. Clean up join key
#'
#' 3. Rename county so that we can combine with rest of data
co_combined_long <- co_stacked |>
  pivot_longer(
    cols = co1:co4,
    names_to = NULL,
    values_to = "county_part",
    values_drop_na = TRUE
  ) |>
  select(
    county_part,
    smoking_prev,
    smoking_prev_lower,
    smoking_prev_upper,
    state_abb
  ) |>
  mutate(
    county_name_clean = str_remove(county_part, ','),
    county_name_clean = str_remove(county_name_clean, state_abb),
    county_name_clean = trimws(county_name_clean)
  ) |>
  left_join(
    co_2010_info,
    by = join_by(county_name_clean == NAMELSAD10, state_abb == state_abb)
  ) |>
  rename(county = county_part)


co_smoking_fin <-
  rbind(co_smoking_indep_co, co_combined_long) |>
  select(
    GEOID10,
    county_name_clean,
    STATEFP10,
    smoking_prev,
    smoking_prev_lower,
    smoking_prev_upper
  )

## ==============================================================
# Export Data ---
## ==============================================================

write_csv(
  co_smoking_fin,
  'data/covariates/smoking_prevalence/clean_county_smoking_estimates.csv'
)
