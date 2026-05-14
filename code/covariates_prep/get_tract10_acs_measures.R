#' PURPOSE: Collect tract level covariates from 2010
#' 5-year ACS (Middle point = 2008, 1st year MDAC)
#'
#' Covariates Include:
#'   1. Population Density Per SQKM
#'   2. Percent of population at least high school degree
#'   3. Poverty rate (all ages)
#'   4. Median household income
#'   5. Racial Categories (SEE Race_ Categories in
#'      {tract_covariates})
#' =======================================================
library(tidycensus)
library(tidyverse)
library(sf)


## =====================================================
# ── 1. Define variables to pull ───────
## =====================================================

# Race / ethnicity (B03002: Hispanic/Latino Origin by Race)
race_vars <- c(
  race_total = "B03002_001",
  race_whitenh = "B03002_003",
  race_blacknh = "B03002_004",
  race_aiannh = "B03002_005",
  race_asiannh = "B03002_006",
  race_nhpinh = "B03002_007",
  race_othernh = "B03002_008",
  race_multinh = "B03002_009",
  race_hisp = "B03002_012"
)

# Poverty (B17001: Poverty Status by Age)
poverty_vars <- c(
  pov_total = "B17001_001",
  pov_below = "B17001_002"
)

# Median household income (B19013)
income_vars <- c(
  medhhinc = "B19013_001"
)

# Educational attainment 25+ (B15002: Sex by Educational Attainment)
edu_vars <- c(
  edu_total = "B15002_001",
  # Male HS or higher (_011 through _018)
  edu_m_hsged = "B15002_011",
  edu_m_sc1 = "B15002_012",
  edu_m_sc2 = "B15002_013",
  edu_m_assoc = "B15002_014",
  edu_m_bach = "B15002_015",
  edu_m_mast = "B15002_016",
  edu_m_prof = "B15002_017",
  edu_m_doct = "B15002_018",
  # Female HS or higher (_028 through _035)
  edu_f_hsged = "B15002_028",
  edu_f_sc1 = "B15002_029",
  edu_f_sc2 = "B15002_030",
  edu_f_assoc = "B15002_031",
  edu_f_bach = "B15002_032",
  edu_f_mast = "B15002_033",
  edu_f_prof = "B15002_034",
  edu_f_doct = "B15002_035"
)

all_vars <- c(race_vars, poverty_vars, income_vars, edu_vars)

## ==========================================================
# ── 2. Pull from ACS -----
## ==========================================================

## States to loop over
state_args <- c(state.abb, 'DC')

tract_vars <- map(state_args, \(state) {
  cli::cli_alert('importing {state}:')

  raw <- get_acs(
    state = state,
    geography = "tract",
    variables = all_vars,
    year = 2010,
    survey = "acs5",
    output = "wide" # one column per variable
  )
}) |>
  list_rbind()


## =========================================================
# ── 3. Compute derived measures ----
## =========================================================

tract_covariates <-
  tract_vars |>
  mutate(
    GEOID,
    NAME,
    race_totalE,

    # Race/ethnicity as % of total population
    RACE_WHITENH = race_whitenhE / race_totalE,
    RACE_BLACKNH = race_blacknhE / race_totalE,
    RACE_ASIANNH = race_asiannhE / race_totalE,
    RACE_HISP = race_hispE / race_totalE,
    RACE_MULTIPLENH = race_multinhE / race_totalE,
    RACE_OTHERNH = (race_aiannhE + race_nhpinhE + race_othernhE) /
      race_totalE,

    # Poverty rate (all ages)
    POV_ALLAGE = pov_belowE / pov_totalE,

    # Median household income (already a single estimate)
    MEDHHINC = medhhincE,

    # % HS graduate or higher (among pop 25+): sum male + female
    # % HS graduate or higher (among pop 25+): sum male + female
    PCT_HSPLUS = (edu_m_hsgedE +
      edu_m_sc1E +
      edu_m_sc2E +
      edu_m_assocE +
      edu_m_bachE +
      edu_m_mastE +
      edu_m_profE +
      edu_m_doctE +
      edu_f_hsgedE +
      edu_f_sc1E +
      edu_f_sc2E +
      edu_f_assocE +
      edu_f_bachE +
      edu_f_mastE +
      edu_f_profE +
      edu_f_doctE) /
      edu_totalE,
    .keep = "none"
  )

## =======================================================================
# 4 - Tack on Population Density per sqkm est ----
## =======================================================================

tract_areas <- map(state_args, \(state_abb) {
  cli::cli_alert('importing {state_abb}:')

  tigris::tracts(state = state_abb, year = 2010) |>
    sf::st_drop_geometry()
}) |>
  list_rbind()


tract_covariates <-
  tract_covariates |>
  left_join(
    tract_areas |> select(GEOID10, ALAND10),
    by = c('GEOID' = 'GEOID10')
  ) |>
  mutate(POP_DENSITY_SQKM = race_totalE / (ALAND10 / 1e6))


## =================================================
# Write tract demographic covariates ----
## =================================================

fs::dir_create('data/covariates/acs5_2010')

write_csv(
  tract_covariates,
  'data/covariates/acs5_2010/acs_2010_covariates.csv'
)
