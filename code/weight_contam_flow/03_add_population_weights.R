# Imports ----------------------------------------------------------------
library(sf)
library(tidyverse)
library(duckdb)


con <- dbConnect(
  duckdb(),
  'data/tract_water_system_db.duckdb',
  read_only = TRUE
)

#dbExecute(con, "LOAD spatial;")

dbListTables(con)

tbl(con, 'weighting.us_tract10_pws_v3') |> filter(st_fips == '11')

pws_dat <-
  read_csv(here::here(
    'data',
    'source_data',
    'epa_water',
    'epa_ucmr',
    'bg10_pwsid_ucmr_dat.csv'
  )) |>
  mutate(
    tract_geoid = substr(GEOID, 1, 11)
  )

ucmr_combo_dat <-
  haven::read_sas(
    'C:/Users/js5466/Drexel University/De Roos,Anneclaire - UCMR_CodedData-PWS_ForMDAC/ucmrcombined_pwscoded.sas7bdat'
  )

tract_basic <-
  tbl(con, 'weighting.us_tract10_pws_v3') |>
  select(GEOID, PWSID) |>
  collect()

## Water systems with actual contaminant info |
#' Use this as filter down stream to prevent population weight denominators from being
#' inflated.
pwsid_w_contam <-
  ucmr_combo_dat |>
  semi_join(tract_basic, by = "PWSID") |>
  distinct(PWSID) |>
  pull()

## ========================================================================
# Derive Population Estimates & Weights -----
## ========================================================================

#' Pulling in hawaii & alaska to add population estimates to tract object
ak_tract_tbl <- tbl(con, 'ak_tract10_3338') |>
  select(GEOID, tract_total_pop = value) |>
  collect()
hi_tract_tbl <- tbl(con, 'hi_tract10_3759') |>
  select(GEOID, tract_total_pop = value) |>
  collect()

## Total Tract Population (Free of any intersections) | Corresponds to Ambers {ct_pop / ct_total_pop}
ct_pop_joe <-
  tbl(con, 'tract10_5070') |>
  select(GEOID, tract_total_pop = value) |>
  collect() |>
  rbind(ak_tract_tbl, hi_tract_tbl) |>
  as_tibble() |>
  arrange(GEOID)

# =========================================================================
## Areal Interpolation Weights:
#' =========================================================================
#'   1. Divide overlap area of census block and intersected water system / block's total land area
#'   2. Summarize this to the tract & water system level
#'
#'
#' Idea: To get some sense of how the population might be distributed across water system & tract
#' boundaries

## Inputs used as the weights (I'm just excluding from the actual summary step)
## so that I can load them if I need to: call collect() on raw_wgts if need to see / verify
raw_wgts <-
  tbl(con, 'weighting.us_bg10_group_pws_v3') |>
  filter(PWSID %in% pwsid_w_contam) |>
  mutate(
    tract_geoid = substr(GEOID, 1, 11),
    wgt = overlap_area / block10_group_5070_area,
    bg_wgt_pop = value * wgt
  ) |>
  group_by(GEOID) |> # within each block
  mutate(
    wgt_sum = sum(wgt),
    wgt_normalized = if_else(wgt_sum > 1, wgt / wgt_sum, wgt) # only normalize if actually over 1
  ) |>
  ungroup() |>
  mutate(bg_wgt_pop = value * wgt_normalized)

#' Reweighted Population Scaled to tract (Corresponds to Amber's
#' PWS_dat %>% rename(pop_bg_pws = SUM_POP_BG_PWS))
pws_reweighted <-
  tbl(con, 'weighting.us_bg10_group_pws_v3') |>
  filter(PWSID %in% pwsid_w_contam) |>
  mutate(
    tract_geoid = substr(GEOID, 1, 11),
    wgt = overlap_area / block10_group_5070_area
  ) |>
  group_by(GEOID) |> # within each block
  mutate(
    wgt_sum = sum(wgt),
    wgt_normalized = if_else(wgt_sum > 1, wgt / wgt_sum, wgt) # only normalize if actually over 1
  ) |>
  ungroup() |>
  mutate(bg_wgt_pop = value * wgt_normalized) |>
  summarise(
    bg_wgt_pop = sum(bg_wgt_pop),
    total_unwgt_pop = sum(value),
    .by = c(tract_geoid, PWSID)
  ) |>
  left_join(
    tbl(con, 'weighting.us_tract10_pws_v3') |>
      select(
        GEOID,
        PWSID,
        PWS_Name,
        tract10_5070_area,
        epa_water_v3_5070_area,
        overlap_area
      ),
    by = c('tract_geoid' = 'GEOID', 'PWSID')
  ) |>
  filter(
    bg_wgt_pop != 0 ## Don't need tracts with no population. Extraneous to the analysis
  ) |>
  collect()


## =======================================================================
# Population Summaries -----
## =======================================================================
#' Using re-weighted population calculated in the last step (pws_reweighted),
#'     summarise on tract. Corresponds to an estimate of the total tract population served
#'      by each PWS (?).
#' Corresponds to Amber's {ct_pop_pws}
pws_tract_pop <-
  pws_reweighted |>
  summarise(
    n_pws = n(),
    ct_pop_pws = sum(bg_wgt_pop),
    .by = c(tract_geoid)
  )

#' Corresponds to the total population we estimate is served by a pws
pws_system_pop <-
  pws_reweighted |>
  summarise(
    n_tracts = n(),
    pws_total_pop = sum(bg_wgt_pop),
    .by = c(PWSID)
  )

## =======================================================================
# Joins | Population Estimates, Contaminants, Reweighting -----
## =======================================================================

pws_populations <-
  pws_reweighted |>
  left_join(pws_tract_pop, by = join_by(tract_geoid)) |>
  left_join(ct_pop_joe, by = join_by('tract_geoid' == 'GEOID')) |>
  left_join(pws_system_pop, by = join_by('PWSID')) |>
  mutate(
    pws_ct_total_prop = round(bg_wgt_pop / tract_total_pop, 2),
    pws_ct_pwsarea_prop = round(bg_wgt_pop / ct_pop_pws, 2)
  )

## =======================================================================
# Tract-Level Weighting -----
## =======================================================================
#' Weight contaminants by each PWS's share of the tract population,
#' then aggregate to tract.
#'
#'
chemical_rt_names <- c(
  'dcpa',
  'perchlorate',
  'dichloroethane',
  'dioxane',
  'HCFC',
  'Halon',
  'pf',
  'chlorate',
  'chrom',
  'cobalt',
  'molybdenum',
  'strontium',
  'vanadium',
  'fts',
  'lithium',
  'haa',
  'butanol',
  'germanium',
  'manganese'
)

pws_tract_contam <-
  pws_populations |>
  left_join(ucmr_combo_dat, by = join_by(PWSID)) |>
  mutate(
    wgt_flag = if_else(n_pws > 1, 1, 0),
    population_weight = bg_wgt_pop / ct_pop_pws
  ) |>
  group_by(tract_geoid, wgt_flag) |>
  mutate(
    pop_weight_sum = sum(population_weight, na.rm = TRUE),
    population_weight = if_else(
      pop_weight_sum > 1,
      population_weight / pop_weight_sum,
      population_weight
    )
  ) |>
  ungroup()

pws_tract_contam <-
  pws_tract_contam |>
  mutate(across(
    starts_with(chemical_rt_names),
    .fns = ~ .x * population_weight,
    .names = "{.col}_weighted"
  )) |>
  select(-contains('_detectpws_weighted'))

contam_tract_level <-
  pws_tract_contam |>
  group_by(tract_geoid) |>
  summarise(
    across(
      ends_with("_weighted"),
      ~ round(sum(.x, na.rm = TRUE), 2),
      .names = "{.col}_sum"
    ),
    .groups = "drop"
  ) |>
  left_join(ct_pop_joe, by = join_by(tract_geoid == GEOID)) |>
  left_join(pws_tract_pop, by = join_by(tract_geoid)) |>
  mutate(
    pws_coverage_proportion = round(ct_pop_pws / tract_total_pop, 3),
    included = if_else(pws_coverage_proportion > 0.5, "yes", "no")
  )

## =======================================================================
# PWS-Level Weighting -----
## =======================================================================
#' Weight by each tract's share of the PWS's total served population,
#' then aggregate to PWS. Note: since contaminant values are constant
#' per PWS, this recovers the original values. Useful when rolling up
#' tract-level variables to the system level.

pws_contam <-
  pws_populations |>
  left_join(ucmr_combo_dat, by = join_by(PWSID)) |>
  mutate(
    population_weight = bg_wgt_pop / pws_total_pop
  ) |>
  group_by(PWSID) |>
  mutate(
    pop_weight_sum = sum(population_weight, na.rm = TRUE),
    population_weight = if_else(
      pop_weight_sum > 1,
      population_weight / pop_weight_sum,
      population_weight
    )
  ) |>
  ungroup()

pws_contam <-
  pws_contam |>
  mutate(across(
    starts_with(chemical_rt_names),
    .fns = ~ .x * population_weight,
    .names = "{.col}_weighted"
  )) |>
  select(-contains('_detectpws_weighted'))

contam_pws_level <-
  pws_contam |>
  group_by(PWSID) |>
  summarise(
    across(
      ends_with("_weighted"),
      ~ round(sum(.x, na.rm = TRUE), 2),
      .names = "{.col}_sum"
    ),
    .groups = "drop"
  )


## =======================================================================
# write wide data for further uses ----
## =======================================================================

#' Write local
write_csv(
  contam_tract_level,
  here::here(
    'data',
    'results',
    'weighted_tract_level_ucmr_contam_wide.csv'
  )
)

write_csv(
  contam_pws_level,
  here::here(
    'data',
    'results',
    'weighted_pws_level_ucmr_contam_wide.csv'
  )
)

#' Write to AC One Drive
#' contam_pws_level

write_csv(
  contam_pws_level,
  'C:/Users/js5466/Drexel University/De Roos,Anneclaire - UCMR_CodedData-PWS_ForMDAC/weighted_pws_level_ucmr_contam_wide.csv'
)


write_csv(
  contam_tract_level,
  'C:/Users/js5466/Drexel University/De Roos,Anneclaire - UCMR_CodedData-PWS_ForMDAC/weighted_tract_level_ucmr_contam_wide.csv'
)
