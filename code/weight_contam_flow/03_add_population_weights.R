# Imports ----------------------------------------------------------------
library(sf)
library(tidyverse)
library(duckdb)


con <- dbConnect(
  duckdb(),
  'data/tract_water_system_db.duckdb',
  read_only = TRUE
)

dbExecute(con, "LOAD spatial;")

dbListTables(con)


PWS_dat <- read.csv(
  "C:/Users/js5466/OneDrive - Drexel University/Palmer,Amber's files - Preliminary_data_PWScontam/PWS_JOINED_BG_POP.csv"
)

pws_dat_v1 <-
  PWS_dat |>
  select(CT_GEOID, pwsid, starts_with(c('pf', 'chr', 'chl', 'mlyb'))) |>
  mutate(
    CT_GEOID = as.character(CT_GEOID),
    geo_tweak = stringr::str_pad(
      CT_GEOID,
      width = 11,
      side = "left",
      pad = "0"
    )
  )

glimpse(pws_dat_v1)

## Water systems with actual contaminant info
pwsid_w_contam <-
  pull(pws_dat_v1, pwsid)

## ========================================================================
# Derive Population Estimates & Weights -----
## ========================================================================

## Total Tract Population (Free of any intersections) | Corresponds to Ambers {ct_pop / ct_total_pop}
ct_pop_joe <-
  tbl(con, 'tract10_5070') |>
  select(GEOID, tract_total_pop = value) |>
  collect() |>
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
  tbl(con, 'block10_group_pws_5070') |>
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
  tbl(con, 'block10_group_pws_v3_5070') |>
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
    tbl(con, 'tract10_pws_v3_5070') |>
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

pws_reweighted |> filter(is.na(tract10_5070_area))

# ===========================================================================
#' Population within each tract that intersects a water system
#' ==========================================================================
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


## =======================================================================
# Joins | Population Estimates, Contaminants, Reweighting -----
## =======================================================================
#' So, now we've got a couple of different population estimates floating across
#' gegoraphies and intersections. I'm going to pull them all into one place.
#'
#' Additionally, we want to relevel our tract populations served by waters system
#' estimates so that they don't exceed 'true tract population estimates.
#'   Why: Where blocks were served by multiple systems, their population estimates were
#'        double counted in the aggregation step. We don't want that.

pws_populations <-
  pws_reweighted |>
  left_join(pws_tract_pop, by = join_by(tract_geoid)) |>
  left_join(ct_pop_joe, by = join_by('tract_geoid' == 'GEOID')) |>
  # mutate(
  #   ct_pop_pws_relevel = pmin(ct_pop_pws, tract_total_pop)
  # ) |>
  mutate(
    pws_ct_total_prop = round(bg_wgt_pop / tract_total_pop, 2), # Proportion of PWS to total CT
    pws_ct_pwsarea_prop = round(bg_wgt_pop / ct_pop_pws, 2) # Proportion of PWS to CT PWS area
  )


#' Let's add in our contaminants data using an inner_join()?
#'    Why inner? (I'm sort of guessing | Hard without the actual version of the data)
#'    - pws_populations represents tracts intersected with water systems &
#'      rescaled population estimates.
#'    - pws_dat_v1 represents all systems in the version that
#'      a) link to a tract
#'      b) have contaminant data available
#'    inner_join() returns only the systems that actually have contaminant info available?
#'
#' =====================================================================================

pws_contam <-
  pws_populations |>
  inner_join(
    pws_dat_v1,
    by = join_by(PWSID == pwsid, tract_geoid == geo_tweak)
  ) |>
  distinct(tract_geoid, PWSID, .keep_all = TRUE) |>
  mutate(
    wgt_flag = if_else(n_pws > 1, 1, 0), # Will help us determine whether we need to weight contaminants estimates or not
    population_weight = bg_wgt_pop / ct_pop_pws # Population in each tract water system / total population served by each water system
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

#' Reweight Contaminants | ----
#'   We have a set of population weights:
#'      Where a tract is served by only 1 water system ---> those weights are 1
#'      Where a tract is served by > 1 water systems ---> those weights vary somewhere between 0 and 1
#'   This step takes each contaminant value and multiplies them by those weights
#'      Contaminant values will change where a tract is served by multiple water systems,
#'      Otherwise they'll be the same as the original value (multiplied by 1)
#' ===========================================================================
pws_contam <-
  pws_contam |>
  mutate(across(
    starts_with(c('pf', 'chr', 'chl', 'mlyb')),
    .fns = ~ .x * population_weight,
    .names = "{.col}_weighted"
  ))


# why_dupes <-
#   pws_contam |>
#   filter(wgt_flag == 1) |>
#   janitor::get_dupes(tract_geoid, PWSID)

# pws_contam |>
#   filter(wgt_flag == 1) |>
#   distinct(tract_geoid, PWSID, .keep_all = TRUE) |>
#   head(5)

# a <-
#   pws_contam |>
#   filter(wgt_flag == 0) |>
#   summarise(min_wgt = min(population_weight), max_wgt = max(population_weight))

# summary(a$max_wgt)
# summary(a$min_wgt)

## ========================================================================
# Aggregate to tract  ------
## ========================================================================

#' So, We have tract - water system contaminants data.
#' We want to scale this to tract so it can cleanly be assigned to MDAC participants

contam_tract_level <-
  pws_contam |>
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
    pws_coverage_proportion = round(ct_pop_pws / tract_total_pop, 3), ## How much of a tract is served by a public water system?
    included = if_else(pws_coverage_proportion > 0.5, "yes", "no") ## Set coverage threshold at more than 50%
  )

janitor::tabyl(contam_tract_level, included)

contam_tract_level |>
  filter(is.na(included)) |>
  select(tract_geoid, ct_pop_pws, tract_total_pop, included)
