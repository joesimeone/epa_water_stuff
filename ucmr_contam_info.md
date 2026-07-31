# Tract-Level UCMR Contaminant Exposure Dataset with Covariates

## Overview

This dataset provides census tract-level estimates of drinking water contaminant
exposure from EPA's Unregulated Contaminant Monitoring Rule (UCMR) program,
linked with sociodemographic, environmental, and health-behavior covariates.
Exposure values are population-weighted using areal interpolation between public
water system (PWS) service area boundaries and 2010 census block groups.

The final analysis-ready dataset is produced by `code/weight_contam_flow/05_combine_covars.R`.

## Pipeline Summary

| Step | Script | Purpose |
|------|--------|---------|
| 0a | `00_convert_to_geoparquet.R` | Convert spatial inputs to geoparquet for DuckDB |
| 1 | `01_ingest_shapefiles_duckdb.R`, `01_ingest_AK_HI_duckdb.R`, `01c_ingest_ntncs.R` | Load census and PWS shapefiles into DuckDB |
| 2 | `02_intersect_census_pws.R` | Compute geometric intersection of census block groups and PWS boundaries in DuckDB |
| 3 | `03_add_population_weights.R` | Areal interpolation and population weighting (see details below); writes `weighted_tract_level_ucmr_contam_wide.csv` and `weighted_pws_level_ucmr_contam_wide.csv` |
| 4 | `04_tidy_results_data.R` | Pivot weighted data to long format; add contaminant group labels and state crosswalks (Used for Shiny app out of project) |
| 5 | `05_combine_covars.R` | Apply coverage and tract-type filters; join tract- and county-level covariates; produce the final analysis dataset |

## Weighting Methodology (Script 03)

Population exposure estimates are constructed through areal interpolation:

1. **Block group overlap weights** — For each block group that intersects a PWS
   boundary, compute `overlap_area / block_group_total_area`. If weights within
   a block group sum to more than 1 (multiple overlapping systems), normalize so
   they sum to 1.
2. **Weighted population** — Multiply block group population by its normalized
   weight to estimate the population served within each tract-by-PWS intersection.
3. **Tract-level population weight** — For each tract, each PWS's share of the
   tract's total served population becomes the contaminant weight:
   `population_weight = bg_wgt_pop / ct_pop_pws`. Weights are normalized within
   tract if they sum to more than 1.
4. **Weighted contaminant values** — Each contaminant's median and max PWS-level
   values are multiplied by the population weight and summed within tract.
5. **Detection flags** — Binary per-contaminant flags indicating whether *any*
   PWS serving a tract had a detection (`1 = any detection`, `0 = no detection`,
   `NA = no data`).

Only water systems with UCMR contaminant data are included in weight denominators
to prevent denominator inflation.

## Filters Applied (Script 05)

1. **PWS coverage threshold (>=50%)** — Tracts are retained only if at least 50%
   of the tract's total population is estimated to be served by one or more
   UCMR-participating public water systems (`pws_coverage_proportion >= 0.5`).
   This reduces the dataset from ~67,102 to ~53,282 tracts (~80% retention). These 
   fell from a starting number of 73,057 tracts.
2. **Special-use tract removal** — Tracts with FIPS suffix codes indicating
   special land use (980000–989999) are dropped from the contaminant data.
3. **Covariate-side tract removal** — Water tracts (990000–999800) and
   special land use tracts are also removed from the covariate table before
   the final join.

## Covariate Sources

| Covariate | Geography | Source | Time Period | Prep Script |
|-----------|-----------|--------|-------------|-------------|
| Race/ethnicity (% NH-White, NH-Black, NH-Asian, Hispanic, NH-Multiple, NH-Other) | Tract | ACS 5-year (2006–2010, midpoint 2008) via `{tidycensus}` | 2010 | `code/covariates_prep/get_tract10_acs_measures.R` |
| Poverty rate (all ages) | Tract | ACS 5-year | 2010 | same |
| Median household income | Tract | ACS 5-year | 2010 | same |
| % high school graduate or higher (25+) | Tract | ACS 5-year | 2010 | same |
| Population density (per sq km) | Tract | ACS 5-year population / `{tigris}` ALAND10 | 2010 | same |
| Tree canopy cover (%) | Tract | MRLC/NLCD Tree Canopy dataset | 2008 | `code/covariates_prep/mlrc_land_cover/01_combine_2008_tree_canopy.R` |
| NDVI (mean, median, max) | Tract | Landsat 5/7 via Michelle's Google Earth Engine code, water-masked | 2008 | `code/covariates_prep/automate_ndvi_modis_pulls.R` + GEE companion |
| PM2.5 annual mean (µg/m³) | Tract | EPA RSIG Downscaler model | 2008 | `code/covariates_prep/get_2008_pm25_data.R` |
| Arsenic & TTHM median exposure (from 6-year review) | Tract | EPA Six-Year Review compliance monitoring (SYR 2–4), population-weighted to tract | 2006–2011 | `code/epa_6yr_treview/` scripts |
| Smoking prevalence (%) | County | Dwyer-Lindgren et al. modeled estimates | 2008 | `code/covariates_prep/clean_smoking_prevalence_estimates.R` |



## Key Variables in the Final Dataset

The final dataset (`ucmr_exposure_fin`) is passed through `janitor::clean_names()`
so all column names are snake_case. It contains:

- **Tract identifier**: `tract_geoid` (11-digit 2010 Census FIPS)
- **~90 weighted contaminant columns** named `{chemical}_{stat}_weighted` where
  stat is `medpws` (median across PWS samples) or `maxpws` (maximum)
- **~45 detection flag columns** named `{chemical}_detectpws` (binary)
- **Population/coverage fields**: `tract_total_pop`, `n_pws_ucmr`, `ct_pop_pws`,
  `pws_coverage_proportion`, `prop_no_pws`
- **ACS demographics**: `race_whitenh`, `race_blacknh`, `pov_allage`, `medhhinc`, etc.
- **Environmental measures**: `tree_can`, `ndvi_mean`, `ndvi_median`, `ndvi_max`,
  `mean_pm25_value`, `mean_pm25_sd`
- **6-year review**: `arsenic_fin_median_weighted`, `tthm_fin_median_weighted`,
  plus detect flags and coverage fields
- **Smoking**: `smoking_prev`, `smoking_prev_lower`, `smoking_prev_upper` (county-level)

## Census Geography

All spatial operations use 2010 Census tract and block group boundaries. Water
system boundaries come from EPA's Safe Drinking Water Information System (SDWIS)
v3 service area polygons. Projections used: EPSG:5070 (contiguous US),
EPSG:3338 (Alaska), EPSG:3759 (Hawaii).
