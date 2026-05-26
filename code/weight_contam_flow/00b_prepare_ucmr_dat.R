library(duckdb)
library(tidyverse)
library(haven)

#' epa v3 water system boundaries
con <- dbConnect(
  duckdb(),
  'data/tract_water_system_db.duckdb',
  read_only = TRUE
)

#' ucmr data from anneclaire
ucmr_combo_dat <-
  haven::read_sas(
    'C:/Users/js5466/Drexel University/De Roos,Anneclaire - UCMR_CodedData-PWS_ForMDAC/ucmrcombined_pwscoded.sas7bdat'
  )


v3_bg10_pws_dat <-
  tbl(con, 'weighting.us_bg10_group_pws_v3') |>
  select(GEOID, st_fips, PWSID, PWS_Name, Primacy_Agency)


## =======================================================================
# Some gut checks for join ----
## =======================================================================

#' 11707 initial public water systems
n_distinct(ucmr_combo_dat$PWSID)

#' 44,130 public water systems intersected with 207,044 census blocks
v3_bg10_pws_dat |>
  summarise(n_distinct(PWSID), n_distinct(GEOID)) |>
  collect()


# Join in database-backend first -----------------------------------------

dbWriteTable(
  con,
  'ucmr_combo_dat',
  ucmr_combo_dat,
  overwrite = TRUE,
  temporary = TRUE
)

bg10_ucmr_dat <-
  tbl(con, 'ucmr_combo_dat') |>
  inner_join(v3_bg10_pws_dat, by = c('PWSID'))


#' After join, we have 10,627 (down from 44,130 | down from 11,707) PWSID's & 190,060 census blocks (down from 207,044)
bg10_ucmr_dat |>
  summarise(n_distinct(PWSID), n_distinct(GEOID)) |>
  collect()


# ! Seems plausible to me

## =======================================================================
# Write data to csv to slot into weight_contam_flow  -----
## =======================================================================

bg10_ucmr_dat |>
  collect() |>
  write_csv(
    here::here(
      'data',
      'source_data',
      'epa_water',
      'epa_ucmr',
      'bg10_pwsid_ucmr_dat.csv'
    )
  )
