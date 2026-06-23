#' Purpose:
#'   We have code hammered out to intersect our community water systems,
#'   but there's an additional layer that estimates boundaries for Non-
#'   transient coummunity water systems (schools, offices etc etc )
#'
#'   This is code to ingest them into my current database. I'm a little off
#'   flow because I'm going to do both the contiguous US, Alaska & Hawaii here,
#'   but I think the asymmetry is alright because there's less going on in here
#'
#'   No census, no validation etc etc
#' =======================================================================

# Imports ----------------------------------------------------------------
library(sf)
library(tidyverse)
library(duckdb)


## ===========================================================================
# Database Pre-reqs  ----
## ===========================================================================

con <- dbConnect(duckdb(), 'data/tract_water_system_db.duckdb')

dbExecute(con, "INSTALL spatial")
dbExecute(con, "LOAD spatial;")

dbListTables(con)


epa_water_v3_tntc <- read_sf(
  here::here(
    'data',
    'source_data',
    'epa_water',
    'epa_water_bounds',
    '3_0',
    'Service_Areas_V_3_0.gpkg'
  ),
  layer = 'T_NTNC'
  #options = c("OGR_ORGANIZE_POLYGONS=SKIP")
)


ingest_geoms <- function(
  source_dat,
  tbl_name,
  fips_abbr,
  crs_info,
  type = 'contig'
) {
  if (type == 'contig') {
    dat <-
      source_dat |>
      filter(!PRIMACY_AGENCY_CODE %in% c('AK', 'HI', 'GU', 'PR')) |>
      st_transform(crs_info) |>
      st_zm(drop = TRUE) |>
      mutate(geom_wkb = st_as_binary(geom, hex = TRUE)) |>
      st_drop_geometry()
  } else {
    dat <-
      source_dat |>
      st_transform(crs_info) |>
      filter(PRIMACY_AGENCY_CODE == {{ fips_abbr }}) |>
      st_zm(drop = TRUE) |>
      mutate(geom_wkb = st_as_binary(geom, hex = TRUE)) |>
      st_drop_geometry()
  }

  full_tbl_name <- str_glue('{tbl_name}_{crs_info}')

  if (!full_tbl_name %in% dbListTables(con)) {
    dbWriteTable(con, full_tbl_name, dat)
  } else {
    cli::cli_alert('{full_tbl_name} already written')
  }
}


## =======================================================================
# Function Calls  ----
## =======================================================================

#' Contiguous Ntnc data
ingest_geoms(
  source_dat = epa_water_v3_tntc,
  crs_info = 5070,
  tbl_name = 'ntnc_v3'
)

#' Alaskan NTNC data
ingest_geoms(
  source_dat = epa_water_v3_tntc,
  fips_abbr = 'AK',
  crs_info = 3338,
  tbl_name = 'ak_ntnc_v3',
  type = 'Alaska'
)

#' Hawaiian NTNC data
ingest_geoms(
  source_dat = epa_water_v3_tntc,
  fips_abbr = 'HI',
  crs_info = 3759,
  tbl_name = 'hi_ntnc_v3',
  type = 'Hawaii'
)
