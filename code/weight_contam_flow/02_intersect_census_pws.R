# Imports ----------------------------------------------------------------
library(sf)
library(tidyverse)
library(duckdb)


## ===========================================================================
# Database Pre-reqs  ----
## ===========================================================================

con <- dbConnect(
  duckdb(),
  'data/tract_water_system_db.duckdb'
)


dbExecute(con, "LOAD spatial;")

dbListTables(con)


## =======================================================================
# Intersection Function -----
## =======================================================================

generate_itws_query <- function(tbl_name, lhs_geom, rhs_geom) {
  intersect_query <-
    str_glue(
      "
CREATE TABLE {tbl_name} AS

WITH 
  bg_spatial AS (
    SELECT *, ST_MakeValid(ST_GeomFromHEXWKB(geom_wkb)) AS geom
    FROM {lhs_geom}
  ),
  ws_spatial AS (
    SELECT *, ST_MakeValid(ST_GeomFromHEXWKB(geom_wkb)) AS geom
    FROM {rhs_geom}
  )

SELECT
  bg.* EXCLUDE (geom_wkb, geom),
  ws.* EXCLUDE (geom_wkb, geom),
  ST_Area(bg.geom) AS {lhs_geom}_area,
  ST_Area(ws.geom) AS {rhs_geom}_area,
  ST_Area(ST_Intersection(bg.geom, ws.geom)) AS overlap_area,
 ST_AsHEXWKB(ST_Intersection(bg.geom, ws.geom)) AS geom_wkb 
 FROM bg_spatial bg
JOIN ws_spatial ws 
  ON ST_Intersects(bg.geom, ws.geom)"
    )

  return(intersect_query)
}


# Generate Sql strings for intersections  --------------------------------

## V3 Queries (Contiguous US)
tract_pws_5070_v3_query <-
  generate_itws_query(
    tbl_name = 'tract10_pws_v3_5070',
    lhs_geom = 'tract10_5070',
    rhs_geom = 'epa_water_v3_5070'
  )

block_group_pws_5070_v3_query <-
  generate_itws_query(
    tbl_name = 'block10_group_pws_v3_5070',
    lhs_geom = 'block10_group_5070',
    rhs_geom = 'epa_water_v3_5070'
  )

## V3 Queries (Hawaii & Alaska)
ak_tract_pws_v3_query <-
  generate_itws_query(
    tbl_name = 'ak_tract10_pws_v3_3338',
    lhs_geom = 'ak_tract10_3338',
    rhs_geom = 'ak_epa_water_v3_3338'
  )

ak_bg_pws_v3_query <-
  generate_itws_query(
    tbl_name = 'ak_block10_group_pws_v3_3338',
    lhs_geom = 'ak_block10_group_3338',
    rhs_geom = 'ak_epa_water_v3_3338'
  )

hi_tract_pws_v3_query <-
  generate_itws_query(
    tbl_name = 'hi_tract10_pws_v3_3759',
    lhs_geom = 'hi_tract10_3759',
    rhs_geom = 'hi_epa_water_v3_3759'
  )

hi_bg_pws_v3_query <-
  generate_itws_query(
    tbl_name = 'hi_block10_group_pws_v3_3759',
    lhs_geom = 'hi_block10_group_3759',
    rhs_geom = 'hi_epa_water_v3_3759'
  )


# Execute Queries  -------------------------------------------------------

## Contiguous US
if (
  !all(
    c('block10_group_pws_v3_5070', 'tract10_pws_v3_5070') %in% dbListTables(con)
  )
) {
  dbExecute(con, tract_pws_5070_v3_query)
  dbExecute(con, block_group_pws_5070_v3_query)
} else {
  cli::cli_alert_info('v3 data already written')
}


## Hawaii & Alaska
non_contig_tables <-
  c(
    'ak_tract10_pws_v3_3338',
    'ak_block10_group_pws_v3_3338',
    'hi_tract10_pws_v3_3759',
    'hi_block10_group_pws_v3_3759'
  )


## Hawaii & Alaska
if (!all(sapply(non_contig_tables, dbExistsTable, conn = con))) {
  dbExecute(con, ak_tract_pws_v3_query)
  dbExecute(con, hi_tract_pws_v3_query)
  dbExecute(con, ak_bg_pws_v3_query)
  dbExecute(con, hi_bg_pws_v3_query)
} else {
  cli::cli_alert_info('v3 data already written')
}

## =================================================================================
# Combine contiguous US w HI & AK ----
## =================================================================================
#' To simplify things and because we don't technically need the geometries anymore,
#' we're going to bind contiguous US data w/ Ak & HI. We'll do this for the intersected
#' tables for now.

dbExecute(con, "CREATE SCHEMA IF NOT EXISTS weighting;")

## List tables is schema agnostic
if (!c('us_tract10_pws_v3' %in% dbListTables(con))) {
  dbExecute(
    con,
    "CREATE TABLE weighting.us_tract10_pws_v3 AS 
SELECT * EXCLUDE (geom_wkb) FROM tract10_pws_v3_5070
UNION ALL
SELECT * EXCLUDE (geom_wkb) FROM ak_tract10_pws_v3_3338
UNION ALL
SELECT * EXCLUDE (geom_wkb) FROM hi_tract10_pws_v3_3759"
  )
} else {
  cli::cli_alert_success('table already written')
}


if (!c('us_bg10_group_pws_v3' %in% dbListTables(con))) {
  dbExecute(
    con,
    "CREATE TABLE weighting.us_bg10_group_pws_v3 AS 
SELECT * EXCLUDE (geom_wkb) FROM block10_group_pws_v3_5070
UNION ALL
SELECT * EXCLUDE (geom_wkb) FROM ak_block10_group_pws_v3_3338
UNION ALL
SELECT * EXCLUDE (geom_wkb) FROM hi_block10_group_pws_v3_3759"
  )
} else {
  cli::cli_alert_success('table already written')
}


dbDisconnect(con)
