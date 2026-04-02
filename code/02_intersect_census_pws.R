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

#' IMPORTANT: Need to run tract10_pws query first for the block tract_pws_query to work

tract_pws_5070_query <-
  generate_itws_query(
    tbl_name = 'tract10_pws_5070',
    lhs_geom = 'tract10_5070',
    rhs_geom = 'epa_water_v2_5070'
  )

block_group_pws_5070_query <-
  generate_itws_query(
    tbl_name = 'block10_group_pws_5070',
    lhs_geom = 'block10_group_5070',
    rhs_geom = 'epa_water_v2_5070'
  )


## V1 Queries to check
tract_pws_5070_v1_query <-
  generate_itws_query(
    tbl_name = 'tract10_pws_v1_5070',
    lhs_geom = 'tract10_5070',
    rhs_geom = 'epa_water_v1_5070'
  )

block_group_pws_v1_5070_query <-
  generate_itws_query(
    tbl_name = 'block10_group_pws_v1_5070',
    lhs_geom = 'block10_group_5070',
    rhs_geom = 'epa_water_v1_5070'
  )


# Execute Queries  -------------------------------------------------------

dbExecute(con, block_group_pws_5070_query)
dbExecute(con, tract_pws_5070_query)

dbExecute(con, tract_pws_5070_v1_query)
dbExecute(con, block_group_pws_v1_5070_query)


dbDisconnect(con)
