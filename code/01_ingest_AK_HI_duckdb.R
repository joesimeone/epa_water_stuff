## ========================================================================
# Overview | Purpose | Etc. -----
## ========================================================================

#' Right now, I've created 2 workflows - 1 for tracts 1 for blocks.
#' This is mostly to experiment. The tract workflow is borked. I haven't been able to get the
#' intersection to work out of memory
#'     1. Tracts: write_to_geoparquet + fix metadata ---> ingest into duckdb ---> pull tables from duckdb into memory, reporject and
#'        intersect
#'     2. Blocks: Pull files straight into R ---> Reproject to 5070 ---> Write to duckb ----> ????
#'
#' I'm not totally sure why the out of memory stuff is not working at the moment, but I think it has to do
#' with reprojecting from geoparquet. Hoping this block one addresses this
## =========================================================================

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

## ============================================================================
# Block files ------
## =============================================================================

## Narrow to contiguous US for now
block_files <- list.files(
  "C:/Users/js5466/OneDrive - Drexel University/Palmer,Amber's files - Preliminary_data_PWScontam/census_blocks_2010",
  full.names = TRUE
) |>
  grep("ak_|hi_", x = _, value = TRUE)

## Narrow to shape files
block_files <-
  block_files[grep('.shp', block_files)]


bg_shapefile <- st_read(
  "C:/Users/js5466/OneDrive - Drexel University/Palmer,Amber's files - Preliminary_data_PWScontam/merged_blockgroups.shp"
) |>
  mutate(st_fips = substr(GEOID, 1, 2)) |>
  filter(st_fips %in% c('02', '15'))


## ============================================================================
# Census Files  -----
## ============================================================================
tract_shp <-
  sfarrow::st_read_parquet(
    here::here(
      'data',
      'geographic_data',
      'tracts',
      'tract10_national.parquet'
    )
  ) |>
  mutate(st_fips = substr(GEOID, 1, 2)) |>
  filter(st_fips %in% c('02', '15'))


## ============================================================================
# v2 EPA Water Systems ----
## ============================================================================

## For shape & geographic operations
epa_water_v2_sf <- read_sf(
  here::here('data', 'geographic_data', 'epa_water', 'CWS_2_1.gdb')
  #options = c("OGR_ORGANIZE_POLYGONS=SKIP")
) |>
  filter(Primacy_Agency %in% c('AK', 'HI'))

epa_water_v1_sf <-
  read_sf(
    here::here('data', 'geographic_data', 'epa_water', 'epa_water_v1.gpkg')
    #options = c("OGR_ORGANIZE_POLYGONS=SKIP")
  ) |>
  filter(Primacy_Ag %in% c('Alaska', 'Hawaii'))


epa_water_v3_sf <- read_sf(
  here::here(
    'data',
    'geographic_data',
    'epa_water',
    '3_0',
    'Service_Areas_V_3_0.gpkg'
  )
  #options = c("OGR_ORGANIZE_POLYGONS=SKIP")
) |>
  filter(Primacy_Agency %in% c('AK', 'HI'))


## ===========================================================================
# First Battle | Weird import error about bad geometries  -----
## ===========================================================================

##' On import, we get a set of warnings relating to some invalid Geometries
##' Found a work around, but need to better understand what it's doing and any
##' any downstream consequences

##' Let's return the invalid geometry just so we have them somewhere

# invalid_entries <- which(!st_is_valid(epa_water_sf))
# st_is_valid(epa_water_sf, reason = TRUE)[invalid_entries]

# invalid_v1_entries <- which(!st_is_valid(epa_water_v1_sf))
# st_is_valid(epa_water_v1_sf, reason = TRUE)[invalid_v1_entries]

# invalid_boundaries <-
#   epa_water_sf[invalid_entries, ]

# invalid_v1_boundaries <-
#   epa_water_v1_sf[invalid_v1_entries, ]

validate_sys_boundaries <- function(dat) {
  fixed_geom <- lwgeom::lwgeom_make_valid(st_geometry(dat))
  st_geometry(dat) <- fixed_geom

  return(dat)
}

epa_water_v1_sf <- validate_sys_boundaries(epa_water_v1_sf)
epa_water_v2_sf <- validate_sys_boundaries(epa_water_v2_sf)
epa_water_v3_sf <- validate_sys_boundaries(epa_water_v3_sf)


map(
  list(
    epa_water_v1_sf,
    epa_water_v2_sf,
    epa_water_v3_sf
  ),
  st_crs
)

## ====================================================================
# Function | Ingest Water System & Census data as needed at crs 570 ----
## ====================================================================

#' 1. Write 5070 table to R
#' 2. Alter the table to make the duckdb understand the geometry
#' 3. Remove redundant geometry table
ingest_geoms <- function(
  source_dat,
  tbl_name,
  fips_code,
  fips_abbr,
  crs_info,
  type = 'census'
) {
  if (type == 'census') {
    contig_dat <-
      source_dat |>
      mutate(st_fips = substr(GEOID, 0, 2)) |>
      filter(st_fips == {{ fips_code }}) |>
      st_transform(crs_info) |>
      st_zm(drop = TRUE) |>
      mutate(geom_wkb = st_as_binary(geometry, hex = TRUE)) |>
      st_drop_geometry()
  } else {
    contig_dat <-
      source_dat |>
      st_transform(crs_info) |>
      filter(Primacy_Agency == {{ fips_abbr }}) |>
      st_zm(drop = TRUE) |>
      mutate(geom_wkb = st_as_binary(Shape, hex = TRUE)) |>
      st_drop_geometry()
  }

  full_tbl_name <- str_glue('{tbl_name}_{crs_info}')

  if (!full_tbl_name %in% dbListTables(con)) {
    dbWriteTable(con, full_tbl_name, contig_dat)
  } else {
    cli::cli_alert('{full_tbl_name} already written')
  }
}

## Call Function

dbListTables(con)

## Alaska Calls | What I thin that we should be using
ingest_geoms(
  source_dat = bg_shapefile,
  crs_info = 3338,
  fips_code = '02',
  tbl_name = 'ak_block10_group'
)

ingest_geoms(
  source_dat = tract_shp,
  crs_info = 3338,
  fips_code = '02',
  tbl_name = 'ak_tract10'
)


st_geometry(epa_water_v3_sf) <- "Shape"

ingest_geoms(
  source_dat = epa_water_v3_sf,
  crs_info = 3338,
  fips_abbr = 'AK',
  tbl_name = 'ak_epa_water_v3',
  type = 'pws'
)


## Hawaii Calls
ingest_geoms(
  source_dat = bg_shapefile,
  crs_info = 6628,
  fips_code = '15',
  tbl_name = 'hi_block10_group'
)

ingest_geoms(
  source_dat = tract_shp,
  crs_info = 6628,
  fips_code = '15',
  tbl_name = 'hi_tract10'
)


st_geometry(epa_water_v3_sf) <- "Shape"

ingest_geoms(
  source_dat = epa_water_v3_sf,
  crs_info = 6628,
  fips_abbr = 'HI',
  tbl_name = 'hi_epa_water_v3',
  type = 'pws'
)

dbDisconnect(con)

## ========================================================================
# Ingest Census Blocks  ----
## ========================================================================

#' Iterate over each block - population shapefile from Amber's one drive folder
#' to build a duckdb tables | Workflow goes register view ---> use view to populate main table

# block_args <-
#   basename(block_files)

# tictoc::tic()
# walk2(block_files, block_args, \(path, file) {
#   cli::cli_alert("Importing {file}:")

#   blocks_pop <-
#     read_sf(path)

#   blocks_df <- blocks_pop |>
#     mutate(geometry_wkb = st_as_binary(geometry, hex = TRUE)) |>
#     st_drop_geometry()

#   # Register as a temporary DuckDB view, convert geometry on the fly
#   duckdb_register(con, "tmp_blocks", blocks_df)

#   if (!dbExistsTable(con, "block10_from_r")) {
#     dbExecute(
#       con,
#       "
#       CREATE TABLE block10_from_r AS
#       SELECT * EXCLUDE geometry_wkb, ST_GeomFromHEXWKB(geometry_wkb) AS geom
#       FROM tmp_blocks
#     "
#     )
#   } else {
#     dbExecute(
#       con,
#       "
#       INSERT INTO block10_from_r
#       SELECT * EXCLUDE geometry_wkb, ST_GeomFromHEXWKB(geometry_wkb) AS geom
#       FROM tmp_blocks
#     "
#     )
#   }

#   duckdb_unregister(con, "tmp_blocks")
#   cli::cli_alert_success("{file} done ({nrow(blocks_df)} blocks_pop)")
# })
# tictoc::toc()

# dbDisconnect(con)

# dbDisconnect(con, shutdown = TRUE)
