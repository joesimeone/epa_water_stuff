# =============================================================================
# Converting tract and water boundaries geometries to parquet
# =============================================================================

#' Problem: Tract geometries and water data can be a little cumbersome to work with
#' and live in different formats and places. Water data lives in a geodatabase.
#' Tract data lives online. While we can use {sf} to deal with this, I want to try
#' to create some consistency in file type and increase speed of intersection ooperations downstream

#' Solution: Using{sfarrow}, we can write all files to parquet, which will shrink file sizes
#' and should help increase the speed of our code. Moreover, this means that we don't have
#' to keep pulling down or caching the tract data that we get from tidy census

library(tidycensus)
library(tidyverse)
library(sf)
library(geoarrow)
library(sfarrow)


## =========================================================================
# Imports -----
## =========================================================================

## For shape & geographic operations
epa_water_sf <- read_sf(
  here::here('data', 'geographic_data', 'epa_water', 'CWS_2_1.gdb'),
  options = c("OGR_ORGANIZE_POLYGONS=SKIP")
)

## For exploring variables name and stuff....
epa_water_df <-
  epa_water_sf |>
  st_drop_geometry() |>
  as_tibble() |>
  janitor::clean_names()


## ===========================================================================
# First Battle | Weird import error about bad geometries  -----
## ===========================================================================

##' On import, we get a set of warnings relating to some invalid Geometries
##' Found a work around, but need to better understand what it's doing and any
##' any downstream consequences

##' Let's return the invalid geometry just so we have them somewhere

invalid_entries <- which(!st_is_valid(epa_water_sf))
st_is_valid(epa_water_sf, reason = TRUE)[invalid_entries]

invalid_boundaries <-
  epa_water_sf[invalid_entries, ]


##' Now lets fix the issue wtih {lwgeom}
fixed_geom <- lwgeom::lwgeom_make_valid(st_geometry(epa_water_sf))
st_geometry(epa_water_sf) <- fixed_geom


## ===========================================================================
# Second Battle | Build US Tract - Population Parquet Files  ----
## ===========================================================================

## NOTE: This takes a while... It's pulling every US tract down from census / tigris
## Let's us iterate over states
state_args <- state.abb

# Accumulate all states into a single sf data frame
all_tracts <- map(state_args, \(state) {
  cli::cli_alert('importing {state}:')

  get_decennial(
    geography = "tract",
    variables = "P001001", # example variable
    state = state,
    geometry = TRUE,
    year = 2010
  )
}) |>
  list_rbind()

all_tracts <-
  all_tracts |>
  st_as_sf()

## =============================================================================
# Write Files  -----
## =============================================================================

st_write_parquet(
  epa_water_sf,
  here::here(
    'data',
    'geographic_data',
    'epa_water_boundaries_v2_fixed.parquet'
  )
)


st_write_parquet(
  all_tracts,
  here::here(
    'data',
    'geographic_data',
    'tract10_national.parquet'
  )
)
