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


## ===========================================================================
# Second Battle | Build US Tract - Population Parquet Files  ----
## ===========================================================================

## NOTE: This takes a while... It's pulling every US tract down from census / tigris
## Let's us iterate over states
state_args <- c(state.abb, 'DC')

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
  all_tracts,
  here::here(
    'data',
    'source_data',
    'census_boundaries',
    'tracts',
    'tract10_national.parquet'
  )
)
