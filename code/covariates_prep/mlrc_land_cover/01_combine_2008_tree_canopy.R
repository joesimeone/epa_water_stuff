#' PURPOSE: Get tree canopy cover for
#' tract10 2008. Comes from already processed
#' contiguous US data & alaska & hawaii datat
#' that I'm processing here
#' ========================================
library(arrow)
library(tidyverse)


## ================================================
# Imports ----
## ================================================

#' 1. Contiguous tree canopy cover data for 2008
treecan_dir <- c(
  '//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Schinasi_HEAT/mrlc/data/tract10_zonal_stats/tree_canopy'
)

treecan_ds <-
  open_dataset(treecan_dir) |>
  filter(year == 2008)


#' 3. AK & HI Rasters for zonal stats

## Zip Extension
zip_ext <- c('/vsizip')

## Directory
raster_path <-
  '//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Schinasi_HEAT/mrlc/data/zipped_raster_files'


## Full File paths ------
ak_rast_path <-
  file.path(
    zip_ext,
    raster_path,
    'nlcd_tcc_SEAK_2008_v2023-5_wgs84.zip/nlcd_tcc_seak_wgs84_v2023-5_20080101_20081231.tif'
  )

hi_rast_path <-
  file.path(
    zip_ext,
    raster_path,
    'nlcd_tcc_HAWAII_2008_v2023-5_wgs84.zip/nlcd_tcc_hawaii_wgs84_v2023-5_20080101_20081231.tif'
  )

#' Pull in raster files | I'm just going to use default mrlc
#' and capture them in commented out code below
ak_rast <-
  terra::rast(ak_rast_path)

# sf::st_crs(ak_rast)
# Coordinate Reference System:
#   User input: Albers_Conical_Equal_Area
#   wkt:
# PROJCRS["Albers_Conical_Equal_Area",
#     BASEGEOGCRS["WGS 84",
#         DATUM["World Geodetic System 1984",
#             ELLIPSOID["WGS 84",6378137,298.257223563,
#                 LENGTHUNIT["metre",1]]],
#         PRIMEM["Greenwich",0,
#             ANGLEUNIT["degree",0.0174532925199433]],
#         ID["EPSG",4326]],
#     CONVERSION["Albers Equal Area",
#         METHOD["Albers Equal Area",
#             ID["EPSG",9822]],
#         PARAMETER["Latitude of false origin",50,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8821]],
#         PARAMETER["Longitude of false origin",-154,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8822]],
#         PARAMETER["Latitude of 1st standard parallel",55,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8823]],
#         PARAMETER["Latitude of 2nd standard parallel",65,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8824]],
#         PARAMETER["Easting at false origin",0,
#             LENGTHUNIT["metre",1],
#             ID["EPSG",8826]],
#         PARAMETER["Northing at false origin",0,
#             LENGTHUNIT["metre",1],
#             ID["EPSG",8827]]],
#     CS[Cartesian,2],
#         AXIS["easting",east,
#             ORDER[1],
#             LENGTHUNIT["metre",1,
#                 ID["EPSG",9001]]],
#         AXIS["northing",north,
#             ORDER[2],
#             LENGTHUNIT["metre",1,
#                 ID["EPSG",9001]]]]

hi_rast <-
  terra::rast(hi_rast_path)

#' sf::st_crs(hi_rast)
#' Coordinate Reference System:
#   User input: Albers_Conical_Equal_Area
#   wkt:
# PROJCRS["Albers_Conical_Equal_Area",
#     BASEGEOGCRS["WGS 84",
#         DATUM["World Geodetic System 1984",
#             ELLIPSOID["WGS 84",6378137,298.257223563,
#                 LENGTHUNIT["metre",1]]],
#         PRIMEM["Greenwich",0,
#             ANGLEUNIT["degree",0.0174532925199433]],
#         ID["EPSG",4326]],
#     CONVERSION["Albers Equal Area",
#         METHOD["Albers Equal Area",
#             ID["EPSG",9822]],
#         PARAMETER["Latitude of false origin",3,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8821]],
#         PARAMETER["Longitude of false origin",-157,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8822]],
#         PARAMETER["Latitude of 1st standard parallel",8,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8823]],
#         PARAMETER["Latitude of 2nd standard parallel",18,
#             ANGLEUNIT["degree",0.0174532925199433],
#             ID["EPSG",8824]],
#         PARAMETER["Easting at false origin",0,
#             LENGTHUNIT["metre",1],
#             ID["EPSG",8826]],
#         PARAMETER["Northing at false origin",0,
#             LENGTHUNIT["metre",1],
#             ID["EPSG",8827]]],
#     CS[Cartesian,2],
#         AXIS["easting",east,
#             ORDER[1],
#             LENGTHUNIT["metre",1,
#                 ID["EPSG",9001]]],
#         AXIS["northing",north,
#             ORDER[2],
#             LENGTHUNIT["metre",1,
#                 ID["EPSG",9001]]]]

#' 2. Alaska & HI tracts for zonal stats

hi_src_tracts <-
  tigris::tracts(state = 'HI', year = 2010)


ak_src_tracts <-
  tigris::tracts(state = 'AK', year = 2010)

## =======================================================================
# Zonal Stats Function -----
## =======================================================================

#' For each tract in Hawaii & Alaska, we want to calculate a mean
#' of the pixels that fall within a given tract boundary. This
#' function does that
derive_zonal_stats <-
  function(
    raster_dat,
    shp_dat,
    measure,
    year,
    mlrc_est_name
  ) {
    ## Re-project boundary file based on raster's projection | Filter for memory & iteration
    bounds_dat <-
      shp_dat |>
      dplyr::mutate(st_fips = substr(GEOID, 1, 2)) |>
      sf::st_transform(sf::st_crs(raster_dat))

    ## Zonal stats calculation with {exactextractr} || Output = Matrix
    cli::cli_alert('Deriving Zonal Stats for FIPS: {unique(shp_dat$st_fips)}')
    pixel_summary <- exactextractr::exact_extract(
      raster_dat,
      bounds_dat,
      'mean',
      progress = FALSE
    )

    ## Store zonal stats matrix in new variable | 1 Row of Matrix = 1 Row of Data
    bounds_dat[[mlrc_est_name]] <- pixel_summary

    ## Reformat data for eventual write to parquet file
    bounds_dat <-
      bounds_dat |>
      sf::st_drop_geometry() |>
      dplyr::mutate(year = year, metric = measure) |>
      dplyr::select(GEOID, st_fips, year, metric, {{ mlrc_est_name }}) |>
      dplyr::as_tibble()

    return(bounds_dat)
  }

## Derive tree canopy estimates

# Alaska (Only South East Alaska)
ak_tree_can <-
  ak_src_tracts |>
  rename(GEOID = GEOID10) |>
  derive_zonal_stats(
    raster_dat = ak_rast,
    measure = 'tree_canopy',
    year = 2008,
    mlrc_est_name = 'tree_can'
  )

# Hawaii
hi_tree_can <-
  hi_src_tracts |>
  rename(GEOID = GEOID10) |>
  derive_zonal_stats(
    raster_dat = hi_rast,
    measure = 'tree_canopy',
    year = 2008,
    mlrc_est_name = 'tree_can'
  )


## =======================================================================
# Combine with 2008 tree canopy data ----
## =======================================================================

hi_tree_can <-
  hi_tree_can |>
  rename(GEOID10 = GEOID, measure = metric)

ak_tree_can <-
  ak_tree_can |>
  rename(GEOID10 = GEOID, measure = metric)

treecan_08 <-
  treecan_ds |>
  collect() |>
  rbind(
    hi_tree_can,
    ak_tree_can
  )


write_csv(
  treecan_08,
  here::here(
    'data',
    'covariates',
    'mlrc_land_cover',
    'tract10_tree_canopy_08.csv'
  )
)
