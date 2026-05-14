#' PURPOSE:
#'  For the Heat Study, I have processed tract level measures of
#'  fractional impervious surface and tree canopy for contiguous 2010 tracts from
#'  2007 - 2019.
#'
#'  For Anneclaire's water contaminants analysis, I need to add Alaska and Hawaii so
#'  tree canopy cover. Additionally, we need to get 2008 measures of green space for
#'  the contiguous US, Alaska & Hawaii.
#'
#'  This code connects to stable raster files hosted at the mlrc website, downloads them,
#'  and sticks them into a user specified directory for further analysis.
#'  ======================================================================

nlds_helper_funcs <-
  list.files('C:/git/nlds/Annual_NLCD/code/helper_functions', full.names = TRUE)

purrr::walk(nlds_helper_funcs, ~ source(.x))


## ==========================================================================
# Source Helper Function ----
## ==========================================================================

#' Purpose: Use {httr2} to pull down static zip files hosted on mrlc website.
#' Script Sets up path, year and file names arguments to hit data hosted at
#' "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/"
# ============================================================================

source('Annual_NLCD/code/helper_functions/download_mrlc.R')


## Here's the sparse function if you want to see it
download_mrlc

## =======================================================================
# Function Arguments ----
## =======================================================================

## Where are we sending the files?
zip_path <-
  c(
    "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Schinasi_HEAT/mrlc/data/zipped_raster_files"
  )

## What years do we want?
non_contig_names <- c('SEAK', 'HAWAII')
years <- c(2008)


## What are the name of the zip files at mrlc.gov/downloads?
tree_can_file_names <-
  stringr::str_glue('nlcd_tcc_{non_contig_names}_{years}_v2023-5_wgs84.zip')


## ========================================================================
# Call Function ------
## ========================================================================

## Download Alaska & Hawaii Tree canopy files
system.time({
  for (i in tree_can_file_names) {
    download_mrlc(
      file_name = i,
      base_url = "https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/docs/v2023-5/",
      dest_dir = zip_path
    )
  }
})
