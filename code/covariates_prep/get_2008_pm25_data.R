#' PURPOSE: Roll Up contiguous United States pm25
#' estimates into tract level averages
#'
#' Raw data comes from here:
#'   https://www.epa.gov/hesc/rsig-related-downloadable-data-files#faqsd
#'
#' Stored at //files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Schinasi_HEAT/Measures/Fused_Air_Quality_Surface_Downscaling
#' ============================================================
library(duckdb)
library(tidyverse)
library(sf)

## Where zip file live
raw_08_ds_dir <-
  '//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Schinasi_HEAT/Measures/Fused_Air_Quality_Surface_Downscaling/2008_pm25_daily_average.txt.gz'

#' Can just use memory to perform this operation (annual averages by tract )
con <- dbConnect(duckdb(), ':memory:')


#' SQL Query, store means in pm25_tracts_src object
pm25_tracts_src <-
  dbGetQuery(
    con,
    str_glue(
      "SELECT 
    FIPS as GEOID10,
    AVG(CAST(\"pm25_daily_average(ug/m3)\" AS DOUBLE)) AS mean_pm25_value,
    AVG(CAST(\"pm25_daily_average_stderr(ug/m3)\" AS DOUBLE)) AS mean_pm25_sd 
  FROM read_csv('{raw_08_ds_dir}', delim=',')
  GROUP BY FIPS"
    )
  ) |>
  as_tibble()


## =======================================================================
# Write Results  ----
## =======================================================================

write_csv(
  pm25_tracts_src,
  'data/covariates/pm25_downscaler/pm25_2008_tract10.csv'
)
