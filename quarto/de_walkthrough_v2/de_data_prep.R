# de_data_prep.R
# -----------------------------------------------------------------------
# Extracts Delaware spatial data from DuckDB and saves as .rds files.
# Run this BEFORE rendering de_water_system_V2.qmd.
# Output: de_walkthrough/data/*.rds
# -----------------------------------------------------------------------

library(sf)
library(dplyr)
library(DBI)
library(duckdb)

# -- Connect to DuckDB ---------------------------------------------------

con <- dbConnect(
  duckdb(),
  'C:/git/epa_water_stuff/data/tract_water_system_db.duckdb',
  read_only = TRUE
)

dbExecute(con, "LOAD spatial;")
dbExecute(con, "INSTALL https;")

out_dir <- "data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -- Helper: convert WKB geometry to sf ----------------------------------

wkb_to_sf <- function(df, geom_col = "geom_wkb", crs = 5070) {
  df |>
    mutate(geometry = st_as_sfc(structure(.data[[geom_col]], class = "WKB"), crs = crs)) |>
    select(-all_of(geom_col)) |>
    st_as_sf()
}

# -- 1. EPA Water System Boundaries (Delaware) ---------------------------

epa_water_de <-
  tbl(con, "epa_water_v2_5070") |>
  filter(Primacy_Agency == "DE") |>
  collect() |>
  mutate(Shape = st_as_sfc(structure(geom_wkb, class = "WKB"), crs = 5070)) |>
  select(-geom_wkb) |>
  st_as_sf()

saveRDS(epa_water_de, file.path(out_dir, "epa_water_de.rds"))
message("Saved epa_water_de.rds")

# -- 2. 2010 Census Block Groups (Delaware) ------------------------------

block10_group_de <-
  tbl(con, "block10_group_5070") |>
  filter(st_fips == "10") |>
  collect() |>
  wkb_to_sf()

saveRDS(block10_group_de, file.path(out_dir, "block10_group_de.rds"))
message("Saved block10_group_de.rds")

# -- 3. Tract x PWS Intersection (Delaware) ------------------------------

tract10_pws_de <-
  tbl(con, "tract10_pws_5070") |>
  filter(st_fips == "10") |>
  collect() |>
  wkb_to_sf()

saveRDS(tract10_pws_de, file.path(out_dir, "tract10_pws_de.rds"))
message("Saved tract10_pws_de.rds")

# -- 4. Block Group x PWS Intersection (Delaware) ------------------------

block10_group_pws_de <-
  tbl(con, "block10_group_pws_5070") |>
  filter(st_fips == "10") |>
  collect() |>
  wkb_to_sf()

saveRDS(block10_group_pws_de, file.path(out_dir, "block10_group_pws_de.rds"))
message("Saved block10_group_pws_de.rds")

# -- 5. 2010 Census Tracts (Delaware) - tibble only ----------------------

tract10_de <-
  tbl(con, "tract10_5070") |>
  filter(substr(GEOID, 1, 2) == "10") |>
  select(GEOID, tract_total_pop = value) |>
  collect() |>
  as_tibble()

saveRDS(tract10_de, file.path(out_dir, "tract10_de.rds"))
message("Saved tract10_de.rds")

# -- Cleanup -------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)
message("Done! All .rds files saved to: ", normalizePath(out_dir))
