library(DBI)
library(odbc)
library(tidyverse)

con <- dbConnect(
  odbc(),
  .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/git/epa_water_stuff/data/source_data/epa_water/epa_6yr_review/zipped/Arsenic_Chem1005.mdb;"
)

ars_out <- dbReadTable(con, "Arsenic_Chem1005")

glimpse(ars_out)


syr_dat <-
  ars_out |>
  summarise(
    min_arsenic_syr2 = min(VALUE, na.rm = TRUE),
    median_arsenic_syr2 = median(VALUE, na.rm = TRUE),
    mean_arsenic_syr2 = mean(VALUE, na.rm = TRUE),
    max_arsenic_syr2 = max(VALUE, na.rm = TRUE),
    sd_arsenic_syr2 = sd(VALUE, na.rm = TRUE),
    .by = PWSID
  ) |>
  mutate(
    contam = 'arsenic',
    unit = 'mg/l'
  )


syr_detect <-
  ars_out |>
  summarise(
    arsenic_detect_flag_syr2 = max(DETECT),
    .by = PWSID
  )


syr_dat_fin <-
  syr_dat |>
  select(PWSID, median_arsenic_syr2) |>
  left_join(syr_detect)


write_csv(
  syr_dat_fin,
  'data/source_data/epa_water/epa_6yr_review/syr2_arsenic.csv'
)
