#' Purpose: Extract arsenic and tthm public water system readings from
#' EPA 6 year review. https://www.epa.gov/dwsixyearreview/six-year-review-3-compliance-monitoring-data-2006-2011

#https://www.epa.gov/sites/default/files/2018-04/syr3_phasechem_1_0.zip
#https://www.epa.gov/sites/default/files/2016-12/syr3_thm.zip

download_epa_stable <- function(file_name, base_url, tox_file, dest_dir) {
  temp_dir <- tempdir()
  message("Temp directory: ", temp_dir)

  temp <- file.path(temp_dir, file_name)

  if (!file.exists(temp)) {
    url <- paste0(base_url, file_name)

    httr2::request(url) |>
      httr2::req_timeout(3600) |>
      httr2::req_progress() |>
      httr2::req_perform(path = temp)

    message("Downloaded to: ", temp)
  } else {
    message("Already exists, skipping: ", temp)
  }

  # Always runs, whether freshly downloaded or already existed
  readr::read_delim(
    unz(temp, tox_file),
    delim = "\t"
  ) |>
    janitor::clean_names() |>
    readr::write_csv(
      dest_dir
    )
}


## =======================================================================
# Calls | Arsenic & TTHM ----
## =======================================================================

#' 2006 - 2011 SYR 3 Data Download
download_epa_stable(
  base_url = "https://www.epa.gov/sites/default/files/2018-04/",
  file_name = 'syr3_phasechem_1_0.zip',
  tox_file = 'arsenic.txt',
  dest_dir = 'data/source_data/epa_water/epa_6yr_review/arsenic_clean.csv'
)


download_epa_stable(
  base_url = "https://www.epa.gov/sites/default/files/2016-12/",
  file_name = 'syr3_thm.zip',
  tox_file = 'TTHM.txt',
  dest_dir = 'data/source_data/epa_water/epa_6yr_review/TTHM_clean.csv'
)

#' 2012 - 2019 SYR 4 Data Download
download_epa_stable(
  base_url = "https://www.epa.gov/system/files/other-files/2024-03/",
  file_name = '_syr4_phasechem_1-111-trichloroethane-to-atrazine.zip',
  tox_file = 'SUMMARY_ANALYTE_ARSENIC.txt',
  dest_dir = 'data/source_data/epa_water/epa_6yr_review/arsenic_clean_syr4.csv'
)


download_epa_stable(
  base_url = "https://www.epa.gov/system/files/other-files/2024-03/",
  file_name = 'syr4_thms.zip',
  tox_file = 'SYR4_THMs/TOTAL TRIHALOMETHANES (TTHM).txt',
  dest_dir = 'data/source_data/epa_water/epa_6yr_review/TTHM_clean_syr4.csv'
)
