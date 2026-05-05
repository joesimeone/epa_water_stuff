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

download_epa_stable(
  base_url = "https://www.epa.gov/sites/default/files/2018-04/",
  file_name = 'syr3_phasechem_1_0.zip',
  tox_file = 'arsenic.txt',
  dest_dir = 'data/epa_6yr_review/arsenic_clean.csv'
)


download_epa_stable(
  base_url = "https://www.epa.gov/sites/default/files/2016-12/",
  file_name = 'syr3_thm.zip',
  tox_file = 'TTHM.txt',
  dest_dir = 'data/epa_6yr_review/TTHM_clean.csv'
)
