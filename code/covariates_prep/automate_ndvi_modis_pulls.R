library(sf)
library(httr2)
library(tidyverse)
library(jsonlite)

resp <-
  request("https://appeears.earthdatacloud.nasa.gov/api/login") |>
  req_auth_basic(
    Sys.getenv("nasa_earth_dat_username"),
    Sys.getenv("nas_earth_dat_password")
  ) |>
  req_method("POST") |>
  req_perform()

token <- resp_body_json(resp)$token


us_boundary <- tigris::nation(year = 2010) |>
  st_transform(4326) |>
  st_simplify(dTolerance = 0.01) # simplify to keep the request lean


# Get state boundaries (excluding AK, HI, and territories)
states <- tigris::states(year = 2010, cb = TRUE) |>
  st_transform(4326) |>
  dplyr::filter(!STATE %in% c("02", "15", "60", "66", "69", "72", "78")) |>
  arrange(STATE)

# Simplify geometries to keep requests lean
states <- st_simplify(states, dTolerance = 0.01)

# We're going to break our requests over a few days :)
st_smaller <-
  states |>
  slice_head(n = 3)

#' Function to submit one state:
#'   The goal here is to loop across states
#'   to download modis ndvi and pixel clarity objects

submit_state_task <- function(st_smaller, token) {
  state_name <- st_smaller$STATE

  ## These are the arguments you supply to the API
  geo_list <- fromJSON(
    geojsonio::geojson_json(st_smaller),
    simplifyVector = FALSE
  )
  task <- list(
    task_type = "area",
    task_name = paste0("ndvi_2008_", state_name),
    params = list(
      dates = list(
        list(startDate = "01-01-2008", endDate = "12-31-2008")
      ),
      layers = list(
        list(product = "MOD13Q1.061", layer = "_250m_16_days_NDVI"),
        list(product = "MOD13Q1.061", layer = "_250m_16_days_pixel_reliability")
      ),
      output = list(
        format = list(type = "geotiff"),
        projection = "geographic"
      ),
      geo = geo_list
    )
  )

  ## These are the instruction that actually
  resp <- request("https://appeears.earthdatacloud.nasa.gov/api/task") |>
    req_auth_bearer_token(token) |>
    req_body_json(task, auto_unbox = TRUE) |>
    req_method("POST") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  ## Did our request work
  status <- resp_status(resp)

  ## Store the task id, if not throw error and tell me what broke
  if (status == 202) {
    task_id <- resp_body_json(resp)$task_id
    message("Submitted: ", state_name, " → ", task_id)
    return(tibble::tibble(
      state = state_name,
      task_id = task_id,
      status = "submitted"
    ))
  } else {
    msg <- tryCatch(resp_body_json(resp)$message, error = \(e) {
      resp_body_string(resp)
    })
    message("FAILED: ", state_name, " → ", status, " ", msg)
    return(tibble::tibble(state = state_name, task_id = NA, status = msg))
  }
}

# Execute the function
task_log <- purrr::map_dfr(1:nrow(st_smaller), function(i) {
  result <- submit_state_task(st_smaller[i, ], token)
  Sys.sleep(2) # brief pause between submissions
  result
})

# Check what worked
task_log


## Check the status of the states that we've processed
check_all_status <- function(task_log, token) {
  purrr::map_dfr(1:nrow(task_log), function(i) {
    resp <- request(paste0(
      "https://appeears.earthdatacloud.nasa.gov/api/task/",
      task_log$task_id[i]
    )) |>
      req_auth_bearer_token(token) |>
      req_perform()

    info <- resp_body_json(resp)
    tibble::tibble(
      state = task_log$state[i],
      task_id = task_log$task_id[i],
      status = info$status
    )
  })
}

# Run this periodically — no need to hammer it
check_all_status(task_log, token)


# ## If we need to recover because we got scared and canceled our function lol
# all_tasks <- request("https://appeears.earthdatacloud.nasa.gov/api/task") |>
#   req_auth_bearer_token(token) |>
#   req_perform() |>
#   resp_body_json()

# # See what's there
# task_log_recovered <- purrr::map_dfr(all_tasks, function(t) {
#   tibble::tibble(
#     task_name = t$task_name,
#     task_id = t$task_id,
#     status = t$status
#   )
# })

# task_log <- task_log_recovered |>
#   dplyr::filter(grepl("^ndvi_2008_", task_name))

## Take the tasks that we submitted, and once we're done.... download!!
fs::dir_create('data/covariates/ndvi_2008/raw')

download_task <- function(task_id, state, token) {
  # Get file listing
  files <- request(paste0(
    "https://appeears.earthdatacloud.nasa.gov/api/bundle/",
    task_id
  )) |>
    req_auth_bearer_token(token) |>
    req_perform() |>
    resp_body_json()

  tif_files <- Filter(function(f) grepl("\\.tif$", f$file_name), files$files)

  state_dir <- file.path("data/covariates/ndvi_2008/raw", state)
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)

  for (f in tif_files) {
    dest <- file.path(state_dir, basename(f$file_name))

    if (!file.exists(dest)) {
      request(paste0(
        "https://appeears.earthdatacloud.nasa.gov/api/bundle/",
        task_id,
        "/",
        f$file_id
      )) |>
        req_auth_bearer_token(token) |>
        req_perform(path = dest)

      message("Downloaded: ", state, "/", basename(f$file_name))
    }
  }
}


# Download all completed tasks
done <- check_all_status(task_log, token) |> dplyr::filter(status == "done")

purrr::walk2(done$task_id, done$state, \(tid, st) {
  download_task(tid, st, token)
  Sys.sleep(1)
})
