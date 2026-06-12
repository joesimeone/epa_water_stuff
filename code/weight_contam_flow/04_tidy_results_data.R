library(tidyverse)
library(duckdb)


## =======================================================================
# Imports | Weigthed dat, orginal ucmr data ----
## =======================================================================

#' Weighted tract contaminants
wgt_tract <-
  read_csv(
    here::here(
      'data',
      'results',
      'weighted_tract_level_ucmr_contam_wide.csv'
    )
  )

#' WEighted water system contaminants
wgt_pws <-
  read_csv(
    here::here(
      'data',
      'results',
      'weighted_pws_level_ucmr_contam_wide.csv'
    )
  )

#' Original ucmr data
ucmr_combo_dat <-
  haven::read_sas(
    'C:/Users/js5466/Drexel University/De Roos,Anneclaire - UCMR_CodedData-PWS_ForMDAC/ucmrcombined_pwscoded.sas7bdat'
  )


con <- dbConnect(
  duckdb(),
  'data/tract_water_system_db.duckdb',
  read_only = TRUE
)

tract10_helper <-
  tbl(con, 'weighting.us_tract10_pws_v3') |>
  distinct(st_fips, PWSID, PWS_Name, Primacy_Agency) |>
  collect()

## =======================================================================
# Pivot data long to make easire to visualize eventually -----
## =======================================================================

piv_contam_dat <- function(dat, exclusions, new_names) {
  #' Remove id columns for pivot
  vars_to_piv <-
    dat |>
    select(-all_of(exclusions)) |>
    names()

  long_dat <-
    dat |>
    pivot_longer(
      cols = vars_to_piv,
      names_to = 'measures',
      values_to = 'value'
    ) |>
    mutate(measures = str_replace(measures, "^([a-zA-Z]+)_(\\d+)", "\\1\\2")) |> # Helps w/ pattern matching on pfas vars
    separate_wider_delim(
      measures,
      delim = "_",
      names = new_names
    ) |>
    mutate(
      stat_clean = if_else(str_detect(stat, 'max'), 'max', 'median')
    ) |>
    mutate(
      contaminant_group = case_when(
        contaminant %in%
          c(
            "pfoa35",
            "pfos35",
            "pfna35",
            "pfhxs35",
            "pfbs35",
            "pfhpa35",
            "pfba",
            "pfhxa",
            "pfpea",
            "fts62"
          ) ~ "PFAS (Simplified)",
        contaminant %in%
          c(
            "chromium",
            "chromium6",
            "cobalt",
            "molybdenum",
            "strontium",
            "vanadium",
            "lithium",
            "germanium",
            "manganese"
          ) ~ "Metals",
        contaminant %in%
          c(
            "chlorate",
            "haa5",
            "haa6br",
            "haa9"
          ) ~ "Disinfection Byproducts",
        contaminant %in%
          c(
            "dichloroethane11",
            "dioxane14",
            "HCFC22",
            "Halon1011"
          ) ~ "VOCs / Solvents",
        contaminant %in%
          c(
            "dcpa",
            "perchlorate",
            "butanol1"
          ) ~ "Pesticides, Perchlorate, Other",
        TRUE ~ "Uncategorized"
      )
    )

  return(long_dat)
}

#' Call cleaning function
wgt_tract_long <-
  piv_contam_dat(
    dat = wgt_tract,
    exclusions = c(
      'tract_geoid',
      'tract_total_pop',
      'n_pws',
      'ct_pop_pws',
      'pws_coverage_proportion',
      'included'
    ),
    new_names = c(
      c("contaminant", "stat", "weighted", "sum")
    )
  ) |>
  select(-sum, -stat) |>
  filter(contaminant_group != 'Uncategorized')

wgt_pws_long <-
  wgt_pws |>
  left_join(tract10_helper, by = join_by(PWSID)) |>
  piv_contam_dat(
    exclusions = c(
      'PWSID',
      'st_fips',
      'PWS_Name',
      'Primacy_Agency'
    ),
    new_names = c(
      c("contaminant", "stat", "weighted", "sum")
    )
  ) |>
  select(-sum, -stat) |>
  filter(contaminant_group != 'Uncategorized')

ucmr_og_dat_long <-
  ucmr_combo_dat |>
  mutate(across(where(is.numeric), function(x) replace_na(x, 0))) |> # Making missings 0 instead
  left_join(tract10_helper, by = join_by(PWSID)) |>
  piv_contam_dat(
    exclusions = c(
      'PWSID',
      'st_fips',
      'PWS_Name',
      'Primacy_Agency'
    ),
    new_names = c('contaminant', 'stat')
  ) |>
  mutate(
    stat_clean = if_else(stat == 'detectpws', 'detect_level', stat_clean)
  ) |>
  select(-stat) |>
  filter(contaminant_group != 'Uncategorized')


## =======================================================================
# Add some labels  ------
## =======================================================================
#' W/in eventual shiny app, we'll want to group things by state.
#' This should help us do that for:
#'   1. States
#' ========================================================================
state_crosswalk <- tibble(
  state_fips = c(
    "01",
    "02",
    "04",
    "05",
    "06",
    "08",
    "09",
    "10",
    "11",
    "12",
    "13",
    "15",
    "16",
    "17",
    "18",
    "19",
    "20",
    "21",
    "22",
    "23",
    "24",
    "25",
    "26",
    "27",
    "28",
    "29",
    "30",
    "31",
    "32",
    "33",
    "34",
    "35",
    "36",
    "37",
    "38",
    "39",
    "40",
    "41",
    "42",
    "44",
    "45",
    "46",
    "47",
    "48",
    "49",
    "50",
    "51",
    "53",
    "54",
    "55",
    "56"
  ),
  state_abbr = c(
    "AL",
    "AK",
    "AZ",
    "AR",
    "CA",
    "CO",
    "CT",
    "DE",
    "DC",
    "FL",
    "GA",
    "HI",
    "ID",
    "IL",
    "IN",
    "IA",
    "KS",
    "KY",
    "LA",
    "ME",
    "MD",
    "MA",
    "MI",
    "MN",
    "MS",
    "MO",
    "MT",
    "NE",
    "NV",
    "NH",
    "NJ",
    "NM",
    "NY",
    "NC",
    "ND",
    "OH",
    "OK",
    "OR",
    "PA",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VT",
    "VA",
    "WA",
    "WV",
    "WI",
    "WY"
  ),
  state_name = c(
    "Alabama",
    "Alaska",
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "District of Columbia",
    "Florida",
    "Georgia",
    "Hawaii",
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming"
  )
)

wgt_tract_long <-
  wgt_tract_long |>
  mutate(st_fips = substr(tract_geoid, 1, 2)) |>
  left_join(state_crosswalk, by = join_by(st_fips == state_fips))


wgt_pws_long <-
  wgt_pws_long |>
  left_join(state_crosswalk, by = join_by(st_fips == state_fips))


ucmr_og_dat_long <-
  ucmr_og_dat_long |>
  left_join(state_crosswalk, by = join_by(st_fips == state_fips))


## =======================================================================
# Write mostly clean data ----
## =======================================================================

arrow::write_parquet(
  wgt_tract_long,
  'data/results/weighted_tract_level_long.parquet'
)

arrow::write_parquet(
  wgt_pws_long,
  'data/results/weighted_pws_level_long.parquet'
)


arrow::write_parquet(
  ucmr_og_dat_long,
  'data/results/ucmr_og_combo_long.parquet'
)
