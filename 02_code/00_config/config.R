#!/usr/bin/env Rscript

PROJECT_ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)

DIR <- list(
  raw_share = file.path(PROJECT_ROOT, "01_data", "01_raw", "share"),
  raw_eobs = file.path(PROJECT_ROOT, "01_data", "01_raw", "eobs"),
  raw_geographic = file.path(PROJECT_ROOT, "01_data", "01_raw", "geographic"),

  processed_italy = file.path(PROJECT_ROOT, "01_data", "02_processed", "italy"),
  processed_croatia = file.path(PROJECT_ROOT, "01_data", "02_processed", "croatia"),

  analysis_italy = file.path(PROJECT_ROOT, "01_data", "03_analysis", "italy"),
  analysis_croatia = file.path(PROJECT_ROOT, "01_data", "03_analysis", "croatia"),

  estimates = file.path(PROJECT_ROOT, "03_output", "01_estimates"),
  table_data = file.path(PROJECT_ROOT, "03_output", "02_table_data"),
  table_latex = file.path(PROJECT_ROOT, "03_output", "03_tables"),
  figures = file.path(PROJECT_ROOT, "03_output", "04_figures")
)

for (directory in DIR) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

INPUT <- list(
  italy_analysis = file.path(
    DIR$analysis_italy,
    "share_italy_birthcohort_1950_1964_with_early_life_temperature.csv"
  ),

  croatia_analysis = file.path(
    DIR$analysis_croatia,
    "share_croatia_birthcohort_1950_1964_with_early_life_temperature.csv"
  ),

  parental_education = file.path(
    DIR$raw_share,
    "wave7",
    "sharew7_rel9-0-0_gv_isced.dta"
  ),

  nuts_2024 = file.path(
    DIR$raw_geographic,
    "NUTS_RG_10M_2024_3035.gpkg"
  ),

  italy_daily_regional_temperature = file.path(
    DIR$processed_italy,
    "eobs_daily_tx_tn_1950_1965_italy_share_regions_area_weighted.csv.gz"
  )
)

assert_inputs_exist <- function(paths, description = "Required input") {

  missing <- paths[
    !file.exists(paths)
  ]

  if (length(missing) > 0L) {

    stop(
      paste0(
        description,
        " file(s) not found:\n",
        paste(
          missing,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}