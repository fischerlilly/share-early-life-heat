#!/usr/bin/env Rscript

# Validate the complete final-output package ---------------------------------
#
# This script checks file counts, expected samples, cluster counts, threshold
# coverage, map inputs, and the absence of machine-specific absolute paths.

suppressPackageStartupMessages(library(data.table))
source(file.path("02_code", "00_config", "config.R"))

table_csv <- list.files(DIR$table_data, pattern = "\\.csv$", full.names = TRUE)
table_tex <- list.files(DIR$table_latex, pattern = "\\.tex$", full.names = TRUE)
figure_png <- list.files(DIR$figures, pattern = "\\.png$", full.names = TRUE)

stopifnot(
  length(table_csv) == 5L,
  length(table_tex) == 5L,
  length(figure_png) == 5L
)

sensitivity_file <- file.path(
  DIR$estimates, "italy_tmin_18_22_health_coefficients.csv"
)

assert_inputs_exist(sensitivity_file, "Sensitivity results")
sensitivity <- fread(sensitivity_file)
stopifnot(
  nrow(sensitivity) == 15L,
  identical(sort(unique(sensitivity$threshold)), 18:22),
  all(sensitivity$clusters == 20L)
)

expected_samples <- c(
  "Self-rated health" = 1777L,
  "Maximum grip strength" = 1597L,
  "Chronic conditions" = 1771L
)

observed_samples <- unique(sensitivity[, .(outcome_label, observations)])
stopifnot(nrow(observed_samples) == 3L)
for (label in names(expected_samples)) {
  stopifnot(
    observed_samples[
      outcome_label == label,
      observations
    ] == expected_samples[[label]]
  )
}

italy_regions <- fread(
  file.path(DIR$processed_italy, "italy_share_regions.csv")
)

croatia_regions <- fread(
  file.path(DIR$processed_croatia, "croatia_share_regions.csv")
)

stopifnot(
  nrow(italy_regions) == 20L,
  nrow(croatia_regions) == 20L
)

direct_share_files <- c(
  file.path(
    DIR$processed_italy,
    "eobs_gridcell_italy_share_region_area_weights.csv"
  ),
  file.path(
    DIR$processed_italy,
    "eobs_daily_tx_tn_1950_1965_italy_share_regions_area_weighted.csv.gz"
  ),
  file.path(
    DIR$processed_croatia,
    "eobs_gridcell_croatia_share_region_area_weights.csv"
  ),
  file.path(
    DIR$processed_croatia,
    "eobs_daily_1950_1965_croatia_share_region_area_weighted.csv.gz"
  )
)

assert_inputs_exist(
  direct_share_files,
  "Direct SHARE-region exposure"
)

italy_weights <- fread(direct_share_files[1L])
croatia_weights <- fread(direct_share_files[3L])

stopifnot(
  uniqueN(italy_weights$weather_region_id) == 20L,
  uniqueN(croatia_weights$weather_region_id) == 20L
)

code_files <- list.files(
  "02_code",
  pattern = "\\.(R|py)$",
  recursive = TRUE,
  full.names = TRUE
)

code_text <- unlist(
  lapply(code_files, readLines, warn = FALSE),
  use.names = FALSE
)

machine_specific_prefix <- paste0("/", "Users", "/")

stopifnot(
  !any(
    grepl(
      machine_specific_prefix,
      code_text,
      fixed = TRUE
    )
  )
)

report <- c(
  "PASS: exactly five table CSVs and five table LaTeX files",
  "PASS: exactly five figure PNGs",
  "PASS: 15 sensitivity estimates cover thresholds 18--22 C",
  "PASS: every sensitivity model uses 20 clusters",
  "PASS: outcome-specific sample sizes match the archived results",
  "PASS: Italy and Croatia each contain 20 analytical regions",
  "PASS: E-OBS area weights are defined directly for 20 SHARE regions per country",
  "PASS: no machine-specific home-directory path appears in executable code"
)

cat(
  paste(report, collapse = "\n"),
  "\n"
)