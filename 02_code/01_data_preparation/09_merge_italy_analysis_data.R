#!/usr/bin/env Rscript

# Construct the Italian analysis file by merging direct SHARE-region exposure
# by early-life region and birth month.
# Inputs and outputs are defined through the shared project configuration.

source(file.path("02_code", "00_config", "config.R"))
source(file.path("02_code", "00_config", "share_regions.R"))

suppressPackageStartupMessages({
  library(data.table)
})

progress <- function(step, text) {
  cat(sprintf("[%s] %s %s\n", format(Sys.time(), "%H:%M:%S"), step, text))
  flush.console()
}

progress("START", "Starting construction of the analysis data.")

share_file <- file.path(
  DIR$processed_italy,
  "easyshare_italy_with_birth_region_birth_cohort_1950_1964.csv"
)

temperature_12m_file <- file.path(
  DIR$processed_italy,
  "eobs_first12months_temperature_threshold_days_birthmonths_1950_1964_share_regions.csv"
)

output_dir <- DIR$analysis_italy

output_file <- file.path(
  output_dir,
  "share_italy_birthcohort_1950_1964_with_early_life_temperature.csv"
)

for (required_file in c(share_file, temperature_12m_file)) {
  if (!file.exists(required_file)) stop("Input file not found: ", required_file)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

progress("STEP 1/6", "Reading SHARE and twelve-month temperature data.")

share <- fread(
  share_file,
  na.strings = c("", "NA"),
  showProgress = FALSE
)

temperature_12m <- fread(
  temperature_12m_file,
  na.strings = c("", "NA"),
  showProgress = FALSE
)

required_share <- c(
  "mergeid",
  "wave",
  "dn002_mod",
  "dn003_mod",
  "birth_region_code",
  "birth_region_label"
)

if (!all(required_share %in% names(share))) {
  stop("Required merge variables are missing from the SHARE file.")
}

required_temperature <- c(
  "weather_region_id",
  "birth_year_month",
  "temperature_variable",
  "days_ge_20",
  "days_ge_30",
  "coverage_complete"
)

if (!all(required_temperature %in% names(temperature_12m))) {
  stop("Required variables are missing from the twelve-month temperature file.")
}

share[, birth_year_month := sprintf(
  "%04d-%02d",
  as.integer(dn003_mod),
  as.integer(dn002_mod)
)]

if (anyDuplicated(share[, .(mergeid, wave)])) {
  stop("The SHARE file contains duplicate person-wave observations.")
}

if (any(!grepl(
  "^(195[0-9]|196[0-4])-(0[1-9]|1[0-2])$",
  share$birth_year_month
))) {
  stop("At least one SHARE birth month is outside 1950-01 through 1964-12.")
}

progress("STEP 2/6", "Attaching the validated SHARE-region identifier.")

region_crosswalk <- copy(italy_share_regions)[, .(
  birth_region_code,
  weather_region_id
)]

if (!setequal(
  unique(share$birth_region_code),
  region_crosswalk$birth_region_code
)) {
  missing_mapping <- setdiff(
    unique(share$birth_region_code),
    region_crosswalk$birth_region_code
  )

  stop(
    "Unmatched SHARE region codes: ",
    paste(missing_mapping, collapse = ", ")
  )
}

share <- region_crosswalk[
  share,
  on = "birth_region_code"
]

if (anyNA(share$weather_region_id)) {
  stop("At least one birth region was not assigned a weather region.")
}

progress("STEP 3/6", "Validating direct SHARE-region exposure.")

exposure_long <- copy(temperature_12m)

if (uniqueN(exposure_long$weather_region_id) != 20L) {
  stop("The temperature table does not contain the expected 20 SHARE regions.")
}

if (!all(exposure_long$coverage_complete)) {
  stop("At least one twelve-month exposure window is incomplete.")
}

progress("STEP 4/6", "Creating the two main temperature exposure variables.")

tn_exposure <- exposure_long[
  temperature_variable == "tn_mean",
  .(
    weather_region_id,
    birth_year_month,
    tn_days_ge_20_first12m = days_ge_20
  )
]

tx_exposure <- exposure_long[
  temperature_variable == "tx_mean",
  .(
    weather_region_id,
    birth_year_month,
    tx_days_ge_30_first12m = days_ge_30
  )
]

if (anyDuplicated(tn_exposure[, .(weather_region_id, birth_year_month)])) {
  stop("The Tmin exposure table contains duplicate merge keys.")
}

if (anyDuplicated(tx_exposure[, .(weather_region_id, birth_year_month)])) {
  stop("The Tmax exposure table contains duplicate merge keys.")
}

exposure_wide <- merge(
  tn_exposure,
  tx_exposure,
  by = c("weather_region_id", "birth_year_month"),
  all = TRUE
)

expected_exposure_columns <- c(
  "tn_days_ge_20_first12m",
  "tx_days_ge_30_first12m"
)

if (!all(expected_exposure_columns %in% names(exposure_wide))) {
  stop("The two expected temperature variables were not created.")
}

if (anyDuplicated(
  exposure_wide[, .(weather_region_id, birth_year_month)]
)) {
  stop("The wide temperature table contains duplicate merge keys.")
}

progress("STEP 5/6", "Merging exposure onto each person-wave record.")

rows_before <- nrow(share)

analysis_data <- exposure_wide[
  share,
  on = .(weather_region_id, birth_year_month)
]

if (nrow(analysis_data) != rows_before) {
  stop("The merge changed the number of person-wave rows.")
}

if (anyDuplicated(analysis_data[, .(mergeid, wave)])) {
  stop("The merge created duplicate person-wave observations.")
}

share_columns <- names(share)

setcolorder(
  analysis_data,
  c(
    share_columns,
    setdiff(names(analysis_data), share_columns)
  )
)

setorder(analysis_data, mergeid, wave)

progress("STEP 6/6", "Checking the analysis data and writing the CSV file.")

exposure_consistency <- analysis_data[
  ,
  lapply(.SD, uniqueN, na.rm = FALSE),
  by = mergeid,
  .SDcols = expected_exposure_columns
]

if (any(
  as.matrix(
    exposure_consistency[, ..expected_exposure_columns]
  ) > 1L
)) {
  stop("At least one respondent received different exposures across waves.")
}

fwrite(
  analysis_data,
  output_file,
  na = ""
)

progress(
  "DONE",
  paste0("File saved: ", output_file)
)

cat(
  "Respondents:",
  uniqueN(analysis_data$mergeid),
  "\n"
)

cat(
  "Person-wave rows:",
  nrow(analysis_data),
  "\n"
)

cat(
  "SHARE birth regions:",
  uniqueN(analysis_data$birth_region_code),
  "\n"
)

cat(
  "Added temperature variables:",
  length(expected_exposure_columns),
  "\n"
)

cat(
  "Rows with at least one missing temperature indicator:",
  sum(
    !complete.cases(
      analysis_data[, ..expected_exposure_columns]
    )
  ),
  "\n"
)