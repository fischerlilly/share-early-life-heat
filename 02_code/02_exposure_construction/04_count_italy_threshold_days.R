#!/usr/bin/env Rscript

# Count regional days per month at or above prespecified temperature thresholds.
#
# Exposure definitions:
#   Tmin (tn_mean):
#     >= 20°C = main exposure
#
#   Tmax (tx_mean):
#     >= 30°C = alternative daytime heat exposure
#
# Mean temperature (tg_mean) is not used.
#
# Inputs and outputs are defined through the shared project configuration.

source(file.path("02_code", "00_config", "config.R"))


suppressPackageStartupMessages({
  library(data.table)
})


progress <- function(step, text) {
  cat(sprintf(
    "[%s] %s %s\n",
    format(Sys.time(), "%H:%M:%S"),
    step,
    text
  ))
  flush.console()
}


progress("START", "Starting monthly threshold counts.")


# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

input_file <- file.path(
  DIR$processed_italy,
  "eobs_daily_tx_tn_1950_1965_italy_share_regions_area_weighted.csv.gz"
)

output_dir <- DIR$processed_italy

output_file <- file.path(
  output_dir,
  "eobs_monthly_temperature_threshold_days_1950_1965_italy_share_regions.csv"
)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# -------------------------------------------------------------------------
# STEP 1: Read daily regional temperature data
# -------------------------------------------------------------------------

progress(
  "STEP 1/4",
  "Reading daily SHARE-region temperature data."
)

input_connection <- gzfile(input_file, open = "rt")

daily <- as.data.table(
  read.csv(
    input_connection,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
)

close(input_connection)


# Only variables actually needed from this point onward
required_columns <- c(
  "date",
  "weather_region_id",
  "weather_region_name",
  "tn_mean",
  "tx_mean"
)

if (!all(required_columns %in% names(daily))) {
  stop("Required columns are missing from the input file.")
}


daily[, date := as.IDate(date)]

if (anyNA(daily$date)) {
  stop("At least one date could not be parsed.")
}

if (anyDuplicated(daily[, .(date, weather_region_id)])) {
  stop("The input contains duplicate day-region combinations.")
}


daily[, `:=`(
  year = year(date),
  month = month(date),
  year_month = format(date, "%Y-%m")
)]


progress(
  "STEP 1/4",
  paste0(
    format(
      nrow(daily),
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE
    ),
    " day-region rows loaded."
  )
)


# -------------------------------------------------------------------------
# STEP 2: Reshape Tmin and Tmax
# -------------------------------------------------------------------------

progress(
  "STEP 2/4",
  "Reshaping Tmin and Tmax to a common long format."
)


long <- melt(
  daily,
  id.vars = c(
    "date",
    "year",
    "month",
    "year_month",
    "weather_region_id",
    "weather_region_name"
  ),
  measure.vars = c(
    "tn_mean",
    "tx_mean"
  ),
  variable.name = "temperature_variable",
  value.name = "temperature_c"
)


count_or_na <- function(x, threshold) {
  if (all(is.na(x))) {
    return(NA_integer_)
  }

  as.integer(
    sum(x >= threshold, na.rm = TRUE)
  )
}


# -------------------------------------------------------------------------
# STEP 3: Count threshold days
# -------------------------------------------------------------------------

progress(
  "STEP 3/4",
  "Counting Tmin and Tmax threshold days."
)


# Tmin:
# 20°C is the main threshold.

monthly_tn <- long[
  temperature_variable == "tn_mean",
  .(
    days_in_month = uniqueN(date),
    days_available = sum(!is.na(temperature_c)),

    days_ge_20 = count_or_na(temperature_c, 20)
  ),
  by = .(
    weather_region_id,
    weather_region_name,
    year,
    month,
    year_month,
    temperature_variable
  )
]


# Tmax:
# 30°C is the prespecified daytime heat threshold.

monthly_tx <- long[
  temperature_variable == "tx_mean",
  .(
    days_in_month = uniqueN(date),
    days_available = sum(!is.na(temperature_c)),

    days_ge_20 = NA_integer_,

    days_ge_30 = count_or_na(temperature_c, 30)
  ),
  by = .(
    weather_region_id,
    weather_region_name,
    year,
    month,
    year_month,
    temperature_variable
  )
]


# Add the Tmax column to Tmin rows so both tables
# have the same structure before combining them.

monthly_tn[, days_ge_30 := NA_integer_]


monthly <- rbindlist(
  list(
    monthly_tn,
    monthly_tx
  ),
  use.names = TRUE,
  fill = TRUE
)


# Ensure consistent column order

setcolorder(
  monthly,
  c(
    "weather_region_id",
    "weather_region_name",
    "year",
    "month",
    "year_month",
    "temperature_variable",
    "days_in_month",
    "days_available",
    "days_ge_20",
    "days_ge_30"
  )
)


# Order Tmin before Tmax

variable_order <- c(
  "tn_mean",
  "tx_mean"
)

monthly[, temperature_variable := factor(
  temperature_variable,
  levels = variable_order
)]

setorder(
  monthly,
  weather_region_id,
  year,
  month,
  temperature_variable
)

monthly[, temperature_variable :=
          as.character(temperature_variable)]


# -------------------------------------------------------------------------
# STEP 4: Validate and save
# -------------------------------------------------------------------------

progress(
  "STEP 4/4",
  "Checking completeness and writing the CSV file."
)


expected_months <- 16L * 12L
expected_regions <- 20L
expected_variables <- 2L

expected_rows <-
  expected_months *
  expected_regions *
  expected_variables


if (uniqueN(monthly$year_month) != expected_months) {
  stop(
    "The output does not contain the expected 192 months."
  )
}


if (uniqueN(monthly$weather_region_id) != expected_regions) {
  stop(
    "The output does not contain the expected 20 SHARE regions."
  )
}


if (uniqueN(monthly$temperature_variable) != expected_variables) {
  stop(
    "The output does not contain the expected 2 temperature variables."
  )
}


if (nrow(monthly) != expected_rows) {
  stop(
    "Unexpected row count: ",
    nrow(monthly),
    " instead of ",
    expected_rows
  )
}


if (
  anyDuplicated(
    monthly[
      ,
      .(
        weather_region_id,
        year_month,
        temperature_variable
      )
    ]
  )
) {
  stop(
    "The output contains duplicate region-month-variable combinations."
  )
}


# Check that threshold columns that should not exist conceptually
# for a temperature variable are indeed missing.

if (
  any(
    !is.na(
      monthly[
        temperature_variable == "tx_mean",
        days_ge_20
      ]
    )
  )
) {
  stop(
    "Unexpected Tmin threshold values found for tx_mean."
  )
}


if (
  any(
    !is.na(
      monthly[
        temperature_variable == "tn_mean",
        days_ge_30
      ]
    )
  )
) {
  stop(
    "Unexpected Tmax threshold values found for tn_mean."
  )
}


fwrite(
  monthly,
  output_file,
  na = ""
)


progress(
  "DONE",
  paste0(
    "File saved: ",
    output_file
  )
)


cat(
  "SHARE regions:",
  uniqueN(monthly$weather_region_id),
  "\n"
)

cat(
  "Months:",
  uniqueN(monthly$year_month),
  "\n"
)

cat(
  "Temperature variables:",
  paste(
    unique(monthly$temperature_variable),
    collapse = ", "
  ),
  "\n"
)

cat(
  "Rows:",
  format(
    nrow(monthly),
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE
  ),
  "\n"
)

cat(
  "Tmin threshold: >=20 C\n"
)

cat(
  "Tmax threshold: >=30 C\n"
)