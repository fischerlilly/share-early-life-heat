#!/usr/bin/env Rscript

# Sum monthly threshold counts over the twelve months beginning in each
# possible birth month and require complete window coverage.
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


progress("START", "Starting first-twelve-month aggregation.")


# -------------------------------------------------------------------------
# File paths and birth-month range
# -------------------------------------------------------------------------

input_file <- file.path(
  DIR$processed_italy,
  "eobs_monthly_temperature_threshold_days_1950_1965_italy_share_regions.csv"
)

output_dir <- DIR$processed_italy

output_file <- file.path(
  output_dir,
  "eobs_first12months_temperature_threshold_days_birthmonths_1950_1964_share_regions.csv"
)


first_birth_month <- as.IDate("1950-01-01")
last_birth_month <- as.IDate("1964-12-01")


if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# -------------------------------------------------------------------------
# STEP 1: Read monthly threshold counts
# -------------------------------------------------------------------------

progress(
  "STEP 1/4",
  "Reading monthly threshold counts."
)


monthly <- fread(
  input_file,
  na.strings = c("", "NA"),
  showProgress = FALSE
)


threshold_columns <- c(
  "days_ge_20",
  "days_ge_30"
)


required_columns <- c(
  "weather_region_id",
  "weather_region_name",
  "year",
  "month",
  "year_month",
  "temperature_variable",
  "days_in_month",
  "days_available",
  threshold_columns
)


if (!all(required_columns %in% names(monthly))) {
  stop("Required columns are missing from the input file.")
}


# Only Tmin and Tmax should be present.

expected_temperature_variables <- c(
  "tn_mean",
  "tx_mean"
)


if (!setequal(
  unique(monthly$temperature_variable),
  expected_temperature_variables
)) {
  stop(
    "Unexpected temperature variables in the input file. ",
    "Expected only tn_mean and tx_mean."
  )
}


monthly[, month_date := as.IDate(
  paste0(year_month, "-01")
)]


if (anyNA(monthly$month_date)) {
  stop("At least one month could not be parsed.")
}


if (
  anyDuplicated(
    monthly[
      ,
      .(
        weather_region_id,
        month_date,
        temperature_variable
      )
    ]
  )
) {
  stop(
    "Duplicate region-month-variable combination in the input file."
  )
}


# -------------------------------------------------------------------------
# STEP 2: Create first-twelve-month windows
# -------------------------------------------------------------------------

progress(
  "STEP 2/4",
  "Creating 180 birth months and their twelve-month windows."
)


birth_months <- seq(
  first_birth_month,
  last_birth_month,
  by = "month"
)


windows <- rbindlist(
  lapply(
    birth_months,
    function(birth_month) {

      data.table(
        birth_month_date = birth_month,

        month_date = seq(
          birth_month,
          by = "month",
          length.out = 12L
        ),

        exposure_month_number = 1:12
      )
    }
  )
)


expanded <- monthly[
  windows,
  on = .(month_date),
  nomatch = 0L,
  allow.cartesian = TRUE
]


if (
  uniqueN(expanded$birth_month_date) !=
  length(birth_months)
) {
  stop(
    "Not all birth months could be matched to weather data."
  )
}


# -------------------------------------------------------------------------
# STEP 3: Sum threshold days over first twelve months
# -------------------------------------------------------------------------

progress(
  "STEP 3/4",
  "Summing threshold days within each window."
)


strict_sum <- function(x) {

  # If a threshold is not defined for this temperature variable
  # (e.g. >=30°C for Tmin), all values are NA and the result remains NA.

  if (anyNA(x)) {
    return(NA_integer_)
  }

  as.integer(sum(x))
}


first12 <- expanded[, .(

  exposure_start = min(month_date),

  exposure_end = as.IDate(
    seq(
      max(month_date),
      by = "month",
      length.out = 2L
    )[2L] - 1L
  ),

  months_expected = 12L,

  months_with_data =
    sum(days_available == days_in_month),

  expected_days =
    sum(days_in_month),

  days_available =
    sum(days_available),

  days_ge_20 = strict_sum(days_ge_20),
  days_ge_30 = strict_sum(days_ge_30)

), by = .(
  weather_region_id,
  weather_region_name,
  birth_month_date,
  temperature_variable
)]


first12[, `:=`(

  birth_year =
    year(birth_month_date),

  birth_month =
    month(birth_month_date),

  birth_year_month =
    format(birth_month_date, "%Y-%m"),

  coverage_complete =
    days_available == expected_days
)]


# Order Tmin before Tmax.

variable_order <- c(
  "tn_mean",
  "tx_mean"
)


first12[, temperature_variable := factor(
  temperature_variable,
  levels = variable_order
)]


setorder(
  first12,
  weather_region_id,
  birth_month_date,
  temperature_variable
)


first12[, temperature_variable :=
          as.character(temperature_variable)]


# -------------------------------------------------------------------------
# Select final output columns
# -------------------------------------------------------------------------

result <- first12[, .(

  weather_region_id,
  weather_region_name,

  birth_year,
  birth_month,
  birth_year_month,

  exposure_start,
  exposure_end,

  temperature_variable,

  months_expected,
  months_with_data,

  expected_days,
  days_available,
  coverage_complete,

  days_ge_20,
  days_ge_30
)]


# -------------------------------------------------------------------------
# STEP 4: Validate and save
# -------------------------------------------------------------------------

progress(
  "STEP 4/4",
  "Checking completeness and writing the CSV file."
)


# 20 SHARE regions
# x 180 possible birth months
# x 2 temperature variables (Tmin and Tmax)

expected_rows <-
  20L *
  length(birth_months) *
  2L


if (nrow(result) != expected_rows) {
  stop(
    "Unexpected row count: ",
    nrow(result),
    " instead of ",
    expected_rows
  )
}


if (
  anyDuplicated(
    result[
      ,
      .(
        weather_region_id,
        birth_year_month,
        temperature_variable
      )
    ]
  )
) {
  stop(
    "The output contains duplicate region-birth-month-variable combinations."
  )
}


if (
  min(result$birth_year_month) !=
  format(first_birth_month, "%Y-%m") ||
  max(result$birth_year_month) !=
  format(last_birth_month, "%Y-%m")
) {
  stop(
    "The output birth-month range is incorrect."
  )
}


# Require complete daily temperature coverage for all
# region × birth-month × temperature-variable windows.

if (any(!result$coverage_complete)) {
  stop(
    "At least one twelve-month exposure window has incomplete temperature coverage."
  )
}


# Check that only the intended thresholds contain values.

if (
  any(
    !is.na(
      result[
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
      result[
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
  result,
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
  "Birth months:",
  uniqueN(result$birth_year_month),
  "\n"
)


cat(
  "SHARE regions:",
  uniqueN(result$weather_region_id),
  "\n"
)


cat(
  "Temperature variables:",
  paste(
    unique(result$temperature_variable),
    collapse = ", "
  ),
  "\n"
)


cat(
  "Rows:",
  format(
    nrow(result),
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE
  ),
  "\n"
)


cat(
  "Incomplete twelve-month windows:",
  sum(!result$coverage_complete),
  "\n"
)


cat(
  "Tmin threshold: >=20 C\n"
)


cat(
  "Tmax threshold: >=30 C\n"
)