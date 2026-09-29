#!/usr/bin/env Rscript

# Estimate fully adjusted sensitivity models for Tmin thresholds 18--22 C.

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

source(file.path("02_code", "00_config", "config.R"))
daily_temperature_file <- INPUT$italy_daily_regional_temperature
analysis_file <- INPUT$italy_analysis
isced_file <- INPUT$parental_education
exposure_file <- file.path(
  DIR$processed_italy, "italy_tmin_18_22_first12m_exposure.csv"
)
coefficient_file <- file.path(
  DIR$estimates, "italy_tmin_18_22_health_coefficients.csv"
)
assert_inputs_exist(
  c(daily_temperature_file, analysis_file, isced_file),
  "Italy sensitivity-analysis input"
)

thresholds <- 18:22

# -----------------------------------------------------------------------------
# 1. Construct exposure for each region and birth month
# -----------------------------------------------------------------------------

daily_connection <- gzfile(daily_temperature_file, open = "rt")
daily <- as.data.table(read.csv(
  daily_connection,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
))[, .(date, weather_region_id, weather_region_name, tn_mean)]
close(daily_connection)
daily[, date := as.IDate(date)]
if (anyNA(daily$date)) stop("At least one daily date could not be parsed.")
if (anyDuplicated(daily[, .(weather_region_id, date)])) {
  stop("Duplicate SHARE-region-date rows.")
}
if (uniqueN(daily$weather_region_id) != 20L) {
  stop("Expected 20 Italian SHARE regions in the daily weather file.")
}
daily[, month_date := as.IDate(format(date, "%Y-%m-01"))]

count_days <- function(x, threshold) {
  if (all(is.na(x))) return(NA_integer_)
  as.integer(sum(x >= threshold, na.rm = TRUE))
}
monthly <- daily[, c(
  list(days_in_month = uniqueN(date), days_available = sum(!is.na(tn_mean))),
  setNames(lapply(thresholds, function(z) count_days(tn_mean, z)),
    paste0("days_ge_", thresholds))
), by = .(weather_region_id, weather_region_name, month_date)]

birth_months <- seq(as.IDate("1950-01-01"), as.IDate("1964-12-01"), by = "month")
windows <- rbindlist(lapply(birth_months, function(birth_month) {
  data.table(
    birth_month_date = birth_month,
    month_date = seq(birth_month, by = "month", length.out = 12L)
  )
}))
expanded <- monthly[windows, on = "month_date", allow.cartesian = TRUE, nomatch = 0L]
strict_sum <- function(x) if (anyNA(x)) NA_integer_ else as.integer(sum(x))
exposure <- expanded[, c(
  list(
    months = .N,
    expected_days = sum(days_in_month),
    days_available = sum(days_available)
  ),
  setNames(lapply(paste0("days_ge_", thresholds), function(v) strict_sum(get(v))),
    paste0("tmin_days_ge_", thresholds))
), by = .(weather_region_id, weather_region_name, birth_month_date)]
exposure[, birth_year_month := format(birth_month_date, "%Y-%m")]
if (any(exposure$months != 12L)) stop("Incomplete 12-month window detected.")
if (any(exposure$days_available != exposure$expected_days)) {
  stop("Missing daily Tmin observations in at least one exposure window.")
}
if (anyDuplicated(exposure[, .(weather_region_id, birth_year_month)])) {
  stop("Duplicate exposure keys.")
}
fwrite(exposure, exposure_file, na = "")

# -----------------------------------------------------------------------------
# 2. Assemble Wave-7 estimation sample and controls
# -----------------------------------------------------------------------------

d <- fread(analysis_file, na.strings = c("", "NA"))[wave == 7]
d <- exposure[
  d,
  on = .(weather_region_id, birth_year_month),
  nomatch = 0L
]
if (nrow(d) != 1925L) stop("Unexpected Wave-7 analysis sample after exposure merge.")

# Verify that the newly constructed 20 C measure reproduces the existing one.
if (!isTRUE(all.equal(
  as.numeric(d$tmin_days_ge_20), as.numeric(d$tn_days_ge_20_first12m),
  tolerance = 0
))) stop("New Tmin >= 20 C exposure does not match the established measure.")

parents <- as.data.table(read_dta(
  isced_file,
  col_select = c("mergeid", "isced1997_m", "isced1997_f")
))
parents[, mergeid := as.character(mergeid)]
d[, mergeid := as.character(mergeid)]
d <- parents[d, on = "mergeid"]
if (anyDuplicated(d$mergeid)) stop("Duplicate mergeid after ISCED merge.")

clean_nonnegative <- function(x) {
  fifelse(!is.na(x) & as.numeric(x) >= 0, as.numeric(x), NA_real_)
}
d[, `:=`(
  female_clean = fifelse(female %in% c(0, 1), as.numeric(female), NA_real_),
  mother_isced = clean_nonnegative(isced1997_m),
  father_isced = clean_nonnegative(isced1997_f),
  sphus_clean = clean_nonnegative(sphus),
  maxgrip_clean = clean_nonnegative(maxgrip),
  chronic_clean = clean_nonnegative(chronic_mod),
  region_fe = factor(birth_region_code),
  birth_year_fe = factor(dn003_mod),
  birth_month_fe = factor(dn002_mod)
)]

outcomes <- data.table(
  outcome = c("sphus_clean", "maxgrip_clean", "chronic_clean"),
  outcome_key = c("self_rated_health", "grip_strength", "chronic_conditions"),
  outcome_label = c("Self-rated health", "Maximum grip strength", "Chronic conditions")
)

# -----------------------------------------------------------------------------
# 3. Fully adjusted models with CR1 clustered standard errors
# -----------------------------------------------------------------------------

cluster_vcov_cr1 <- function(model, cluster) {
  keep <- !is.na(coef(model))
  X <- model.matrix(model)[, keep, drop = FALSE]
  u <- residuals(model)
  cluster <- droplevels(factor(cluster))
  n <- nrow(X); k <- ncol(X); g <- nlevels(cluster)
  bread <- solve(crossprod(X))
  meat <- matrix(0, k, k)
  for (level in levels(cluster)) {
    index <- cluster == level
    score <- crossprod(X[index, , drop = FALSE], u[index])
    meat <- meat + tcrossprod(score)
  }
  (g / (g - 1)) * ((n - 1) / (n - k)) * bread %*% meat %*% bread
}

fit_model <- function(outcome, threshold) {
  exposure_variable <- paste0("tmin_days_ge_", threshold)
  needed <- c(
    outcome, exposure_variable, "female_clean", "mother_isced", "father_isced",
    "birth_region_code", "dn003_mod", "dn002_mod"
  )
  model_data <- d[complete.cases(d[, ..needed])]
  model_data[, exposure_10days := get(exposure_variable) / 10]
  model <- lm(
    as.formula(paste(
      outcome,
      "~ exposure_10days + female_clean + factor(mother_isced) +",
      "factor(father_isced) + region_fe + birth_year_fe + birth_month_fe"
    )),
    data = model_data
  )
  covariance <- cluster_vcov_cr1(model, model_data$birth_region_code)
  estimate <- unname(coef(model)["exposure_10days"])
  standard_error <- sqrt(covariance["exposure_10days", "exposure_10days"])
  clusters <- uniqueN(model_data$birth_region_code)
  critical_value <- qt(0.975, df = clusters - 1L)
  p_value <- 2 * pt(abs(estimate / standard_error), df = clusters - 1L,
    lower.tail = FALSE)
  data.table(
    threshold = threshold,
    estimate = estimate,
    standard_error = standard_error,
    conf_low = estimate - critical_value * standard_error,
    conf_high = estimate + critical_value * standard_error,
    p_value = p_value,
    observations = nobs(model),
    clusters = clusters,
    outcome_mean = mean(model_data[[outcome]]),
    r_squared = summary(model)$r.squared
  )
}

results <- rbindlist(lapply(seq_len(nrow(outcomes)), function(i) {
  rbindlist(lapply(thresholds, function(threshold) {
    z <- fit_model(outcomes$outcome[i], threshold)
    z[, `:=`(
      outcome = outcomes$outcome[i],
      outcome_key = outcomes$outcome_key[i],
      outcome_label = outcomes$outcome_label[i]
    )]
    z
  }))
}))
setcolorder(results, c(
  "outcome", "outcome_key", "outcome_label", "threshold", "estimate",
  "standard_error", "conf_low", "conf_high", "p_value", "observations",
  "clusters", "outcome_mean", "r_squared"
))
fwrite(results, coefficient_file, na = "")

cat("Saved exposure data:", exposure_file, "\n")
cat("Saved estimates:", coefficient_file, "\n")
print(results)
