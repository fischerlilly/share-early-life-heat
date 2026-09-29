#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))


# Merge Croatian SHARE person-wave data with first-12-month temperature exposure.

suppressPackageStartupMessages(library(data.table))

share_file <- file.path(DIR$processed_croatia,
  "easyshare_croatia_with_region_birthcohort_1950_1964.csv")
temperature_file <- file.path(DIR$processed_croatia,
  "eobs_first12months_threshold_days_birthmonths_1950_1964_croatia_share_regions.csv")
output_dir <- DIR$analysis_croatia
output_file <- file.path(output_dir,
  "share_croatia_birthcohort_1950_1964_with_early_life_temperature.csv")
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

share <- fread(share_file,na.strings=c("","NA"))
temperature <- fread(temperature_file,na.strings=c("","NA"))

if(any(!temperature$coverage_complete)) stop("Incomplete temperature exposure windows.")

required_temperature <- c(
  "weather_region_id",
  "birth_year_month",
  "temperature_variable",
  "days_ge_20",
  "coverage_complete"
)

if(!all(required_temperature %in% names(temperature))) {
  stop("Required variables are missing from the temperature exposure file.")
}

temperature <- temperature[
  temperature_variable == "tn_mean"
]

wide <- temperature[, .(
  weather_region_id,
  birth_year_month,
  tn_days_ge_20_first12m = days_ge_20
)]

if(anyDuplicated(wide[,.(weather_region_id,birth_year_month)])) stop("Duplicate exposure keys.")

before <- nrow(share)

result <- wide[
  share,
  on=.(weather_region_id,birth_year_month),
  nomatch=0L
]

if(nrow(result)!=before) stop("Some SHARE rows did not match temperature exposure.")

setorder(result,mergeid,wave)

fwrite(result,output_file,na="")

cat("Saved:",output_file,"\n")
cat("Person-wave observations:",nrow(result),"\n")
cat("Persons:",uniqueN(result$mergeid),"\n")
cat("Wave 7 persons:",uniqueN(result[wave==7]$mergeid),"\n")