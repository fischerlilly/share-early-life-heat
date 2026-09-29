#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))


# Restrict the Croatian person-wave file to exact birth months 1950-01--1964-12.

suppressPackageStartupMessages(library(data.table))

input_file <- file.path(
  DIR$processed_croatia,
  "easyshare_croatia_with_early_life_region.csv"
)
output_file <- file.path(
  DIR$processed_croatia,
  "easyshare_croatia_with_region_birthcohort_1950_1964.csv"
)

d <- fread(input_file, na.strings = c("", "NA"))
d[, birth_year := as.integer(dn003_mod)]
d[, birth_month := as.integer(dn002_mod)]
d[, birth_year_month := fifelse(
  birth_year %between% c(1950L, 1964L) & birth_month %between% c(1L, 12L),
  sprintf("%04d-%02d", birth_year, birth_month),
  NA_character_
)]

births <- unique(d[!is.na(birth_year_month), .(mergeid, birth_year_month)])
if (anyDuplicated(births$mergeid)) stop("Inconsistent birth month within person.")
eligible <- births[
  birth_year_month >= "1950-01" & birth_year_month <= "1964-12",
  mergeid
]
result <- d[mergeid %in% eligible]
setorder(result, mergeid, wave)

fwrite(result, output_file, na = "")
cat("Saved:", output_file, "\n")
cat("Persons:", uniqueN(result$mergeid), "\n")
cat("Person-wave observations:", nrow(result), "\n")
print(result[, .N, by = wave][order(wave)])
