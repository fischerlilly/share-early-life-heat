#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))


# Inner join easySHARE person-wave records with confirmed Croatian early-life regions.

suppressPackageStartupMessages(library(data.table))

input_dir <- DIR$processed_croatia
easy_file <- file.path(input_dir, "easyshare_croatia_selected_variables.csv")
region_file <- file.path(input_dir, "croatia_early_life_regions_wave7.csv")
output_file <- file.path(input_dir, "easyshare_croatia_with_early_life_region.csv")

easy <- fread(easy_file, na.strings = c("", "NA"))
regions <- fread(region_file, na.strings = c("", "NA"))
if (anyDuplicated(regions$mergeid)) stop("Region file is not unique by mergeid.")
if (anyDuplicated(easy[, .(mergeid, wave)])) stop("easySHARE is not unique by person-wave.")

combined <- regions[easy, on = "mergeid", nomatch = 0L]
setorder(combined, mergeid, wave)
if (anyDuplicated(combined[, .(mergeid, wave)])) stop("Join created duplicates.")

fwrite(combined, output_file, na = "")
cat("Saved:", output_file, "\n")
cat("Persons:", uniqueN(combined$mergeid), "\n")
cat("Person-wave observations:", nrow(combined), "\n")
print(combined[, .N, by = wave][order(wave)])

