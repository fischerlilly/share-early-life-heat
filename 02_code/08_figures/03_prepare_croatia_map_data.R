#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

source(file.path("02_code", "00_config", "config.R"))
source(file.path("02_code", "00_config", "share_regions.R"))

analysis_file <- INPUT$croatia_analysis
output_dir <- DIR$processed_croatia
assert_inputs_exist(analysis_file, "Croatia map input")

regions <- copy(croatia_share_regions)

d <- fread(analysis_file, na.strings = c("", "NA"))[wave == 7]
if (anyDuplicated(d$mergeid)) stop("Wave-7 Croatian sample contains duplicate persons.")
counts <- d[, .(sample_n = .N), by = early_life_region_code]
regions <- regions[counts, on = "early_life_region_code"]
if (nrow(regions) != 20L || anyNA(regions$sample_n)) {
  stop("Not all 20 Croatian SHARE regions have a sample count.")
}
setorder(regions, early_life_region_code)
fwrite(regions, file.path(output_dir, "croatia_share_regions.csv"), na = "")

cat("Prepared", nrow(regions), "SHARE regions.\n")
cat("Wave-7 persons represented:", sum(regions$sample_n), "\n")
