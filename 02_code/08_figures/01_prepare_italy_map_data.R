#!/usr/bin/env Rscript

# Prepare sample counts, SHARE-region crosswalk, and E-OBS grid assignments.

suppressPackageStartupMessages(library(data.table))

source(file.path("02_code", "00_config", "config.R"))
source(file.path("02_code", "00_config", "share_regions.R"))

analysis_file <- INPUT$italy_analysis
weights_file <- file.path(
  DIR$processed_italy, "eobs_gridcell_italy_share_region_area_weights.csv"
)
grid_file <- file.path(DIR$processed_italy, "eobs_gridcells_overlapping_italy.csv")
output_dir <- DIR$processed_italy
assert_inputs_exist(c(analysis_file, weights_file, grid_file), "Italy map input")

region_crosswalk <- copy(italy_share_regions)

analysis <- fread(analysis_file, na.strings = c("", "NA"))[wave == 7]
if (anyDuplicated(analysis$mergeid)) stop("Wave-7 analysis sample has duplicate persons.")
counts <- analysis[, .(sample_n = .N), by = birth_region_code]
regions <- region_crosswalk[counts, on = "birth_region_code"]
if (nrow(regions) != 20L || anyNA(regions$sample_n)) {
  stop("Not all 20 SHARE regions have a Wave-7 sample count.")
}
setorder(regions, birth_region_code)
fwrite(regions, file.path(output_dir, "italy_share_regions.csv"), na = "")

weights <- fread(weights_file)
weights <- region_crosswalk[, .(
  weather_region_id, share_code, birth_region_code, region_name
)][weights, on = "weather_region_id", nomatch = 0L]
if (uniqueN(weights$cell_id) != 3775L) stop("Unexpected number of weighted E-OBS cells.")

# Aggregate intersection area for the combined SHARE regions and assign every
# cell to the region with the largest overlapping Italian area.
cell_region_area <- weights[, .(overlap_area_m2 = sum(area_m2)),
  by = .(cell_id, share_code, birth_region_code, region_name)]
setorder(cell_region_area, cell_id, -overlap_area_m2, share_code)
dominant <- cell_region_area[, .SD[1L], by = cell_id]
dominant[, total_italy_overlap_m2 := cell_region_area[
  dominant, on = "cell_id", sum(overlap_area_m2), by = .EACHI
]$V1]
dominant[, dominant_share := overlap_area_m2 / total_italy_overlap_m2]

grid <- fread(grid_file)
grid_assignment <- dominant[grid, on = "cell_id"]
if (nrow(grid_assignment) != 3775L || anyNA(grid_assignment$share_code)) {
  stop("Every retained E-OBS cell must have a dominant SHARE-region assignment.")
}
fwrite(grid_assignment,
  file.path(output_dir, "italy_eobs_grid_dominant_share_region.csv"), na = "")

cat("Prepared 20 SHARE regions and", nrow(grid_assignment), "E-OBS cells.\n")
cat("Wave-7 persons represented:", sum(regions$sample_n), "\n")
cat("Boundary-crossing cells:", sum(grid_assignment$dominant_share < 0.999), "\n")
