#!/usr/bin/env Rscript

# Reproduce the five final tables and five final figures ----------------------
#
# This master script starts exclusively from provider files in 01_data/01_raw.
# It rebuilds processed temperature and SHARE files, constructs both analysis
# datasets, and only then estimates models and creates the final outputs.

# Set to NULL to run the complete pipeline.
# Alternatively, enter the path of a script to start from that script.
START_FROM <- NULL

jobs <- list(
  c("Rscript", "02_code/00_config/check_dependencies.R"),
  c("Rscript", "02_code/09_validation/00_validate_raw_inputs.R"),
  c("Rscript", "02_code/02_exposure_construction/01_prepare_eobs_bbox.R"),
  c("Rscript", "02_code/02_exposure_construction/02_mask_eobs_italy.R"),
  c("Rscript", "02_code/02_exposure_construction/03_aggregate_eobs_italy_regions.R"),
  c("Rscript", "02_code/02_exposure_construction/04_count_italy_threshold_days.R"),
  c("Rscript", "02_code/02_exposure_construction/05_aggregate_italy_first_year.R"),
  c("Rscript", "02_code/02_exposure_construction/06_mask_eobs_croatia.R"),
  c("Rscript", "02_code/02_exposure_construction/07_aggregate_eobs_croatia_regions.R"),
  c("Rscript", "02_code/02_exposure_construction/08_count_croatia_threshold_days.R"),
  c("Rscript", "02_code/02_exposure_construction/09_aggregate_croatia_first_year.R"),
  c("Rscript", "02_code/01_data_preparation/01_prepare_italy_birth_regions.R"),
  c("Rscript", "02_code/01_data_preparation/02_prepare_italy_easyshare.R"),
  c("Rscript", "02_code/01_data_preparation/03_merge_italy_share_regions.R"),
  c("Rscript", "02_code/01_data_preparation/04_filter_italy_birth_cohort.R"),
  c("Rscript", "02_code/01_data_preparation/05_prepare_croatia_birth_regions.R"),
  c("Rscript", "02_code/01_data_preparation/06_prepare_croatia_easyshare.R"),
  c("Rscript", "02_code/01_data_preparation/07_merge_croatia_share_regions.R"),
  c("Rscript", "02_code/01_data_preparation/08_filter_croatia_birth_cohort.R"),
  c("Rscript", "02_code/01_data_preparation/09_merge_italy_analysis_data.R"),
  c("Rscript", "02_code/01_data_preparation/10_merge_croatia_analysis_data.R"),
  c("Rscript", "02_code/03_main_regressions/01_estimate_italy_tmin20.R"),
  c("Rscript", "02_code/07_tables/01_format_italy_tmin20_table.R"),
  c("Rscript", "02_code/03_main_regressions/02_estimate_italy_tmax30.R"),
  c("Rscript", "02_code/03_main_regressions/03_estimate_croatia_tmin20.R"),
  c("Rscript", "02_code/07_tables/02_format_croatia_tmin20_table.R"),
  c("Rscript", "02_code/06_summary_statistics/01_italy_summary_statistics.R"),
  c("Rscript", "02_code/04_bootstrap/01_italy_tmin20_wild_cluster.R"),
  c("Rscript", "02_code/05_sensitivity_analysis/01_estimate_italy_threshold_sensitivity.R"),
  c("Rscript", "02_code/05_sensitivity_analysis/02_plot_italy_threshold_sensitivity.R"),
  c("Rscript", "02_code/08_figures/01_prepare_italy_map_data.R"),
  c("Rscript", "02_code/08_figures/02_create_italy_maps.R"),
  c("Rscript", "02_code/08_figures/03_prepare_croatia_map_data.R"),
  c("Rscript", "02_code/08_figures/04_create_croatia_maps.R"),
  c("python3", "02_code/08_figures/05_crop_map_pairs.py"),
  c("Rscript", "02_code/09_validation/01_validate_outputs.R")
)

if (!is.null(START_FROM)) {
  scripts <- vapply(jobs, `[[`, character(1), 2)
  start_index <- match(START_FROM, scripts)

  if (is.na(start_index)) {
    stop("START_FROM script not found in jobs: ", START_FROM)
  }

  jobs <- jobs[start_index:length(jobs)]

  message("\nStarting pipeline from: ", START_FROM)
} else {
  message("\nStarting complete pipeline.")
}

for (job in jobs) {
  command <- job[[1]]
  script <- job[[2]]
  message("\n--- Running ", script, " ---")
  status <- system2(command, script)
  if (!identical(status, 0L)) stop("Replication failed in: ", script)
}

message("\nReplication completed successfully.")