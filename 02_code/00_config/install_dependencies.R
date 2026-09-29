#!/usr/bin/env Rscript

# Install only missing packages required by the final-output pipeline.
# Package installation changes the local R library and is therefore never
# called automatically by `run_all.R`.

required_packages <- c(
  "data.table", "haven", "ggplot2", "sf", "scales", "dplyr",
  "ncdf4", "terra"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All required R packages are already installed.")
}
