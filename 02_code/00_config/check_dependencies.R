#!/usr/bin/env Rscript

# Check dependencies for both raw-data construction and final analysis.
required_packages <- c(
  "data.table", "haven", "ggplot2", "sf", "scales", "dplyr",
  "ncdf4", "terra"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) stop(
  "Missing R packages: ", paste(missing_packages, collapse = ", ")
)
message("All required R packages are installed.")
