#!/usr/bin/env Rscript

# Combine the E-OBS temperature files, retain 1950--1965, crop the common grid to the study bounding box, and export daily values in memory-safe blocks.
# Inputs and outputs are defined through the shared project configuration.
source(file.path("02_code", "00_config", "config.R"))



suppressPackageStartupMessages(library(terra))

progress <- function(step, text) {
  cat(
    sprintf("[%s] %s %s\n", format(Sys.time(), "%H:%M:%S"), step, text)
  )
  flush.console()
}

progress("START", "Starting temperature preparation.")


input_dir <- DIR$raw_eobs
output_dir <- DIR$processed_italy

output_file <- file.path(
  output_dir,
  "eobs_daily_tx_tn_1950_1965_bbox_35_48N_5_20E.csv.gz"
)
report_file <- file.path(
  output_dir,
  "eobs_daily_tx_tn_1950_1965_grid_report.csv"
)

start_date <- as.Date("1950-01-01")
end_date <- as.Date("1965-12-31")

target_bbox <- ext(5, 20, 35, 48)

days_per_block <- 31L

source_files <- list(
  tx = c(
    file.path(input_dir, "tx_ens_mean_0.1deg_reg_1950-1964_v33.0e.nc"),
    file.path(input_dir, "tx_ens_mean_0.1deg_reg_1965-1979_v33.0e.nc")
  ),
  tn = c(
    file.path(input_dir, "tn_ens_mean_0.1deg_reg_1950-1964_v33.0e.nc"),
    file.path(input_dir, "tn_ens_mean_0.1deg_reg_1965-1979_v33.0e.nc")
  )
)

all_source_files <- unlist(source_files, use.names = FALSE)
missing_files <- all_source_files[!file.exists(all_source_files)]
if (length(missing_files) > 0L) {
  stop("The following input files are missing: ", paste(missing_files, collapse = ", "))
}
progress("STEP 1/6", "All four NetCDF input files were found.")


open_and_select_period <- function(files, variable_name) {
  progress(
    "STEP 2/6",
    paste0("Opening both time blocks for ", variable_name, ".")
  )
  first_chunk <- rast(files[1])
  second_chunk <- rast(files[2])

  first_dates <- as.Date(time(first_chunk))
  second_dates <- as.Date(time(second_chunk))

  if (anyNA(first_dates) || anyNA(second_dates)) {
    stop("Invalid time coordinates for variable ", variable_name, ".")
  }

  keep_first <- which(first_dates >= start_date & first_dates <= end_date)
  keep_second <- which(second_dates >= start_date & second_dates <= end_date)

  selected <- c(first_chunk[[keep_first]], second_chunk[[keep_second]])
  selected_dates <- c(first_dates[keep_first], second_dates[keep_second])
  time(selected) <- selected_dates

  date_order <- order(selected_dates)
  selected <- selected[[date_order]]
  time(selected) <- selected_dates[date_order]
  names(selected) <- paste0(variable_name, "_", format(time(selected), "%Y%m%d"))

  progress(
    "STEP 2/6",
    paste0(
      variable_name, ": ", nlyr(selected),
      " days from ", min(selected_dates), " through ", max(selected_dates),
      " selected."
    )
  )

  selected
}

rasters <- list(
  tx = open_and_select_period(source_files$tx, "tx"),
  tn = open_and_select_period(source_files$tn, "tn")
)

expected_dates <- seq(start_date, end_date, by = "day")

for (variable_name in names(rasters)) {
  actual_dates <- as.Date(time(rasters[[variable_name]]))

  if (!isTRUE(all.equal(actual_dates, expected_dates))) {
    missing_dates <- setdiff(expected_dates, actual_dates)
    duplicate_dates <- actual_dates[duplicated(actual_dates)]
    stop(
      "Time axis for ", variable_name, " is incomplete. ",
      "Missing days: ", length(missing_dates), "; duplicate days: ",
      length(duplicate_dates), "."
    )
  }
}

progress(
  "STEP 2/6",
  "The daily time axes for tx and tn are complete and identical."
)


reference_full <- rasters$tn[[1]]

grid_is_equal <- vapply(
  rasters,
  function(x) {
    isTRUE(compareGeom(
      reference_full,
      x[[1]],
      crs = TRUE,
      ext = TRUE,
      rowcol = TRUE,
      res = TRUE,
      stopOnError = FALSE,
      messages = FALSE
    ))
  },
  logical(1)
)

progress(
  "STEP 3/6",
  paste0(
    "Compared original grids: ",
    paste(names(grid_is_equal), grid_is_equal, sep = "=", collapse = ", "),
    "."
  )
)

grid_report <- data.frame(
  variable = names(rasters),
  source_grid_matches_tn = unname(grid_is_equal),
  source_nrow = vapply(rasters, nrow, numeric(1)),
  source_ncol = vapply(rasters, ncol, numeric(1)),
  source_resolution_x = vapply(rasters, function(x) res(x)[1], numeric(1)),
  source_resolution_y = vapply(rasters, function(x) res(x)[2], numeric(1)),
  source_xmin = vapply(rasters, function(x) xmin(x), numeric(1)),
  source_xmax = vapply(rasters, function(x) xmax(x), numeric(1)),
  source_ymin = vapply(rasters, function(x) ymin(x), numeric(1)),
  source_ymax = vapply(rasters, function(x) ymax(x), numeric(1)),
  start_date = as.character(start_date),
  end_date = as.character(end_date),
  n_days = length(expected_dates),
  stringsAsFactors = FALSE
)


progress(
  "STEP 4/6",
  "Cropping all variables to 35-48 degrees north and 5-20 degrees east."
)
rasters <- lapply(rasters, crop, y = target_bbox, snap = "near")

reference_grid <- rasters$tn[[1]]
was_resampled <- setNames(rep(FALSE, length(rasters)), names(rasters))

for (variable_name in names(rasters)) {
  same_after_crop <- isTRUE(compareGeom(
    reference_grid,
    rasters[[variable_name]][[1]],
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE,
    stopOnError = FALSE,
    messages = FALSE
  ))

  if (!same_after_crop) {
    message(
      "Grid for ", variable_name,
      " differs from the tn reference grid and will be aligned bilinearly."
    )
    rasters[[variable_name]] <- resample(
      rasters[[variable_name]],
      reference_grid,
      method = "bilinear"
    )
    was_resampled[variable_name] <- TRUE
  }
}

progress(
  "STEP 4/6",
  paste0(
    "Grid alignment completed: ",
    paste(names(was_resampled), was_resampled, sep = " resampelt=", collapse = ", "),
    "."
  )
)


for (variable_name in names(rasters)) {
  if (!isTRUE(compareGeom(
    reference_grid,
    rasters[[variable_name]][[1]],
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE,
    stopOnError = FALSE,
    messages = FALSE
  ))) {
    stop("Grid alignment failed for variable ", variable_name, ".")
  }
}

grid_report$was_resampled <- unname(was_resampled[grid_report$variable])
grid_report$cropped_nrow <- nrow(reference_grid)
grid_report$cropped_ncol <- ncol(reference_grid)
grid_report$cropped_resolution_x <- res(reference_grid)[1]
grid_report$cropped_resolution_y <- res(reference_grid)[2]
grid_report$cropped_xmin <- xmin(reference_grid)
grid_report$cropped_xmax <- xmax(reference_grid)
grid_report$cropped_ymin <- ymin(reference_grid)
grid_report$cropped_ymax <- ymax(reference_grid)


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  grid_report,
  report_file,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
progress("STEP 5/6", paste("Grid validation report saved:", report_file))

if (identical(Sys.getenv("TEMP_PREP_VALIDATE_ONLY"), "1")) {
  cat("Validation succeeded; the large CSV export was not started.\n")
  cat("Grid report created:", report_file, "\n")
  cat("Period:", as.character(start_date), "through", as.character(end_date), "\n")
  cat("Days:", length(expected_dates), "\n")
  cat("Cropped grid:", nrow(reference_grid), "rows x",
      ncol(reference_grid), "columns\n")
  cat("Raster resampelt:",
      paste(names(was_resampled), was_resampled, sep = "=", collapse = ", "),
      "\n")
  quit(save = "no", status = 0)
}

coordinates <- xyFromCell(reference_grid, seq_len(ncell(reference_grid)))
names_coordinates <- c("longitude", "latitude")
colnames(coordinates) <- names_coordinates

output_connection <- gzfile(output_file, open = "wb", compression = 6)

progress(
  "STEP 6/6",
  paste0(
    "Starting blockwise CSV.GZ export in 31-day blocks. Expected: ",
    format(
      ncell(reference_grid) * length(expected_dates),
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE
    ),
    " rows."
  )
)

header_written <- FALSE
total_rows_written <- 0

block_starts <- seq(1L, length(expected_dates), by = days_per_block)

for (block_start in block_starts) {
  block_end <- min(block_start + days_per_block - 1L, length(expected_dates))
  layer_index <- block_start:block_end
  block_dates <- expected_dates[layer_index]

  tx_values <- values(rasters$tx[[layer_index]], mat = TRUE)
  tn_values <- values(rasters$tn[[layer_index]], mat = TRUE)

  n_block_days <- length(layer_index)
  n_grid_cells <- nrow(coordinates)

  block <- data.frame(
    date = rep(block_dates, each = n_grid_cells),
    longitude = rep(coordinates[, "longitude"], times = n_block_days),
    latitude = rep(coordinates[, "latitude"], times = n_block_days),
    tx = as.vector(tx_values),
    tn = as.vector(tn_values),
    stringsAsFactors = FALSE
  )


  write.table(
    block,
    file = output_connection,
    sep = ",",
    row.names = FALSE,
    col.names = !header_written,
    quote = FALSE,
    na = ""
  )

  header_written <- TRUE
  total_rows_written <- total_rows_written + nrow(block)

  progress(
    "STEP 6/6",
    paste0(
      "Written through ", max(block_dates), ": ",
      format(
        total_rows_written,
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
      ),
      " rows (block ",
      match(block_start, block_starts), " of ", length(block_starts), ")."
    )
  )
}

close(output_connection)


cat("\nTemperature file created:", output_file, "\n")
cat("Grid report created:", report_file, "\n")
cat("Period:", as.character(start_date), "through", as.character(end_date), "\n")
cat("Days:", length(expected_dates), "\n")
cat("Cropped grid:", nrow(reference_grid), "rows x",
    ncol(reference_grid), "columns\n")
cat("Grid resolution:", paste(res(reference_grid), collapse = " x "), "degrees\n")
cat("Actual grid extent after snap='near':",
    paste(as.vector(ext(reference_grid)), collapse = ", "), "\n")
cat("Raster resampelt:",
    paste(names(was_resampled), was_resampled, sep = "=", collapse = ", "),
    "\n")
cat("Rows written including no-data cells:",
    total_rows_written, "\n")
progress("DONE", "Temperature preparation completed successfully.")