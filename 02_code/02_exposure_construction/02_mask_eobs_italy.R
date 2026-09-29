#!/usr/bin/env Rscript

# Retain every E-OBS grid cell whose square intersects Italy. Regional assignments are deliberately deferred to the next stage.
# Inputs and outputs are defined through the shared project configuration.
source(file.path("02_code", "00_config", "config.R"))



suppressPackageStartupMessages({
  library(sf)
  library(data.table)
})

progress <- function(step, text) {
  cat(sprintf("[%s] %s %s\n", format(Sys.time(), "%H:%M:%S"), step, text))
  flush.console()
}

progress("START", "Starting the Italy mask.")


input_file <- file.path(DIR$processed_italy,
  "eobs_daily_tx_tn_1950_1965_bbox_35_48N_5_20E.csv.gz"
)
boundary_file <- INPUT$nuts_2024
output_dir <- DIR$processed_italy
output_file <- file.path(
  output_dir, "eobs_daily_tx_tn_1950_1965_italy.csv.gz"
)
temporary_output_file <- paste0(output_file, ".part")
mask_file <- file.path(output_dir, "eobs_gridcells_overlapping_italy.csv")

grid_cells_per_day <- 19500L
number_of_days <- 5844L
expected_input_rows <- as.double(grid_cells_per_day) * number_of_days
lines_per_chunk <- 500000L

if (!file.exists(input_file)) stop("Input file not found: ", input_file)
if (!file.exists(boundary_file)) stop("Boundary file not found: ", boundary_file)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


progress("STEP 1/4", "Reading the 19,500 grid cells for the first day.")

preview_connection <- gzfile(input_file, open = "rt")
header_line <- readLines(preview_connection, n = 1L, warn = FALSE)
first_day_lines <- readLines(
  preview_connection, n = grid_cells_per_day, warn = FALSE
)
close(preview_connection)

if (length(header_line) != 1L) stop("No readable header was found.")
if (length(first_day_lines) != grid_cells_per_day) {
  stop("The first day does not contain 19,500 grid rows.")
}

first_day <- fread(
  text = paste(c(header_line, first_day_lines), collapse = "\n"),
  select = c("date", "longitude", "latitude"),
  na.strings = c("", "NA"),
  showProgress = FALSE
)

if (uniqueN(first_day$date) != 1L) {
  stop("The first 19,500 rows do not belong to exactly one day.")
}
if (anyDuplicated(first_day[, .(longitude, latitude)])) {
  stop("The first day contains duplicate grid coordinates.")
}

first_day[, longitude_key := round(longitude, 6)]
first_day[, latitude_key := round(latitude, 6)]
first_day[, cell_id := .I]


progress(
  "STEP 2/4",
  "Creating grid squares and testing their intersection with Italy."
)

half_cell <- 0.05

make_cell_polygon <- function(longitude, latitude) {
  st_polygon(list(matrix(
    c(
      longitude - half_cell, latitude - half_cell,
      longitude + half_cell, latitude - half_cell,
      longitude + half_cell, latitude + half_cell,
      longitude - half_cell, latitude + half_cell,
      longitude - half_cell, latitude - half_cell
    ),
    ncol = 2, byrow = TRUE
  )))
}

grid_sf <- st_sf(
  cell_id = first_day$cell_id,
  longitude = first_day$longitude,
  latitude = first_day$latitude,
  longitude_key = first_day$longitude_key,
  latitude_key = first_day$latitude_key,
  geometry = st_sfc(
    Map(make_cell_polygon, first_day$longitude, first_day$latitude),
    crs = 4326
  )
)

italy_boundary <- st_read(boundary_file, quiet = TRUE)
italy_boundary <- italy_boundary[
  italy_boundary$CNTR_CODE == "IT" & italy_boundary$LEVL_CODE == 0,
]

if (nrow(italy_boundary) != 1L) {
  stop("The boundary file does not contain exactly one Italian NUTS-0 geometry.")
}

grid_3035 <- st_transform(grid_sf, 3035)
italy_boundary <- st_transform(italy_boundary, 3035)

intersection_list <- st_intersects(grid_3035, italy_boundary, sparse = TRUE)
keep_cell <- lengths(intersection_list) > 0L

italy_cells <- first_day[
  keep_cell,
  .(cell_id, longitude, latitude, longitude_key, latitude_key)
]

if (nrow(italy_cells) == 0L) stop("No grid cell intersects Italy.")
if (anyDuplicated(italy_cells[, .(longitude_key, latitude_key)])) {
  stop("The Italy mask contains duplicate grid coordinates.")
}

fwrite(italy_cells, mask_file, na = "")
setkey(italy_cells, longitude_key, latitude_key)

progress(
  "STEP 2/4",
  paste0(
    nrow(italy_cells), " of ", nrow(first_day),
    " grid cells intersect Italy. No NUTS assignment was made."
  )
)

if (identical(Sys.getenv("TEMP_MASK_VALIDATE_ONLY"), "1")) {
  progress("VALIDATION RUN", "Italy mask validated; large export not started.")
  cat("Grid cells in bounding box:", nrow(first_day), "\n")
  cat("Grid cells intersecting Italy:", nrow(italy_cells), "\n")
  cat("Maskendatei:", mask_file, "\n")
  quit(save = "no", status = 0)
}


progress(
  "STEP 3/4",
  paste0(
    "Filtere blockweise; pro Block werden ",
    format(lines_per_chunk, big.mark = ".", decimal.mark = ","),
    " input rows read."
  )
)

input_connection <- gzfile(input_file, open = "rt")
output_connection <- gzfile(temporary_output_file, open = "wb", compression = 6)

header_again <- readLines(input_connection, n = 1L, warn = FALSE)
if (!identical(header_again, header_line)) {
  close(input_connection)
  close(output_connection)
  stop("The header changed between the preview and main run.")
}

input_rows_read <- 0
output_rows_written <- 0
chunk_number <- 0L
output_header_written <- FALSE

repeat {
  lines <- readLines(input_connection, n = lines_per_chunk, warn = FALSE)
  if (length(lines) == 0L) break

  chunk_number <- chunk_number + 1L
  input_rows_read <- input_rows_read + length(lines)

  chunk <- fread(
    text = paste(c(header_line, lines), collapse = "\n"),
    na.strings = c("", "NA"),
    showProgress = FALSE
  )

  required_columns <- c("date", "longitude", "latitude", "tx", "tn")
  if (!all(required_columns %in% names(chunk))) {
    close(input_connection)
    close(output_connection)
    stop("At least one expected column is missing from the input block.")
  }

  chunk[, longitude_key := round(longitude, 6)]
  chunk[, latitude_key := round(latitude, 6)]

  chunk <- chunk[italy_cells, on = .(longitude_key, latitude_key), nomatch = 0L]
  chunk[, c("longitude_key", "latitude_key", "cell_id", "i.longitude", "i.latitude") := NULL]
  setcolorder(chunk, required_columns)

  write.table(
    chunk,
    file = output_connection,
    sep = ",",
    row.names = FALSE,
    col.names = !output_header_written,
    quote = FALSE,
    na = ""
  )

  output_header_written <- TRUE
  output_rows_written <- output_rows_written + nrow(chunk)

  progress(
    "STEP 3/4",
    paste0(
      "Block ", chunk_number, ": ",
      format(input_rows_read, big.mark = ".", decimal.mark = ","),
      " input rows read; ",
      format(output_rows_written, big.mark = ".", decimal.mark = ","),
      " Italy rows written."
    )
  )
}

close(input_connection)
close(output_connection)

if (input_rows_read != expected_input_rows) {
  stop(
    "Incomplete input file: ", input_rows_read,
    " instead of ", expected_input_rows, " data rows read."
  )
}

expected_output_rows <- as.double(nrow(italy_cells)) * number_of_days
if (output_rows_written != expected_output_rows) {
  stop(
    "Unexpected output row count: ", output_rows_written,
    " instead of ", expected_output_rows, "."
  )
}

if (file.exists(output_file)) unlink(output_file)
if (!file.rename(temporary_output_file, output_file)) {
  stop("The complete .part file could not be renamed.")
}

progress("STEP 4/4", "Completeness and row count validated successfully.")
progress("DONE", paste("Italy file created:", output_file))

cat("\nRasterzellen im Ausgangsrechteck:", nrow(first_day), "\n")
cat("Grid cells intersecting Italy:", nrow(italy_cells), "\n")
cat("Days:", number_of_days, "\n")
cat("Output rows:", output_rows_written, "\n")
cat("Included columns:", paste(required_columns, collapse = ", "), "\n")
