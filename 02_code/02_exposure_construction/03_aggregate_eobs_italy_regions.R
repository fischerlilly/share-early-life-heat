#!/usr/bin/env Rscript

# Construct the 20 Italian SHARE-region polygons from NUTS-2 boundaries, then
# intersect E-OBS grid squares with those polygons and compute exact
# area-weighted daily regional temperature statistics.
# Inputs and outputs are defined through the shared project configuration.

source(file.path("02_code", "00_config", "config.R"))
source(file.path("02_code", "00_config", "share_regions.R"))

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
})

progress <- function(step, text) {
  cat(sprintf("[%s] %s %s\n", format(Sys.time(), "%H:%M:%S"), step, text))
  flush.console()
}

weighted_results <- function(sum_w, sum_wx, sum_wx2) {
  mean_value <- fifelse(sum_w > 0, sum_wx / sum_w, NA_real_)
  variance_value <- fifelse(
    sum_w > 0,
    pmax(sum_wx2 / sum_w - mean_value^2, 0),
    NA_real_
  )
  list(mean = mean_value, variance = variance_value)
}

progress("START", "Starting direct SHARE-region aggregation.")

input_file <- file.path(DIR$processed_italy,
  "eobs_daily_tx_tn_1950_1965_italy.csv.gz"
)
grid_file <- file.path(DIR$processed_italy,
  "eobs_gridcells_overlapping_italy.csv"
)
nuts_file <- INPUT$nuts_2024
output_dir <- DIR$processed_italy
weights_file <- file.path(output_dir, "eobs_gridcell_italy_share_region_area_weights.csv")
output_file <- file.path(
  output_dir,
  "eobs_daily_tx_tn_1950_1965_italy_share_regions_area_weighted.csv.gz"
)
temporary_output <- paste0(output_file, ".part")
temporary_summaries <- tempfile("share_region_partial_", tmpdir = output_dir, fileext = ".csv")

lines_per_chunk <- 500000L
half_cell <- 0.05

for (required_file in c(input_file, grid_file, nuts_file)) {
  if (!file.exists(required_file)) stop("Input file not found: ", required_file)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

progress("STEP 1/5", "Constructing Italian SHARE regions from NUTS-2 boundaries.")

grid <- fread(grid_file, showProgress = FALSE)
required_grid_columns <- c("cell_id", "longitude", "latitude")
if (!all(required_grid_columns %in% names(grid))) {
  stop("The grid file does not contain all required columns.")
}

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
  cell_id = grid$cell_id,
  longitude_key = round(grid$longitude, 6),
  latitude_key = round(grid$latitude, 6),
  geometry = st_sfc(
    Map(make_cell_polygon, grid$longitude, grid$latitude),
    crs = 4326
  )
)

nuts2 <- st_read(nuts_file, quiet = TRUE)
nuts2 <- nuts2[nuts2$CNTR_CODE == "IT" & nuts2$LEVL_CODE == 2, ]
if (nrow(nuts2) != 21L) {
  stop("Expected 21 Italian NUTS-2 regions; found: ", nrow(nuts2))
}

grid_sf <- st_transform(grid_sf, 3035)
nuts2 <- st_transform(nuts2[, c("NUTS_ID", "NAME_LATN")], 3035)
nuts2 <- st_make_valid(nuts2)

crosswalk <- copy(italy_share_regions)[, .(
  birth_region_code, weather_region_id,
  weather_region_name = region_name, nuts_ids
)]

nuts_to_share <- rbindlist(lapply(seq_len(nrow(crosswalk)), function(index) {
  data.table(
    NUTS_ID = strsplit(crosswalk$nuts_ids[index], "\\|")[[1]],
    weather_region_id = crosswalk$weather_region_id[index],
    weather_region_name = crosswalk$weather_region_name[index]
  )
}))

if (!setequal(nuts2$NUTS_ID, nuts_to_share$NUTS_ID)) {
  stop("The Italian NUTS-2 geometries do not match the SHARE crosswalk.")
}

nuts2 <- merge(nuts2, nuts_to_share, by = "NUTS_ID", all.x = TRUE, sort = FALSE)
source_area <- sum(as.numeric(st_area(nuts2)))

share_regions <- nuts2 |>
  dplyr::group_by(weather_region_id, weather_region_name) |>
  dplyr::summarise(do_union = TRUE, .groups = "drop") |>
  st_make_valid()

if (nrow(share_regions) != 20L) stop("Expected 20 Italian SHARE regions.")

share_area <- sum(as.numeric(st_area(share_regions)))
if (!isTRUE(all.equal(source_area, share_area, tolerance = 1e-8))) {
  stop("Dissolving NUTS-2 boundaries changed the total Italian area.")
}

progress("STEP 2/5", "Intersecting grid squares with SHARE regions and computing weights.")

suppressWarnings({
  overlaps <- st_intersection(grid_sf, share_regions)
})

overlaps$area_m2 <- as.numeric(st_area(overlaps))

weights <- as.data.table(st_drop_geometry(overlaps))[
  area_m2 > 0,
  .(cell_id, longitude_key, latitude_key, weather_region_id,
    weather_region_name, area_m2)
]

if (nrow(weights) == 0L) stop("No grid-SHARE-region intersections were found.")

if (anyDuplicated(weights[, .(cell_id, weather_region_id)])) {
  weights <- weights[, .(
    longitude_key = first(longitude_key),
    latitude_key = first(latitude_key),
    weather_region_name = first(weather_region_name),
    area_m2 = sum(area_m2)
  ), by = .(cell_id, weather_region_id)]
}

setkey(weights, longitude_key, latitude_key)
fwrite(weights, weights_file, na = "")

progress(
  "STEP 2/5",
  paste0(
    format(nrow(weights), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    " cell-region weights for ", uniqueN(weights$weather_region_id),
    " regions saved."
  )
)

progress(
  "STEP 3/5",
  paste0("Processing temperature data in blocks of ",
         format(lines_per_chunk, big.mark = ".", decimal.mark = ",", scientific = FALSE),
         " rows.")
)

input_connection <- gzfile(input_file, open = "rt")
header_line <- readLines(input_connection, n = 1L, warn = FALSE)
if (length(header_line) != 1L) stop("No header found in the temperature file.")

chunk_number <- 0L
rows_read <- 0
partial_header_written <- FALSE

repeat {
  lines <- readLines(input_connection, n = lines_per_chunk, warn = FALSE)
  if (length(lines) == 0L) break

  chunk_number <- chunk_number + 1L
  rows_read <- rows_read + length(lines)

  chunk <- fread(
    text = paste(c(header_line, lines), collapse = "\n"),
    na.strings = c("", "NA"), showProgress = FALSE
  )

  required_columns <- c("date", "longitude", "latitude", "tn", "tx")
  if (!all(required_columns %in% names(chunk))) {
    stop("The temperature block does not contain all required columns.")
  }

  chunk[, `:=`(
    longitude_key = round(longitude, 6),
    latitude_key = round(latitude, 6)
  )]

  joined <- weights[
    chunk,
    on = .(longitude_key, latitude_key),
    nomatch = 0L,
    allow.cartesian = TRUE
  ]

  partial <- joined[, .(
    tn_w = sum(area_m2[!is.na(tn)]),
    tn_wx = sum(area_m2[!is.na(tn)] * tn[!is.na(tn)]),
    tn_wx2 = sum(area_m2[!is.na(tn)] * tn[!is.na(tn)]^2),
    tx_w = sum(area_m2[!is.na(tx)]),
    tx_wx = sum(area_m2[!is.na(tx)] * tx[!is.na(tx)]),
    tx_wx2 = sum(area_m2[!is.na(tx)] * tx[!is.na(tx)]^2),
    n_cells_tn = sum(!is.na(tn)),
    n_cells_tx = sum(!is.na(tx))
  ), by = .(date, weather_region_id, weather_region_name)]

  fwrite(
    partial, temporary_summaries, append = partial_header_written,
    col.names = !partial_header_written, na = ""
  )
  partial_header_written <- TRUE

  progress(
    "STEP 3/5",
    paste0("Block ", chunk_number, " complete; ",
           format(rows_read, big.mark = ".", decimal.mark = ",", scientific = FALSE),
           " weather rows read.")
  )
}

close(input_connection)

progress("STEP 4/5", "Combining partial sums by day and SHARE region.")

partials <- fread(temporary_summaries, showProgress = TRUE)

sum_columns <- setdiff(
  names(partials),
  c("date", "weather_region_id", "weather_region_name")
)

daily <- partials[, lapply(.SD, sum),
  by = .(date, weather_region_id, weather_region_name),
  .SDcols = sum_columns
]

tn_stats <- weighted_results(daily$tn_w, daily$tn_wx, daily$tn_wx2)
tx_stats <- weighted_results(daily$tx_w, daily$tx_wx, daily$tx_wx2)

daily[, `:=`(
  tn_mean = tn_stats$mean,
  tn_variance = tn_stats$variance,
  tx_mean = tx_stats$mean,
  tx_variance = tx_stats$variance
)]

daily[, `:=`(
  coverage_area_tn_km2 = tn_w / 1e6,
  coverage_area_tx_km2 = tx_w / 1e6
)]

result <- daily[, .(
  date, weather_region_id, weather_region_name,
  tn_mean, tn_variance,
  tx_mean, tx_variance,
  n_cells_tn, n_cells_tx,
  coverage_area_tn_km2, coverage_area_tx_km2
)]

setorder(result, date, weather_region_id)

progress("STEP 5/5", "Writing the compressed result file and validating output.")

fwrite(result, temporary_output, na = "", compress = "gzip")

if (!file.exists(temporary_output) || file.info(temporary_output)$size == 0) {
  stop("The export was not created correctly.")
}

if (file.exists(output_file)) unlink(output_file)

if (!file.rename(temporary_output, output_file)) {
  stop("The temporary output could not be moved to its final name.")
}

unlink(temporary_summaries)

expected_days <- as.integer(as.Date("1965-12-31") - as.Date("1950-01-01")) + 1L

progress("DONE", paste0("File saved: ", output_file))
cat("Days in output:", uniqueN(result$date), "(expected:", expected_days, ")\n")
cat("SHARE regions:", uniqueN(result$weather_region_id), "\n")
cat("Rows:", format(nrow(result), big.mark = ".", decimal.mark = ",",
                   scientific = FALSE), "\n")
cat("Expected complete day-region combinations:",
    format(expected_days * nrow(share_regions), big.mark = ".", decimal.mark = ",",
           scientific = FALSE), "\n")
cat("Note: variances are measured in degrees C squared; means in degrees C.\n")