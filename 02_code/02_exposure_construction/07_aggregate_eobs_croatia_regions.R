#!/usr/bin/env Rscript

source(file.path("02_code", "00_config", "config.R"))
source(file.path("02_code", "00_config", "share_regions.R"))

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
})

progress <- function(s, x) {
  cat(sprintf(
    "[%s] %s %s\n",
    format(Sys.time(), "%H:%M:%S"),
    s,
    x
  ))
  flush.console()
}

weighted_stats <- function(w, wx, wx2) {
  m <- fifelse(w > 0, wx / w, NA_real_)

  list(
    mean = m,
    variance = fifelse(
      w > 0,
      pmax(wx2 / w - m^2, 0),
      NA_real_
    )
  )
}


# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

input_dir <- DIR$processed_croatia

input_file <- file.path(
  input_dir,
  "eobs_daily_1950_1965_croatia.csv.gz"
)

grid_file <- file.path(
  input_dir,
  "eobs_gridcells_overlapping_croatia.csv"
)

nuts_file <- INPUT$nuts_2024

regions_file <- file.path(
  input_dir,
  "croatia_share_region_boundaries.gpkg"
)

weights_file <- file.path(
  input_dir,
  "eobs_gridcell_croatia_share_region_area_weights.csv"
)

output_file <- file.path(
  input_dir,
  "eobs_daily_1950_1965_croatia_share_region_area_weighted.csv.gz"
)

partial_file <- file.path(
  input_dir,
  "temporary_daily_partial_sums.csv"
)

lines_per_chunk <- 500000L


# -------------------------------------------------------------------------
# STEP 1: Construct SHARE-region polygons and area weights
# -------------------------------------------------------------------------

progress(
  "1/5",
  "Construct SHARE-region polygons and intersection weights."
)

grid <- fread(grid_file)

half <- 0.05

cell_polygon <- function(lon, lat) {
  st_polygon(
    list(
      matrix(
        c(
          lon - half, lat - half,
          lon + half, lat - half,
          lon + half, lat + half,
          lon - half, lat + half,
          lon - half, lat - half
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )
}


grid_sf <- st_sf(
  cell_id = grid$cell_id,
  longitude_key = round(grid$longitude, 6),
  latitude_key = round(grid$latitude, 6),
  geometry = st_sfc(
    Map(
      cell_polygon,
      grid$longitude,
      grid$latitude
    ),
    crs = 4326
  )
)


share_regions <- build_share_region_geometries(
  "croatia",
  nuts_file,
  crs = 3035
)

share_regions$weather_region_name <-
  share_regions$region_name

share_regions <- share_regions[, c(
  "early_life_region_code",
  "weather_region_id",
  "weather_region_name"
)]


if (file.exists(regions_file)) {
  unlink(regions_file)
}

st_write(
  share_regions,
  regions_file,
  quiet = TRUE
)


suppressWarnings(
  overlap <- st_intersection(
    st_transform(grid_sf, 3035),
    share_regions
  )
)

overlap$area_m2 <- as.numeric(
  st_area(overlap)
)


weights <- as.data.table(
  st_drop_geometry(overlap)
)[
  area_m2 > 0,
  .(
    cell_id,
    longitude_key,
    latitude_key,
    early_life_region_code,
    weather_region_id,
    weather_region_name,
    area_m2
  )
]


# Geometry repair can occasionally create more than one intersection
# fragment for a cell-region pair. Combine those fragments without
# changing the region.

weights <- weights[
  ,
  .(
    longitude_key = first(longitude_key),
    latitude_key = first(latitude_key),
    early_life_region_code = first(early_life_region_code),
    weather_region_name = first(weather_region_name),
    area_m2 = sum(area_m2)
  ),
  by = .(
    cell_id,
    weather_region_id
  )
]


setkey(
  weights,
  longitude_key,
  latitude_key
)

fwrite(
  weights,
  weights_file
)


if (uniqueN(weights$weather_region_id) != 20L) {
  stop("Expected 20 SHARE-compatible regions.")
}


# -------------------------------------------------------------------------
# STEP 2: Create daily weighted partial sums
# -------------------------------------------------------------------------

progress(
  "2/5",
  "Create daily weighted partial sums for Tmin and Tmax."
)


if (file.exists(partial_file)) {
  unlink(partial_file)
}


con <- gzfile(
  input_file,
  "rt"
)

header <- readLines(
  con,
  n = 1L,
  warn = FALSE
)

chunk_id <- 0L
rows_read <- 0
append_output <- FALSE


repeat {

  lines <- readLines(
    con,
    n = lines_per_chunk,
    warn = FALSE
  )

  if (!length(lines)) {
    break
  }


  chunk_id <- chunk_id + 1L
  rows_read <- rows_read + length(lines)


  d <- fread(
    text = paste(
      c(header, lines),
      collapse = "\n"
    ),
    na.strings = c("", "NA")
  )


  # Check that the two required temperature variables are present.

  required_temperature_columns <- c(
    "tn",
    "tx"
  )

  if (!all(required_temperature_columns %in% names(d))) {
    stop(
      "Required temperature columns tn and tx are missing from the input file."
    )
  }


  d[, longitude_key := round(longitude, 6)]
  d[, latitude_key := round(latitude, 6)]


  j <- weights[
    d,
    on = .(
      longitude_key,
      latitude_key
    ),
    nomatch = 0L,
    allow.cartesian = TRUE
  ]


  # Area-weighted partial sums for Tmin and Tmax only.
  # Mean temperature (tg) is no longer used.

  p <- j[
    ,
    .(
      tn_w = sum(
        area_m2[!is.na(tn)]
      ),

      tn_wx = sum(
        area_m2[!is.na(tn)] *
          tn[!is.na(tn)]
      ),

      tn_wx2 = sum(
        area_m2[!is.na(tn)] *
          tn[!is.na(tn)]^2
      ),

      tx_w = sum(
        area_m2[!is.na(tx)]
      ),

      tx_wx = sum(
        area_m2[!is.na(tx)] *
          tx[!is.na(tx)]
      ),

      tx_wx2 = sum(
        area_m2[!is.na(tx)] *
          tx[!is.na(tx)]^2
      ),

      n_cells_tn = sum(!is.na(tn)),
      n_cells_tx = sum(!is.na(tx))
    ),
    by = .(
      date,
      weather_region_id,
      weather_region_name
    )
  ]


  fwrite(
    p,
    partial_file,
    append = append_output,
    col.names = !append_output
  )

  append_output <- TRUE


  progress(
    "2/5",
    paste(
      "Chunk",
      chunk_id,
      "complete;",
      rows_read,
      "rows read."
    )
  )
}


close(con)


# -------------------------------------------------------------------------
# STEP 3: Combine partial sums
# -------------------------------------------------------------------------

progress(
  "3/5",
  "Combine partial sums and calculate regional Tmin and Tmax."
)


p <- fread(partial_file)


sum_cols <- setdiff(
  names(p),
  c(
    "date",
    "weather_region_id",
    "weather_region_name"
  )
)


d <- p[
  ,
  lapply(.SD, sum),
  by = .(
    date,
    weather_region_id,
    weather_region_name
  ),
  .SDcols = sum_cols
]


# Calculate area-weighted regional Tmin.

tn <- weighted_stats(
  d$tn_w,
  d$tn_wx,
  d$tn_wx2
)


# Calculate area-weighted regional Tmax.

tx <- weighted_stats(
  d$tx_w,
  d$tx_wx,
  d$tx_wx2
)


d[, tn_mean := tn$mean]
d[, tn_variance := tn$variance]

d[, tx_mean := tx$mean]
d[, tx_variance := tx$variance]


# Area with available temperature information.

d[, coverage_area_tn_km2 := tn_w / 1e6]
d[, coverage_area_tx_km2 := tx_w / 1e6]


# Keep only final variables.

result <- d[
  ,
  .(
    date,
    weather_region_id,
    weather_region_name,

    tn_mean,
    tn_variance,

    tx_mean,
    tx_variance,

    n_cells_tn,
    n_cells_tx,

    coverage_area_tn_km2,
    coverage_area_tx_km2
  )
]


setorder(
  result,
  date,
  weather_region_id
)


# -------------------------------------------------------------------------
# STEP 4: Validate daily region panel
# -------------------------------------------------------------------------

progress(
  "4/5",
  "Validate daily region panel."
)


expected_days <- as.integer(
  as.Date("1965-12-31") -
    as.Date("1950-01-01")
) + 1L


if (
  uniqueN(result$date) != expected_days ||
  uniqueN(result$weather_region_id) != 20L
) {
  stop("Incomplete panel.")
}


if (
  nrow(result) != expected_days * 20L
) {
  stop("Unexpected daily panel row count.")
}


if (
  anyDuplicated(
    result[
      ,
      .(
        date,
        weather_region_id
      )
    ]
  )
) {
  stop(
    "Duplicate date-region combinations in final output."
  )
}


if (
  anyNA(result$tn_mean) ||
  anyNA(result$tx_mean)
) {
  stop(
    "Missing regional Tmin or Tmax values in final output."
  )
}


# -------------------------------------------------------------------------
# STEP 5: Write output
# -------------------------------------------------------------------------

progress(
  "5/5",
  "Write daily Croatian SHARE-region file."
)


fwrite(
  result,
  output_file,
  na = "",
  compress = "gzip"
)


unlink(partial_file)


cat(
  "Saved:",
  output_file,
  "\n"
)

cat(
  "Rows:",
  nrow(result),
  "\n"
)

cat(
  "Days:",
  uniqueN(result$date),
  "\n"
)

cat(
  "SHARE regions:",
  uniqueN(result$weather_region_id),
  "\n"
)

cat(
  "Temperature variables: Tmin and Tmax\n"
)
