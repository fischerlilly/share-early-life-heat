#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
  library(scales)
  library(dplyr)
})

source(file.path("02_code", "00_config", "config.R"))
source(file.path("02_code", "00_config", "share_regions.R"))
nuts_file <- INPUT$nuts_2024
region_file <- file.path(DIR$processed_croatia, "croatia_share_regions.csv")
grid_file <- file.path(DIR$processed_croatia, "eobs_gridcells_overlapping_croatia.csv")
output_dir <- DIR$figures
assert_inputs_exist(c(nuts_file, region_file, grid_file), "Croatia figure input")

regions_data <- fread(region_file)
grid_data <- fread(grid_file)

# -----------------------------------------------------------------------------
# 1. Construct 20 SHARE-compatible Croatian regions
# -----------------------------------------------------------------------------

regions_sf <- build_share_region_geometries("croatia", nuts_file, crs = 3035)
regions_sf <- merge(
  regions_sf,
  regions_data[, .(share_code, sample_n)],
  by = "share_code",
  all.x = TRUE
)

if (nrow(regions_sf) != 20L || anyNA(regions_sf$sample_n)) {
  stop("Failed to construct all 20 analytical Croatian regions.")
}

# -----------------------------------------------------------------------------
# 2. Label positions and common map extent
# -----------------------------------------------------------------------------

label_points <- st_point_on_surface(regions_sf)
coordinates <- st_coordinates(label_points)
labels <- data.frame(
  share_code = regions_sf$share_code,
  short_label = regions_sf$short_label,
  sample_n = regions_sf$sample_n,
  x_origin = coordinates[, 1], y_origin = coordinates[, 2],
  x = coordinates[, 1], y = coordinates[, 2]
)

# Croatia's counties are compact and several meet around Zagreb. Fixed offsets
# and leader lines keep every name and N legible without stochastic placement.
offsets <- data.table(
  share_code = sprintf("R%02d", 1:20),
  dx = c(
    -100000, -175000, -150000, -170000, -70000,
    35000, 120000, -185000, -170000, 145000,
    135000, 150000, -155000, 180000, -150000,
    185000, 145000, -185000, 155000, 45000
  ),
  dy = c(
    10000, 105000, -20000, -80000, 155000,
    145000, 65000, 25000, -70000, 105000,
    30000, -25000, -95000, 85000, -120000,
    15000, -145000, 125000, -175000, 190000
  )
)
labels <- merge(labels, offsets, by = "share_code", all.x = TRUE)
labels$x <- labels$x + labels$dx
labels$y <- labels$y + labels$dy

bbox <- st_bbox(regions_sf)
x_limits <- c(bbox["xmin"] - 260000, bbox["xmax"] + 260000)
y_limits <- c(bbox["ymin"] - 220000, bbox["ymax"] + 230000)
fill_limits <- c(0, max(regions_sf$sample_n))
fill_breaks <- c(25, 50, 75, 100)

common_theme <- theme_void(base_size = 10.5, base_family = "serif") +
  theme(
    plot.margin = margin(4, 8, 4, 4),
    legend.position = "right",
    legend.title = element_text(size = 9.5),
    legend.text = element_text(size = 8.5),
    legend.key.height = unit(0.65, "in"),
    legend.key.width = unit(0.17, "in")
  )
common_fill <- scale_fill_gradient(
  low = "#E8F1F8", high = "#2166AC",
  limits = fill_limits, breaks = fill_breaks,
  name = "Respondents", oob = squish
)

# -----------------------------------------------------------------------------
# 3. Figure 1: regions, names, and Wave-7 sample sizes
# -----------------------------------------------------------------------------

figure1 <- ggplot() +
  geom_sf(data = regions_sf, aes(fill = sample_n), color = "white", linewidth = 0.48) +
  geom_segment(
    data = labels,
    aes(x = x_origin, y = y_origin, xend = x, yend = y),
    linewidth = 0.27, color = "grey30"
  ) +
  geom_label(
    data = labels,
    aes(x = x, y = y, label = paste0(short_label, "\nN = ", sample_n)),
    size = 1.85, family = "serif", lineheight = 0.86,
    linewidth = 0.13, label.padding = unit(0.065, "lines"),
    fill = alpha("white", 0.90), color = "grey10"
  ) +
  common_fill +
  coord_sf(xlim = x_limits, ylim = y_limits, datum = NA, expand = FALSE, clip = "off") +
  common_theme

# -----------------------------------------------------------------------------
# 4. Figure 2: exact E-OBS grid-region intersections
# -----------------------------------------------------------------------------

half_cell <- 0.05
make_cell <- function(longitude, latitude) {
  st_polygon(list(matrix(c(
    longitude - half_cell, latitude - half_cell,
    longitude + half_cell, latitude - half_cell,
    longitude + half_cell, latitude + half_cell,
    longitude - half_cell, latitude + half_cell,
    longitude - half_cell, latitude - half_cell
  ), ncol = 2, byrow = TRUE)))
}
grid_sf <- st_sf(
  cell_id = grid_data$cell_id,
  geometry = st_sfc(Map(make_cell, grid_data$longitude, grid_data$latitude), crs = 4326)
)
grid_sf <- st_make_valid(st_transform(grid_sf, 3035))

grid_region_parts <- suppressWarnings(st_intersection(
  grid_sf[, "cell_id"],
  regions_sf[, c("share_code", "sample_n")]
))
if (nrow(grid_region_parts) <= nrow(grid_sf)) {
  stop("Expected boundary cells to produce additional intersection pieces.")
}

figure2 <- ggplot() +
  geom_sf(
    data = grid_sf,
    fill = "#D9D9D9", color = alpha("white", 0.90), linewidth = 0.07
  ) +
  geom_sf(
    data = grid_region_parts, aes(fill = sample_n),
    color = alpha("white", 0.76), linewidth = 0.07, show.legend = "polygon"
  ) +
  geom_sf(
    data = regions_sf, aes(fill = sample_n),
    alpha = 0, color = NA, show.legend = TRUE
  ) +
  geom_sf(data = regions_sf, fill = NA, color = "#202020", linewidth = 0.58) +
  common_fill +
  coord_sf(xlim = x_limits, ylim = y_limits, datum = NA, expand = FALSE, clip = "off") +
  common_theme

ggsave(
  file.path(output_dir, "03_croatia_share_regions.png"),
  figure1, width = 7.0, height = 8.2, units = "in", dpi = 400, bg = "white"
)
ggsave(
  file.path(output_dir, "04_croatia_eobs_intersections.png"),
  figure2, width = 7.0, height = 8.2, units = "in", dpi = 400, bg = "white"
)

cat("Saved clean Croatian PNG figures in", output_dir, "\n")
cat("E-OBS cells:", nrow(grid_sf), "\n")
cat("Exact grid-region intersection pieces:", nrow(grid_region_parts), "\n")
