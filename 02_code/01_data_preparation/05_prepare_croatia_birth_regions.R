#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))


# Croatia: early-life region from SHARE Wave 7.
# Croatia was not part of SHARE Wave 3, so no Wave-3 region file exists.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

progress <- function(step, text) {
  cat(sprintf("[%s] %s %s\n", format(Sys.time(), "%H:%M:%S"), step, text))
  flush.console()
}

progress("START", "Prepare Croatian early-life regions from SHARE Wave 7.")

raw_dir <- file.path(DIR$raw_share, "wave7")
output_dir <- DIR$processed_croatia
output_file <- file.path(output_dir, "croatia_early_life_regions_wave7.csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ra <- read_dta(
  file.path(raw_dir, "sharew7_rel9-0-0_ra.dta"),
  encoding = "latin1",
  col_select = c("mergeid", "ra004_", "ra013_1", "ra014c_1", "ra015c_1")
)
dn <- read_dta(
  file.path(raw_dir, "sharew7_rel9-0-0_dn.dta"),
  col_select = c("mergeid", "country")
)
cv <- read_dta(
  file.path(raw_dir, "sharew7_rel9-0-0_cv_r.dta"),
  col_select = c("mergeid", "interview")
)

region_labels <- data.table(
  mergeid = as.character(ra$mergeid),
  early_life_region_label = as.character(as_factor(ra$ra015c_1, levels = "labels"))
)

x <- merge(as.data.frame(ra), as.data.frame(dn), by = "mergeid", all.x = TRUE, sort = FALSE)
x <- merge(x, as.data.frame(cv), by = "mergeid", all.x = TRUE, sort = FALSE)
x <- as.data.table(x)
for (variable in c("ra004_", "ra013_1", "ra014c_1", "ra015c_1", "country", "interview")) {
  x[, (variable) := as.numeric(get(variable))]
}
x[, mergeid := as.character(mergeid)]
x <- region_labels[x, on = "mergeid"]

share_country_croatia <- 47
iso_country_croatia <- 191
x[, first_residence_country_iso := fifelse(
  ra013_1 == 1 & country == share_country_croatia,
  iso_country_croatia,
  fifelse(ra013_1 == 5, ra014c_1, NA_real_)
)]

keep <- x$interview == 1 &
  x$first_residence_country_iso == iso_country_croatia &
  x$ra004_ == 1 &
  x$ra015c_1 %between% c(4701, 4720)
keep[is.na(keep)] <- FALSE

out <- x[keep, .(
  mergeid,
  birth_country_iso = iso_country_croatia,
  early_life_region_code = as.integer(ra015c_1),
  early_life_region_label,
  first_residence_since_birth = 1L,
  source_wave = 7L,
  source_region_variable = "ra015c_1"
)]

region_crosswalk <- data.table(
  early_life_region_code = 4701:4720,
  weather_region_id = c(
    "HR_ZAGREB", "HR064", "HR028", "HR027", "HR062",
    "HR063", "HR021", "HR031", "HR032", "HR022",
    "HR023", "HR024", "HR033", "HR025", "HR034",
    "HR026", "HR035", "HR036", "HR037", "HR061"
  )
)
out <- region_crosswalk[out, on = "early_life_region_code"]
setorder(out, mergeid)

if (anyDuplicated(out$mergeid)) stop("Duplicate mergeid in Croatian region file.")
if (uniqueN(out$early_life_region_code) != 20L) {
  warning("Not all 20 Croatian SHARE regions occur in the retained sample.")
}

fwrite(out, output_file, na = "")
progress("DONE", paste("Saved:", output_file))
cat("Persons:", nrow(out), "\n")
cat("SHARE early-life regions:", uniqueN(out$early_life_region_code), "\n")

