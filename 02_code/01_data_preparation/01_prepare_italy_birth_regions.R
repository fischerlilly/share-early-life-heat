#!/usr/bin/env Rscript

# Prepare validated Italian birth-region identifiers from SHARE waves 3 and 7. Wave 3 takes precedence if a respondent appears in both sources.
# Inputs and outputs are defined through the shared project configuration.
source(file.path("02_code", "00_config", "config.R"))



suppressPackageStartupMessages(library(haven))


wave3_dir <- file.path(DIR$raw_share, "wave3")
wave7_dir <- file.path(DIR$raw_share, "wave7")

output_dir <- DIR$processed_italy
output_file <- file.path(output_dir, "italy_birth_region_waves3_7.csv")

share_country_italy <- 16

iso_country_italy <- 380

repair_label_encoding <- function(x) {
  repaired <- iconv(x, from = "UTF-8", to = "latin1")
  Encoding(repaired) <- "UTF-8"
  repaired[is.na(repaired)] <- x[is.na(repaired)]
  repaired
}


ac3 <- read_dta(
  file.path(wave3_dir, "sharew3_rel9-0-0_ac.dta"),
  col_select = c(
    "mergeid", "sl_ac004_", "sl_ac013_1",
    "sl_ac014c_1_iso", "sl_ac015c_1"
  )
)

st3 <- read_dta(
  file.path(wave3_dir, "sharew3_rel9-0-0_st.dta"),
  col_select = c("mergeid", "country")
)
cv3 <- read_dta(
  file.path(wave3_dir, "sharew3_rel9-0-0_cv_r.dta"),
  col_select = c("mergeid", "interview")
)

wave3 <- merge(
  as.data.frame(ac3), as.data.frame(st3),
  by = "mergeid", all.x = TRUE, sort = FALSE
)
wave3 <- merge(
  wave3, as.data.frame(cv3),
  by = "mergeid", all.x = TRUE, sort = FALSE
)

numeric_wave3 <- c(
  "country", "interview", "sl_ac004_", "sl_ac013_1",
  "sl_ac014c_1_iso", "sl_ac015c_1"
)
wave3[numeric_wave3] <- lapply(wave3[numeric_wave3], as.numeric)

wave3$first_residence_country_iso <- ifelse(
  wave3$sl_ac013_1 == 1 & wave3$country == share_country_italy,
  iso_country_italy,
  ifelse(wave3$sl_ac013_1 == 5, wave3$sl_ac014c_1_iso, NA_real_)
)

wave3$birth_region_label <- as.character(
  as_factor(ac3$sl_ac015c_1, levels = "labels")
)[match(wave3$mergeid, ac3$mergeid)]
wave3$birth_region_label <- repair_label_encoding(wave3$birth_region_label)

keep3 <-
  wave3$interview == 1 &
  wave3$first_residence_country_iso == iso_country_italy &
  wave3$sl_ac004_ == 1 &
  !is.na(wave3$sl_ac015c_1) &
  wave3$sl_ac015c_1 >= 0
keep3[is.na(keep3)] <- FALSE

out3 <- data.frame(
  mergeid = as.character(wave3$mergeid[keep3]),
  birth_country_iso = iso_country_italy,
  birth_region_code = wave3$sl_ac015c_1[keep3],
  birth_region_label = wave3$birth_region_label[keep3],
  first_residence_since_birth = 1L,
  source_wave = 3L,
  source_region_variable = "sl_ac015c_1",
  stringsAsFactors = FALSE
)


ra7 <- read_dta(
  file.path(wave7_dir, "sharew7_rel9-0-0_ra.dta"),
  encoding = "latin1",
  col_select = c(
    "mergeid", "ra004_", "ra013_1", "ra014c_1", "ra015c_1"
  )
)

dn7 <- read_dta(
  file.path(wave7_dir, "sharew7_rel9-0-0_dn.dta"),
  col_select = c("mergeid", "country")
)
cv7 <- read_dta(
  file.path(wave7_dir, "sharew7_rel9-0-0_cv_r.dta"),
  col_select = c("mergeid", "interview")
)

wave7 <- merge(
  as.data.frame(ra7), as.data.frame(dn7),
  by = "mergeid", all.x = TRUE, sort = FALSE
)
wave7 <- merge(
  wave7, as.data.frame(cv7),
  by = "mergeid", all.x = TRUE, sort = FALSE
)

numeric_wave7 <- c(
  "country", "interview", "ra004_", "ra013_1", "ra014c_1", "ra015c_1"
)
wave7[numeric_wave7] <- lapply(wave7[numeric_wave7], as.numeric)

wave7$first_residence_country_iso <- ifelse(
  wave7$ra013_1 == 1 & wave7$country == share_country_italy,
  iso_country_italy,
  ifelse(wave7$ra013_1 == 5, wave7$ra014c_1, NA_real_)
)

wave7$birth_region_label <- as.character(
  as_factor(ra7$ra015c_1, levels = "labels")
)[match(wave7$mergeid, ra7$mergeid)]
wave7$birth_region_label <- repair_label_encoding(wave7$birth_region_label)

keep7 <-
  wave7$interview == 1 &
  wave7$first_residence_country_iso == iso_country_italy &
  wave7$ra004_ == 1 &
  !is.na(wave7$ra015c_1) &
  wave7$ra015c_1 >= 0
keep7[is.na(keep7)] <- FALSE

out7 <- data.frame(
  mergeid = as.character(wave7$mergeid[keep7]),
  birth_country_iso = iso_country_italy,
  birth_region_code = wave7$ra015c_1[keep7],
  birth_region_label = wave7$birth_region_label[keep7],
  first_residence_since_birth = 1L,
  source_wave = 7L,
  source_region_variable = "ra015c_1",
  stringsAsFactors = FALSE
)


out <- rbind(out3, out7)
out <- out[order(out$mergeid, out$source_wave), ]
duplicated_ids <- duplicated(out$mergeid)
number_duplicates_removed <- sum(duplicated_ids)
out <- out[!duplicated_ids, ]
row.names(out) <- NULL


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(out, output_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")

cat("File created:", output_file, "\n")
cat("Wave 3:", nrow(out3), "respondents\n")
cat("Wave 7:", nrow(out7), "respondents\n")
cat("Entfernte Doppelungen:", number_duplicates_removed, "\n")
cat("Combined sample:", nrow(out), "respondents\n")
cat("Eindeutige mergeid:", length(unique(out$mergeid)), "\n")
cat("Distinct regions:", length(unique(out$birth_region_code)), "\n")
