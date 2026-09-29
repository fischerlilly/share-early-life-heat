#!/usr/bin/env Rscript

# Merge the Italian easySHARE person-wave records with the validated, time-invariant birth-region identifiers.
# Inputs and outputs are defined through the shared project configuration.
source(file.path("02_code", "00_config", "config.R"))




input_dir <- DIR$processed_italy

easyshare_file <- file.path(
  input_dir,
  "easyshare_italy_selected_variables.csv"
)

birth_region_file <- file.path(
  input_dir,
  "italy_birth_region_waves3_7.csv"
)

output_file <- file.path(
  input_dir,
  "easyshare_italy_with_birth_region.csv"
)


easyshare <- read.csv(
  easyshare_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

birth_regions <- read.csv(
  birth_region_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)


if (!"mergeid" %in% names(easyshare) || !"mergeid" %in% names(birth_regions)) {
  stop("Both input files must contain mergeid.")
}

if (anyNA(easyshare$mergeid) || any(easyshare$mergeid == "")) {
  stop("The easySHARE file contains missing mergeid values.")
}

if (anyNA(birth_regions$mergeid) || any(birth_regions$mergeid == "")) {
  stop("The birth-region file contains missing mergeid values.")
}

if (anyDuplicated(birth_regions$mergeid)) {
  stop("The birth-region file contains duplicate mergeid values.")
}

person_wave_key_before <- paste(
  easyshare$mergeid,
  easyshare$wave,
  sep = "__wave__"
)
if (anyDuplicated(person_wave_key_before)) {
  stop("The easySHARE file contains duplicate person-wave observations.")
}


combined <- merge(
  easyshare,
  birth_regions,
  by = "mergeid",
  all = FALSE,
  sort = FALSE
)

combined <- combined[order(combined$mergeid, combined$wave), ]
row.names(combined) <- NULL


person_wave_key_after <- paste(
  combined$mergeid,
  combined$wave,
  sep = "__wave__"
)

if (anyDuplicated(person_wave_key_after)) {
  stop("The merge created duplicate person-wave observations.")
}

if (!all(combined$mergeid %in% birth_regions$mergeid)) {
  stop("The result contains respondents without a region-file record.")
}

expected_ids <- intersect(
  unique(easyshare$mergeid),
  unique(birth_regions$mergeid)
)

if (!setequal(unique(combined$mergeid), expected_ids)) {
  stop("The respondent set after the merge does not match the expected set.")
}


write.csv(
  combined,
  output_file,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

cat("File created:", output_file, "\n")
cat("Person-wave rows before merge:", nrow(easyshare), "\n")
cat("Respondents before merge:", length(unique(easyshare$mergeid)), "\n")
cat("Respondents with a validated birth region:",
    length(unique(combined$mergeid)), "\n")
cat("Person-wave rows after merge:", nrow(combined), "\n")
cat("Ausgeschlossene Respondents:",
    length(setdiff(unique(easyshare$mergeid), unique(combined$mergeid))), "\n")
cat("Observations by easySHARE wave:\n")
print(table(combined$wave, useNA = "ifany"))
cat("Respondents by birth-region source:\n")
print(table(
  birth_regions$source_wave[birth_regions$mergeid %in% combined$mergeid],
  useNA = "ifany"
))
