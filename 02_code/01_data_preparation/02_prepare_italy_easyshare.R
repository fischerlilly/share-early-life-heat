#!/usr/bin/env Rscript

# Select the person-wave variables required for the Italian analysis from easySHARE Release 9.0.0 and verify observation keys.
# Inputs and outputs are defined through the shared project configuration.
source(file.path("02_code", "00_config", "config.R"))




input_file <- file.path(DIR$raw_share, "easyshare", "easySHARE_rel9_0_0.rda")

output_dir <- DIR$processed_italy
output_file <- file.path(
  output_dir,
  "easyshare_italy_selected_variables.csv"
)

share_country_italy <- 16


easy_environment <- new.env(parent = emptyenv())
loaded_object_names <- load(input_file, envir = easy_environment)

if (length(loaded_object_names) != 1L) {
  stop("The RDA file must contain exactly one easySHARE data object.")
}

easyshare <- easy_environment[[loaded_object_names[1]]]


selected_variables <- c(
  "mergeid",
  "dn002_mod",
  "dn003_mod",
  "birth_country",
  "wave",
  "female",
  "sphus",
  "chronic_mod",
  "maxgrip"
)

missing_variables <- setdiff(selected_variables, names(easyshare))
if (length(missing_variables) > 0L) {
  stop(
    "The following variables are missing from easySHARE: ",
    paste(missing_variables, collapse = ", ")
  )
}


italy_rows <- !is.na(easyshare$country) &
  as.numeric(easyshare$country) == share_country_italy

italy <- as.data.frame(
  easyshare[italy_rows, selected_variables],
  stringsAsFactors = FALSE
)

italy$mergeid <- as.character(italy$mergeid)
italy <- italy[order(italy$mergeid, italy$wave), ]
row.names(italy) <- NULL


if (anyNA(italy$mergeid) || any(italy$mergeid == "")) {
  stop("The Italian sample contains missing respondent IDs.")
}

person_wave_key <- paste(italy$mergeid, italy$wave, sep = "__wave__")
if (anyDuplicated(person_wave_key)) {
  stop("At least one person-wave combination is duplicated.")
}


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  italy,
  output_file,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

cat("File created:", output_file, "\n")
cat("Person-wave observations:", nrow(italy), "\n")
cat("Eindeutige Respondents:", length(unique(italy$mergeid)), "\n")
cat("Variablen:", ncol(italy), "\n")
cat("Observations by wave:\n")
print(table(italy$wave, useNA = "ifany"))
