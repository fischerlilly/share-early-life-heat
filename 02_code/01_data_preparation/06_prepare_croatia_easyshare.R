#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))


# Selected easySHARE variables for all Croatian person-wave observations.

suppressPackageStartupMessages(library(data.table))

input_file <- file.path(DIR$raw_share, "easyshare", "easySHARE_rel9_0_0.rda")
output_dir <- DIR$processed_croatia
output_file <- file.path(output_dir, "easyshare_croatia_selected_variables.csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

easy_environment <- new.env(parent = emptyenv())
loaded <- load(input_file, envir = easy_environment)
if (length(loaded) != 1L) stop("Expected exactly one object in easySHARE RDA.")
easy <- as.data.table(easy_environment[[loaded]])

selected <- c(
  "mergeid", "dn002_mod", "dn003_mod", "birth_country", "wave",
  "female", "sphus", "chronic_mod", "maxgrip"
)
missing <- setdiff(selected, names(easy))
if (length(missing)) stop("Missing easySHARE variables: ", paste(missing, collapse = ", "))

croatia <- easy[as.numeric(country) == 47, ..selected]
croatia[, mergeid := as.character(mergeid)]
setorder(croatia, mergeid, wave)
if (anyDuplicated(croatia[, .(mergeid, wave)])) {
  stop("Duplicate Croatian person-wave observations.")
}

fwrite(croatia, output_file, na = "")
cat("Saved:", output_file, "\n")
cat("Person-wave observations:", nrow(croatia), "\n")
cat("Persons:", uniqueN(croatia$mergeid), "\n")
print(croatia[, .N, by = wave][order(wave)])

