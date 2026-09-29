#!/usr/bin/env Rscript

# Restrict the Italian sample to respondents born from January 1950 through December 1964 while retaining every available survey wave for eligible respondents.
# Inputs and outputs are defined through the shared project configuration.
source(file.path("02_code", "00_config", "config.R"))



progress <- function(step, text) {
  cat(sprintf("[%s] %s %s\n", format(Sys.time(), "%H:%M:%S"), step, text))
  flush.console()
}

progress("START", "Starting the birth-month cohort filter.")


input_file <- file.path(
  DIR$processed_italy,
  "easyshare_italy_with_birth_region.csv"
)
output_dir <- DIR$processed_italy
output_file <- file.path(
  output_dir,
  "easyshare_italy_with_birth_region_birth_cohort_1950_1964.csv"
)

first_birth_month <- as.Date("1950-01-01")
last_birth_month <- as.Date("1964-12-01")

if (!file.exists(input_file)) stop("Input file not found: ", input_file)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


progress("STEP 1/4", "Reading the combined Italian SHARE file.")

share <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

required_columns <- c("mergeid", "wave", "dn002_mod", "dn003_mod")
missing_columns <- setdiff(required_columns, names(share))
if (length(missing_columns) > 0L) {
  stop("The following required columns are missing: ", paste(missing_columns, collapse = ", "))
}
if (anyNA(share$mergeid) || any(share$mergeid == "")) {
  stop("The input file contains missing mergeid values.")
}
if (anyDuplicated(paste(share$mergeid, share$wave, sep = "__wave__"))) {
  stop("The input file contains duplicate person-wave observations.")
}

rows_before <- nrow(share)
persons_before <- length(unique(share$mergeid))


progress("STEP 2/4", "Checking birth year and month at respondent level.")

birth_month <- suppressWarnings(as.integer(share$dn002_mod))
birth_year <- suppressWarnings(as.integer(share$dn003_mod))

valid_birth_components <-
  !is.na(birth_year) &
  !is.na(birth_month) &
  birth_year >= 0L &
  birth_month >= 1L & birth_month <= 12L

birth_month_date <- rep(as.Date(NA), nrow(share))
birth_month_date[valid_birth_components] <- as.Date(sprintf(
  "%04d-%02d-01",
  birth_year[valid_birth_components],
  birth_month[valid_birth_components]
))

birth_check <- data.frame(
  mergeid = share$mergeid,
  birth_month_date = birth_month_date,
  stringsAsFactors = FALSE
)

birth_values_by_person <- split(birth_check$birth_month_date, birth_check$mergeid)
unique_valid_births <- lapply(birth_values_by_person, function(x) {
  unique(x[!is.na(x)])
})

inconsistent_ids <- names(unique_valid_births)[lengths(unique_valid_births) > 1L]
if (length(inconsistent_ids) > 0L) {
  stop(
    "Conflicting valid birth months within the same respondent: ",
    paste(head(inconsistent_ids, 10L), collapse = ", ")
  )
}

person_birth <- data.frame(
  mergeid = names(unique_valid_births),
  birth_month_date = as.Date(vapply(
    unique_valid_births,
    function(x) if (length(x) == 1L) as.character(x) else NA_character_,
    character(1L)
  )),
  stringsAsFactors = FALSE
)

eligible_ids <- person_birth$mergeid[
  !is.na(person_birth$birth_month_date) &
  person_birth$birth_month_date >= first_birth_month &
  person_birth$birth_month_date <= last_birth_month
]


progress("STEP 3/4", "Retaining all waves for eligible birth cohorts.")

filtered <- share[share$mergeid %in% eligible_ids, , drop = FALSE]
filtered <- filtered[order(filtered$mergeid, filtered$wave), , drop = FALSE]
row.names(filtered) <- NULL

if (!all(filtered$mergeid %in% eligible_ids)) {
  stop("The output contains an ineligible respondent.")
}
if (!setequal(unique(filtered$mergeid), eligible_ids)) {
  stop("Not all eligible respondents are present in the output.")
}
if (anyDuplicated(paste(filtered$mergeid, filtered$wave, sep = "__wave__"))) {
  stop("Filtering created duplicate person-wave observations.")
}

progress("STEP 4/4", "Writing the filtered CSV file.")

write.csv(
  filtered,
  output_file,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

persons_after <- length(unique(filtered$mergeid))

progress("DONE", paste0("File saved: ", output_file))
cat("Respondents before filtering:", persons_before, "\n")
cat("Respondents after filtering:", persons_after, "\n")
cat("Ausgeschlossene Respondents:", persons_before - persons_after, "\n")
cat("Person-wave rows before filtering:", rows_before, "\n")
cat("Person-wave rows after filtering:", nrow(filtered), "\n")
cat("Birth period: 1950-01 through 1964-12\n")
cat("Observations by easySHARE wave:\n")
print(table(filtered$wave, useNA = "ifany"))
