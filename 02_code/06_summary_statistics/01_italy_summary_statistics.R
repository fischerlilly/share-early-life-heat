#!/usr/bin/env Rscript

# Summary statistics for the variables used in the Italian health regressions.

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

source(file.path("02_code", "00_config", "config.R"))
analysis_file <- INPUT$italy_analysis
isced_file <- INPUT$parental_education
output_csv <- file.path(DIR$table_data, "04_italy_summary_statistics.csv")
output_tex <- file.path(DIR$table_latex, "04_italy_summary_statistics.tex")
assert_inputs_exist(c(analysis_file, isced_file), "Italy summary-statistics input")

d <- fread(analysis_file, na.strings = c("", "NA"))[wave == 7]
parents <- as.data.table(read_dta(
  isced_file,
  col_select = c("mergeid", "isced1997_m", "isced1997_f")
))
parents[, mergeid := as.character(mergeid)]
d[, mergeid := as.character(mergeid)]
d <- parents[d, on = "mergeid"]
if (anyDuplicated(d$mergeid)) stop("Duplicate mergeid after parental-education merge.")

clean_nonnegative <- function(x) {
  fifelse(!is.na(x) & as.numeric(x) >= 0, as.numeric(x), NA_real_)
}
clean_isced <- function(x) {
  # ISCED levels are 0--6; negative missing codes and 95/97 are not cardinal
  # education levels and therefore do not enter numerical summary statistics.
  fifelse(!is.na(x) & as.numeric(x) %in% 0:6, as.numeric(x), NA_real_)
}

d[, `:=`(
  female_clean = fifelse(female %in% c(0, 1), as.numeric(female), NA_real_),
  mother_isced_clean = clean_isced(isced1997_m),
  father_isced_clean = clean_isced(isced1997_f),
  tmin20_days = clean_nonnegative(tn_days_ge_20_first12m),
  sphus_clean = clean_nonnegative(sphus),
  maxgrip_clean = clean_nonnegative(maxgrip),
  chronic_clean = clean_nonnegative(chronic_mod)
)]

variables <- data.table(
  variable = c(
    "female_clean", "mother_isced_clean", "father_isced_clean",
    "tmin20_days", "sphus_clean", "maxgrip_clean", "chronic_clean"
  ),
  label = c(
    "Female", "Mother's education (ISCED-97)",
    "Father's education (ISCED-97)",
    "Heat exposure ($T_{\\min} \\geq 20^{\\circ}$C), days",
    "Self-rated health", "Maximum grip strength", "Chronic conditions"
  )
)

summary_table <- rbindlist(lapply(seq_len(nrow(variables)), function(i) {
  x <- d[[variables$variable[i]]]
  x <- x[!is.na(x)]
  data.table(
    variable = variables$variable[i],
    Variable = variables$label[i],
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    Max = max(x),
    N = length(x)
  )
}))
fwrite(summary_table, output_csv, na = "")

line_end <- intToUtf8(c(92, 92))
escape_label <- function(x) x
latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Summary statistics for the Italian Wave-7 health sample}",
  "\\label{tab:italy-health-summary-statistics}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  paste0("Variable & Mean & SD & Min & Max & N ", line_end),
  "\\midrule"
)
for (i in seq_len(nrow(summary_table))) {
  row <- summary_table[i]
  latex <- c(latex, paste0(
    escape_label(row$Variable), " & ",
    sprintf("%.2f", row$Mean), " & ",
    sprintf("%.2f", row$SD), " & ",
    sprintf("%.2f", row$Min), " & ",
    sprintf("%.2f", row$Max), " & ",
    format(row$N, big.mark = ",", scientific = FALSE), " ", line_end
  ))
}
latex <- c(
  latex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.92\\textwidth}\\footnotesize",
  paste0(
    "\\textit{Notes:} The sample consists of Italian SHARE respondents born in 1950--1964 and observed in Wave 7. ",
    "Female is an indicator equal to one for women. Parental education is coded using ISCED-97 levels 0--6; negative missing codes and non-ordinal category 97 (Other) are excluded from numerical summaries. ",
    "$T_{\\min}$ denotes the area-weighted regional mean of daily minimum temperature. Heat exposure is the number of days during the first 12 months of life for which $T_{\\min} \\geq 20^{\\circ}$C. ",
    "Statistics are calculated variable by variable, so N differs because of item nonresponse."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(latex, output_tex, useBytes = TRUE)

cat("Saved:", output_tex, "\n")
cat("Saved:", output_csv, "\n")
print(summary_table)
