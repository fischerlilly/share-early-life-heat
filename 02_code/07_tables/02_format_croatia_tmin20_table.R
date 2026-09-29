#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

source(file.path("02_code", "00_config", "config.R"))

country <- "Croatia"
country_key <- "croatia"
input_file <- file.path(DIR$estimates, "croatia_tmin20_health_outcomes.csv")
output_csv <- file.path(DIR$table_data, "01_croatia_tmin20_health.csv")
output_tex <- file.path(DIR$table_latex, "01_croatia_tmin20_health.tex")
assert_inputs_exist(input_file, "Croatia Tmin table input")

keep <- data.table(
  variable = c("sphus", "maxgrip", "chronic_mod"),
  label = c("Self-rated health", "Maximum grip strength", "Chronic conditions")
)
results <- fread(input_file)[variable %in% keep$variable]
results[, outcome_order := match(variable, keep$variable)]
setorder(results, outcome_order, specification)
results[, outcome_order := NULL]
fwrite(results, output_csv, na = "")

cell <- function(variable_name, specification_number, field) {
  results[variable == variable_name & specification == specification_number][[field]][1]
}
fmt_coef <- function(v, s) sprintf("%.3f%s", cell(v, s, "estimate"), cell(v, s, "stars"))
fmt_se <- function(v, s) sprintf("(%.3f)", cell(v, s, "standard_error"))
fmt_n <- function(v, s) format(cell(v, s, "observations"), big.mark = ",", scientific = FALSE)
fmt_mean <- function(v, s) sprintf("%.2f", cell(v, s, "outcome_mean"))
fmt_r2 <- function(v, s) sprintf("%.3f", cell(v, s, "r_squared"))

variables <- rep(keep$variable, each = 3)
specifications <- rep(1:3, times = 3)
line_end <- intToUtf8(c(92, 92))
tex_row <- function(title, values) {
  paste0(title, " & ", paste(values, collapse = " & "), " ", line_end)
}
group_header <- paste0(
  " & ", paste(sprintf("\\multicolumn{3}{c}{%s}", keep$label), collapse = " & "),
  " ", line_end
)

latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  sprintf("\\caption{Early-life heat exposure and adult health outcomes in %s}", country),
  sprintf("\\label{tab:%s-three-health-outcomes}", country_key),
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lccccccccc}",
  "\\toprule",
  group_header,
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
  tex_row("", rep(c("(1)", "(2)", "(3)"), 3)),
  "\\midrule",
  tex_row("Heat exposure ($T_{\\min} \\geq 20^{\\circ}$C)",
    mapply(fmt_coef, variables, specifications)),
  tex_row("", mapply(fmt_se, variables, specifications)),
  "\\midrule",
  tex_row("Gender control", rep(c("No", "Yes", "Yes"), 3)),
  tex_row("Mother's education", rep(c("No", "No", "Yes"), 3)),
  tex_row("Father's education", rep(c("No", "No", "Yes"), 3)),
  tex_row("Region FE", rep("Yes", 9)),
  tex_row("Birth-year FE", rep("Yes", 9)),
  tex_row("Birth-month FE", rep("Yes", 9)),
  "\\midrule",
  tex_row("Observations", mapply(fmt_n, variables, specifications)),
  tex_row("Outcome mean", mapply(fmt_mean, variables, specifications)),
  tex_row("$R^2$", mapply(fmt_r2, variables, specifications)),
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.99\\textwidth}\\footnotesize",
  paste0(
    "\\textit{Notes:} OLS estimates for Croatian respondents born in 1950--1964 and observed in SHARE Wave 7. ",
    "Heat exposure is the number of days during the first 12 months of life on which the area-weighted regional mean of the daily minimum temperature was at least $20^{\\circ}$C; coefficients are per ten additional days. ",
    "Outcomes are in their original units. Parental education is controlled flexibly with ISCED-97 category indicators. ",
    "Standard errors in parentheses are clustered by early-life region. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(latex, output_tex, useBytes = TRUE)
cat("Saved:", output_tex, "\nSaved:", output_csv, "\n")
