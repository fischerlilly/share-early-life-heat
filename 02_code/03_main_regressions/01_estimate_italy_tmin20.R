#!/usr/bin/env Rscript

# Italy, Wave 7: three adult health outcomes and three nested specifications.
# Coefficients refer to ten additional days with Tmin >= 20 C during the first
# twelve months of life. Outcomes remain in their original units.

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

source(file.path("02_code", "00_config", "config.R"))

analysis_file <- INPUT$italy_analysis
isced_file <- INPUT$parental_education

output_tex <- file.path(
  DIR$estimates,
  "italy_tmin20_health_outcomes.tex"
)

output_csv <- file.path(
  DIR$estimates,
  "italy_tmin20_health_outcomes.csv"
)

assert_inputs_exist(
  c(analysis_file, isced_file),
  "Italy Tmin regression input"
)

d <- fread(
  analysis_file,
  na.strings = c("", "NA")
)[wave == 7]

parents <- as.data.table(
  read_dta(
    isced_file,
    col_select = c(
      "mergeid",
      "isced1997_m",
      "isced1997_f"
    )
  )
)

parents[, mergeid := as.character(mergeid)]

if (anyDuplicated(parents$mergeid)) {
  stop("Duplicate mergeid in Wave-7 gv_isced.")
}

d[, mergeid := as.character(mergeid)]

d <- parents[
  d,
  on = "mergeid"
]

if (anyDuplicated(d$mergeid)) {
  stop("Duplicate mergeid after parental-education merge.")
}

clean_nonnegative <- function(x) {
  fifelse(
    !is.na(x) & as.numeric(x) >= 0,
    as.numeric(x),
    NA_real_
  )
}

d[, `:=`(
  heat_10days = clean_nonnegative(
    tn_days_ge_20_first12m
  ) / 10,

  female_clean = fifelse(
    female %in% c(0, 1),
    as.numeric(female),
    NA_real_
  ),

  mother_isced = clean_nonnegative(
    isced1997_m
  ),

  father_isced = clean_nonnegative(
    isced1997_f
  ),

  birth_year_fe = factor(
    dn003_mod
  ),

  birth_month_fe = factor(
    dn002_mod
  ),

  region_fe = factor(
    birth_region_code
  )
)]

outcomes <- data.table(
  variable = c(
    "sphus",
    "maxgrip",
    "chronic_mod"
  ),

  label = c(
    "Self-rated health",
    "Maximum grip strength",
    "Chronic conditions"
  )
)

for (v in outcomes$variable) {
  d[, (paste0(v, "_clean")) :=
      clean_nonnegative(get(v))]
}

cluster_vcov_cr1 <- function(model, cluster) {

  keep <- !is.na(coef(model))

  X <- model.matrix(model)[
    ,
    keep,
    drop = FALSE
  ]

  u <- residuals(model)

  cluster <- droplevels(
    factor(cluster)
  )

  n <- nrow(X)
  k <- ncol(X)
  g <- nlevels(cluster)

  if (g < 2L || n <= k) {
    stop("CR1 covariance cannot be computed.")
  }

  bread <- solve(
    crossprod(X)
  )

  meat <- matrix(
    0,
    k,
    k
  )

  for (level in levels(cluster)) {

    index <- cluster == level

    score <- crossprod(
      X[index, , drop = FALSE],
      u[index]
    )

    meat <- meat + tcrossprod(score)
  }

  (g / (g - 1)) *
    ((n - 1) / (n - k)) *
    bread %*%
    meat %*%
    bread
}

fit_one <- function(outcome, specification) {

  rhs <- c(
    "heat_10days",
    "region_fe",
    "birth_year_fe",
    "birth_month_fe"
  )

  if (specification >= 2L) {
    rhs <- c(
      rhs,
      "female_clean"
    )
  }

  if (specification >= 3L) {
    rhs <- c(
      rhs,
      "factor(mother_isced)",
      "factor(father_isced)"
    )
  }

  needed <- c(
    outcome,
    "heat_10days",
    "birth_region_code",
    "dn003_mod",
    "dn002_mod",

    if (specification >= 2L) {
      "female_clean"
    },

    if (specification >= 3L) {
      c(
        "mother_isced",
        "father_isced"
      )
    }
  )

  model_data <- d[
    complete.cases(
      d[, ..needed]
    )
  ]

  model <- lm(
    as.formula(
      paste(
        outcome,
        "~",
        paste(
          rhs,
          collapse = " + "
        )
      )
    ),
    data = model_data
  )

  covariance <- cluster_vcov_cr1(
    model,
    model_data$birth_region_code
  )

  estimate <- unname(
    coef(model)["heat_10days"]
  )

  standard_error <- sqrt(
    covariance[
      "heat_10days",
      "heat_10days"
    ]
  )

  clusters <- uniqueN(
    model_data$birth_region_code
  )

  p_value <- 2 * pt(
    abs(
      estimate /
        standard_error
    ),
    df = clusters - 1L,
    lower.tail = FALSE
  )

  data.table(
    estimate = estimate,
    standard_error = standard_error,
    p_value = p_value,
    observations = nobs(model),
    outcome_mean = mean(
      model_data[[outcome]]
    ),
    r_squared = summary(model)$r.squared,
    clusters = clusters
  )
}

results <- rbindlist(
  lapply(
    seq_len(nrow(outcomes)),
    function(i) {

      outcome <- paste0(
        outcomes$variable[i],
        "_clean"
      )

      rbindlist(
        lapply(
          1:3,
          function(specification) {

            result <- fit_one(
              outcome,
              specification
            )

            result[, `:=`(
              variable = outcomes$variable[i],
              label = outcomes$label[i],
              specification = specification
            )]

            result
          }
        )
      )
    }
  )
)

results[, stars := fcase(
  p_value < 0.01, "***",
  p_value < 0.05, "**",
  p_value < 0.10, "*",
  default = ""
)]

setcolorder(
  results,
  c(
    "variable",
    "label",
    "specification",
    "estimate",
    "standard_error",
    "p_value",
    "observations",
    "outcome_mean",
    "r_squared",
    "clusters",
    "stars"
  )
)

fwrite(
  results,
  output_csv,
  na = ""
)

cell <- function(
  variable_name,
  specification_number,
  field
) {

  results[
    variable == variable_name &
      specification == specification_number
  ][[field]][1]
}

fmt_coef <- function(v, s) {
  sprintf(
    "%.3f%s",
    cell(v, s, "estimate"),
    cell(v, s, "stars")
  )
}

fmt_se <- function(v, s) {
  sprintf(
    "(%.3f)",
    cell(v, s, "standard_error")
  )
}

fmt_n <- function(v, s) {
  format(
    cell(v, s, "observations"),
    big.mark = ",",
    scientific = FALSE
  )
}

fmt_mean <- function(v, s) {
  sprintf(
    "%.2f",
    cell(v, s, "outcome_mean")
  )
}

fmt_r2 <- function(v, s) {
  sprintf(
    "%.3f",
    cell(v, s, "r_squared")
  )
}

specifications <- unlist(
  lapply(
    outcomes$variable,
    function(v) 1:3
  ),
  use.names = FALSE
)

variables <- rep(
  outcomes$variable,
  each = 3
)

line_end <- intToUtf8(
  c(92, 92)
)

tex_row <- function(title, values) {

  paste0(
    title,
    " & ",
    paste(
      values,
      collapse = " & "
    ),
    " ",
    line_end
  )
}

group_header <- paste0(
  " & ",
  paste(
    sprintf(
      "\\multicolumn{3}{c}{%s}",
      outcomes$label
    ),
    collapse = " & "
  ),
  " ",
  line_end
)

cmidrules <- paste(
  vapply(
    seq_len(nrow(outcomes)),
    function(i) {

      start <- 2L +
        (i - 1L) * 3L

      sprintf(
        "\\cmidrule(lr){%d-%d}",
        start,
        start + 2L
      )
    },
    character(1)
  ),
  collapse = " "
)

latex <- c(

  "\\begin{table}[!htbp]",

  "\\centering",

  "\\caption{Early-life heat exposure and adult health outcomes in Italy}",

  "\\label{tab:italy-health-outcomes}",

  "\\resizebox{\\textwidth}{!}{%",

  paste0(
    "\\begin{tabular}{l",
    paste(
      rep("c", 9),
      collapse = ""
    ),
    "}"
  ),

  "\\toprule",

  group_header,

  cmidrules,

  tex_row(
    "",
    rep(
      c("(1)", "(2)", "(3)"),
      3
    )
  ),

  "\\midrule",

  tex_row(
    "Heat exposure ($T_{\\min} \\geq 20^{\\circ}$C)",
    mapply(
      fmt_coef,
      variables,
      specifications
    )
  ),

  tex_row(
    "",
    mapply(
      fmt_se,
      variables,
      specifications
    )
  ),

  "\\midrule",

  tex_row(
    "Gender control",
    rep(
      c("No", "Yes", "Yes"),
      3
    )
  ),

  tex_row(
    "Mother's education",
    rep(
      c("No", "No", "Yes"),
      3
    )
  ),

  tex_row(
    "Father's education",
    rep(
      c("No", "No", "Yes"),
      3
    )
  ),

  tex_row(
    "Region FE",
    rep(
      "Yes",
      9
    )
  ),

  tex_row(
    "Birth-year FE",
    rep(
      "Yes",
      9
    )
  ),

  tex_row(
    "Birth-month FE",
    rep(
      "Yes",
      9
    )
  ),

  "\\midrule",

  tex_row(
    "Observations",
    mapply(
      fmt_n,
      variables,
      specifications
    )
  ),

  tex_row(
    "Outcome mean",
    mapply(
      fmt_mean,
      variables,
      specifications
    )
  ),

  tex_row(
    "$R^2$",
    mapply(
      fmt_r2,
      variables,
      specifications
    )
  ),

  "\\bottomrule",

  "\\end{tabular}}",

  "\\begin{minipage}{0.99\\textwidth}\\footnotesize",

  paste0(
    "\\textit{Notes:} OLS estimates for Italian respondents born in 1950--1964 and observed in SHARE Wave 7. ",
    "Heat exposure is the number of days during the first 12 months of life on which the area-weighted regional mean of the daily minimum temperature was at least $20^{\\circ}$C; coefficients are per ten additional days. ",
    "All outcomes are in their original units. Parental education is controlled flexibly with ISCED-97 category indicators. ",
    "Standard errors in parentheses are clustered by early-life region. ",
    "Outcome means and $R^2$ refer to each model's estimation sample. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
  ),

  "\\end{minipage}",

  "\\end{table}"
)

writeLines(
  latex,
  output_tex,
  useBytes = TRUE
)

cat(
  "Saved:",
  output_tex,
  "\n"
)

cat(
  "Saved:",
  output_csv,
  "\n"
)

cat(
  "Wave-7 analysis sample:",
  nrow(d),
  "\n"
)

cat(
  "Valid mother ISCED:",
  sum(!is.na(d$mother_isced)),
  "\n"
)

cat(
  "Valid father ISCED:",
  sum(!is.na(d$father_isced)),
  "\n"
)