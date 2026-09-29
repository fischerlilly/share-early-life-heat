#!/usr/bin/env Rscript

# Italy, Tmin >= 20 C: fully adjusted health models with CR1 clustered
# standard errors and null-imposed wild cluster bootstrap-t p-values.

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

source(file.path("02_code", "00_config", "config.R"))
analysis_file <- INPUT$italy_analysis
isced_file <- INPUT$parental_education
output_csv <- file.path(DIR$table_data, "05_italy_tmin20_wild_cluster.csv")
output_tex <- file.path(DIR$table_latex, "05_italy_tmin20_wild_cluster.tex")
assert_inputs_exist(c(analysis_file, isced_file), "Italy bootstrap input")

bootstrap_replications <- 9999L
bootstrap_seed <- 20260926L
bootstrap_batch_size <- 250L

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
d[, `:=`(
  exposure_10days = clean_nonnegative(tn_days_ge_20_first12m) / 10,
  female_clean = fifelse(female %in% c(0, 1), as.numeric(female), NA_real_),
  mother_isced = clean_nonnegative(isced1997_m),
  father_isced = clean_nonnegative(isced1997_f),
  sphus_clean = clean_nonnegative(sphus),
  maxgrip_clean = clean_nonnegative(maxgrip),
  chronic_clean = clean_nonnegative(chronic_mod),
  region_fe = factor(birth_region_code),
  birth_year_fe = factor(dn003_mod),
  birth_month_fe = factor(dn002_mod)
)]

outcomes <- data.table(
  outcome = c("sphus_clean", "maxgrip_clean", "chronic_clean"),
  label = c("Self-rated health", "Maximum grip strength", "Chronic conditions"),
  column = 1:3
)

cluster_vcov_cr1 <- function(model, cluster) {
  keep <- !is.na(coef(model))
  X <- model.matrix(model)[, keep, drop = FALSE]
  u <- residuals(model)
  cluster <- droplevels(factor(cluster))
  n <- nrow(X); k <- ncol(X); g <- nlevels(cluster)
  bread <- solve(crossprod(X))
  meat <- matrix(0, k, k)
  for (level in levels(cluster)) {
    index <- cluster == level
    score <- crossprod(X[index, , drop = FALSE], u[index])
    meat <- meat + tcrossprod(score)
  }
  correction <- (g / (g - 1)) * ((n - 1) / (n - k))
  correction * bread %*% meat %*% bread
}

# Restricted wild cluster bootstrap-t for H0: beta_exposure = 0.
# Rademacher weights are drawn once per cluster and replication. Computation is
# batched and algebraically vectorized, but reproduces refitting the unrestricted
# model and recomputing its CR1 standard error in every bootstrap replication.
wild_cluster_bootstrap_p <- function(model, cluster, coefficient,
  replications, seed, batch_size) {
  X <- model.matrix(model)
  y <- model.response(model.frame(model))
  coefficient_index <- match(coefficient, colnames(X))
  if (is.na(coefficient_index)) stop("Test coefficient not found in model matrix.")

  cluster <- droplevels(factor(cluster))
  cluster_index <- as.integer(cluster)
  g <- nlevels(cluster)
  n <- nrow(X)
  k <- ncol(X)
  bread <- solve(crossprod(X))

  restricted_X <- X[, -coefficient_index, drop = FALSE]
  restricted_fit <- lm.fit(restricted_X, y)
  restricted_residual <- restricted_fit$residuals

  q <- as.numeric(X %*% bread[, coefficient_index])
  observed_beta <- unname(coef(model)[coefficient])
  observed_vcov <- cluster_vcov_cr1(model, cluster)
  observed_se <- sqrt(observed_vcov[coefficient, coefficient])
  observed_t <- observed_beta / observed_se
  correction <- (g / (g - 1)) * ((n - 1) / (n - k))

  set.seed(seed)
  exceedances <- 0L
  completed <- 0L
  while (completed < replications) {
    b <- min(batch_size, replications - completed)
    weights_by_cluster <- matrix(
      sample(c(-1, 1), g * b, replace = TRUE), nrow = g, ncol = b
    )
    bootstrap_disturbance <- restricted_residual * weights_by_cluster[cluster_index, ]
    x_u <- crossprod(X, bootstrap_disturbance)
    bootstrap_beta_all <- bread %*% x_u
    bootstrap_beta <- bootstrap_beta_all[coefficient_index, ]
    bootstrap_residual <- bootstrap_disturbance - X %*% bootstrap_beta_all

    cluster_scores <- matrix(0, nrow = g, ncol = b)
    for (cluster_number in seq_len(g)) {
      index <- cluster_index == cluster_number
      cluster_scores[cluster_number, ] <- colSums(q[index] * bootstrap_residual[index, , drop = FALSE])
    }
    bootstrap_se <- sqrt(correction * colSums(cluster_scores^2))
    bootstrap_t <- bootstrap_beta / bootstrap_se
    exceedances <- exceedances + sum(abs(bootstrap_t) >= abs(observed_t))
    completed <- completed + b
  }

  # Finite-simulation correction prevents p-values of exactly zero.
  (exceedances + 1) / (replications + 1)
}

fit_outcome <- function(outcome, outcome_index) {
  needed <- c(
    outcome, "exposure_10days", "female_clean", "mother_isced", "father_isced",
    "birth_region_code", "dn003_mod", "dn002_mod"
  )
  model_data <- d[complete.cases(d[, ..needed])]
  model <- lm(
    as.formula(paste(
      outcome,
      "~ exposure_10days + female_clean + factor(mother_isced) +",
      "factor(father_isced) + region_fe + birth_year_fe + birth_month_fe"
    )),
    data = model_data
  )
  covariance <- cluster_vcov_cr1(model, model_data$birth_region_code)
  estimate <- unname(coef(model)["exposure_10days"])
  clustered_se <- sqrt(covariance["exposure_10days", "exposure_10days"])
  clusters <- uniqueN(model_data$birth_region_code)
  conventional_cluster_p <- 2 * pt(
    abs(estimate / clustered_se), df = clusters - 1L, lower.tail = FALSE
  )
  bootstrap_p <- wild_cluster_bootstrap_p(
    model = model,
    cluster = model_data$birth_region_code,
    coefficient = "exposure_10days",
    replications = bootstrap_replications,
    seed = bootstrap_seed + outcome_index,
    batch_size = bootstrap_batch_size
  )
  data.table(
    estimate = estimate,
    clustered_se = clustered_se,
    cluster_p_value = conventional_cluster_p,
    wild_bootstrap_p_value = bootstrap_p,
    observations = nobs(model),
    clusters = clusters,
    outcome_mean = mean(model_data[[outcome]]),
    r_squared = summary(model)$r.squared
  )
}

results <- rbindlist(lapply(seq_len(nrow(outcomes)), function(i) {
  z <- fit_outcome(outcomes$outcome[i], i)
  z[, `:=`(
    outcome = outcomes$outcome[i],
    outcome_label = outcomes$label[i],
    model_column = outcomes$column[i]
  )]
  z
}))
setcolorder(results, c(
  "model_column", "outcome", "outcome_label", "estimate", "clustered_se",
  "cluster_p_value", "wild_bootstrap_p_value", "observations", "clusters",
  "outcome_mean", "r_squared"
))
fwrite(results, output_csv, na = "")

values <- function(field, format_string) sprintf(format_string, results[[field]])
line_end <- intToUtf8(c(92, 92))
tex_row <- function(title, entries) {
  paste0(title, " & ", paste(entries, collapse = " & "), " ", line_end)
}

latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Early-life minimum-temperature exposure and adult health in Italy}",
  "\\label{tab:italy-tmin20-wild-cluster}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  tex_row("", paste0("(", 1:3, ")")),
  tex_row("", results$outcome_label),
  "\\midrule",
  tex_row("Heat exposure ($T_{\\min} \\geq 20^{\\circ}$C)", values("estimate", "%.3f")),
  tex_row("Clustered SE", paste0("(", values("clustered_se", "%.3f"), ")")),
  tex_row("Clustered $p$-value", paste0("[", values("cluster_p_value", "%.3f"), "]")),
  tex_row("Wild bootstrap $p$-value", paste0("[", values("wild_bootstrap_p_value", "%.3f"), "]")),
  "\\midrule",
  tex_row("Gender control", rep("Yes", 3)),
  tex_row("Mother's education", rep("Yes", 3)),
  tex_row("Father's education", rep("Yes", 3)),
  tex_row("Region FE", rep("Yes", 3)),
  tex_row("Birth-year FE", rep("Yes", 3)),
  tex_row("Birth-month FE", rep("Yes", 3)),
  "\\midrule",
  tex_row("Observations", format(results$observations, big.mark = ",", scientific = FALSE)),
  tex_row("Clusters", as.character(results$clusters)),
  tex_row("Outcome mean", values("outcome_mean", "%.2f")),
  tex_row("$R^2$", values("r_squared", "%.3f")),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.94\\textwidth}\\footnotesize",
  paste0(
    "\\textit{Notes:} Each column reports a separate OLS regression for Italian respondents born in 1950--1964 and observed in SHARE Wave 7. ",
    "Heat exposure is the number of days during the first 12 months of life for which the area-weighted regional mean daily minimum temperature was at least $20^{\\circ}$C; coefficients are per ten additional days. ",
    "All models include gender, categorical maternal and paternal ISCED-97 controls, early-life-region fixed effects, birth-year fixed effects, and birth-month fixed effects. ",
    "Parentheses report CR1 standard errors clustered by early-life region (20 clusters); the conventional clustered $p$-values use a $t$ distribution with 19 degrees of freedom. ",
    "Brackets report null-imposed restricted wild cluster bootstrap-$t$ $p$-values using Rademacher weights, 9,999 replications, and clustering by early-life region. ",
    "Outcomes are in their original units."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(latex, output_tex, useBytes = TRUE)

cat("Saved:", output_tex, "\n")
cat("Saved:", output_csv, "\n")
print(results)
