#!/usr/bin/env Rscript

# Publication-style coefficient plot for the Tmin threshold sensitivity models.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

source(file.path("02_code", "00_config", "config.R"))
input_file <- file.path(DIR$estimates, "italy_tmin_18_22_health_coefficients.csv")
output_pdf <- file.path(DIR$figures, "05_italy_tmin_sensitivity.pdf")
output_png <- file.path(DIR$figures, "05_italy_tmin_sensitivity.png")
assert_inputs_exist(input_file, "Sensitivity-figure input")

d <- fread(input_file)
d[, outcome_label := factor(
  outcome_label,
  levels = c("Self-rated health", "Maximum grip strength", "Chronic conditions")
)]

# Outcomes are shown in separate panels because their coefficients have very
# different scales. This avoids artificial horizontal offsets at each thermal
# threshold and ensures that all three estimates are displayed at 18--22 C.
figure <- ggplot(
  d,
  aes(x = threshold, y = estimate)
) +
  geom_hline(
    yintercept = 0, linewidth = 0.55, linetype = "dashed",
    color = "#D73027"
  ) +
  geom_line(linewidth = 0.58, linetype = "solid", color = "grey42") +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.10, linewidth = 0.58, linetype = "solid", color = "grey38"
  ) +
  geom_point(size = 2.15, shape = 16, color = "grey20") +
  facet_wrap(
    vars(outcome_label), ncol = 1, scales = "free_y",
    strip.position = "top"
  ) +
  scale_x_continuous(
    breaks = 18:22,
    minor_breaks = seq(18, 22, by = 0.5),
    name = expression(paste("Minimum-temperature threshold, ", degree, "C"))
  ) +
  scale_y_continuous(
    name = expression(hat(beta)~"per 10 additional exposure days"),
    expand = expansion(mult = c(0.10, 0.12))
  ) +
  theme_bw(base_size = 11.5, base_family = "sans") +
  theme(
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey84", linewidth = 0.32),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.24),
    panel.border = element_rect(color = "grey40", linewidth = 0.45),
    strip.background = element_rect(fill = "grey94", color = "grey40", linewidth = 0.45),
    strip.text = element_text(face = "bold", color = "black", margin = margin(4, 0, 4, 0)),
    panel.spacing = unit(0.55, "lines"),
    plot.margin = margin(8, 12, 8, 8)
  )

ggsave(output_pdf, figure, width = 7.2, height = 7.6, units = "in", device = "pdf")
ggsave(output_png, figure, width = 7.2, height = 7.6, units = "in", dpi = 400,
  bg = "white")

cat("Saved:", output_pdf, "\n")
cat("Saved:", output_png, "\n")
