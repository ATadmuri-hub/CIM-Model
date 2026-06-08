# R/24_dose_response.R
#
# Item 13 (Phase 3): Dose-response composition sweep.
#
# Tests whether the H2 composition effect is monotone or threshold-like by
# running 5 dose levels of locals-per-park:
#   - Dose 1: Suboptimal Composition  (~1 local/park,  existing 300 runs)
#   - Dose 2: Composition2            ( 2 locals/park,  new 300 runs)
#   - Dose 3: Composition3            ( 3 locals/park,  new 300 runs)
#   - Dose 4: Composition4            ( 4 locals/park,  new 300 runs)
#   - Dose 5: Baseline                ( 5 locals/park,  existing 300 runs)
#
# Methods:
#   1. Per-dose mean + 95% CI for retention, motivation, CEFR, cross-tie ratio
#   2. Mann-Kendall trend test (distribution-free monotonicity)
#   3. Linear regression: outcome ~ dose (slope = effect per additional local)
#
# Outputs:
#   tables/table_dose_response.csv   -- per-dose summary statistics
#   tables/table_dose_trend_tests.csv -- Mann-Kendall and linear regression results
#   figures/fig_dose_response.png    -- 2x2 panel figure with error bars and fit lines
#   outputs/dose_response.rds        -- full results object
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T3.13 (Phase 3 Item 13)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

DATA_DIR  <- "data"
TAB_DIR   <- "tables"
FIG_DIR   <- "figures"
OUT_DIR   <- "outputs"

dir.create(TAB_DIR, showWarnings = FALSE)
dir.create(FIG_DIR, showWarnings = FALSE)
dir.create(OUT_DIR, showWarnings = FALSE)

# Dose-scenario mapping
DOSE_MAP <- tibble::tribble(
  ~dose, ~scenario,
  1L,    "Suboptimal Composition",
  2L,    "Composition2",
  3L,    "Composition3",
  4L,    "Composition4",
  5L,    "Baseline"
)

OUTCOMES <- c("retention_rate", "avg_motivation", "avg_language_cefr",
              "cross_group_tie_ratio")

# --- Load per-run results from CSVs (uses last-block logic to dedupe) --------
load_scenario_results <- function(scenario) {
  d <- file.path(DATA_DIR, scenario)
  files <- list.files(d, pattern = "^CIM_results_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    warning(sprintf("No results files for %s", scenario))
    return(NULL)
  }
  dfs <- lapply(files, function(fp) {
    lines <- readLines(fp, warn = FALSE)
    header_rows <- which(lines == "metric,value")
    if (length(header_rows) == 0) return(NULL)
    block_start <- tail(header_rows, 1) + 1
    block_lines <- lines[block_start:length(lines)]
    block_lines <- block_lines[nzchar(block_lines)]
    pairs <- block_lines[grepl(",", block_lines)]
    keys <- sub(",.*$", "", pairs)
    vals <- sub("^[^,]+,", "", pairs)
    df <- data.frame(key = keys, value = suppressWarnings(as.numeric(vals)), stringsAsFactors = FALSE)
    df_wide <- as.data.frame(t(df$value))
    colnames(df_wide) <- df$key
    df_wide$scenario <- scenario
    df_wide
  })
  bind_rows(dfs)
}

cat("=== Loading dose-scenario results ===\n")
results_list <- lapply(DOSE_MAP$scenario, load_scenario_results)
results_df <- bind_rows(results_list)
results_df <- results_df %>%
  inner_join(DOSE_MAP, by = "scenario") %>%
  mutate(dose = as.integer(dose))

cat(sprintf("Loaded %d total runs across %d doses\n",
            nrow(results_df), length(unique(results_df$dose))))
print(table(results_df$dose, results_df$scenario))

# --- Per-dose summary statistics ---------------------------------------------
cat("\n=== Per-dose summary (mean +/- 95% CI) ===\n")
summary_df <- results_df %>%
  group_by(dose, scenario) %>%
  summarise(
    n           = n(),
    retention_mean = mean(retention_rate, na.rm = TRUE),
    retention_se   = sd(retention_rate, na.rm = TRUE) / sqrt(n()),
    retention_ci_low  = retention_mean - 1.96 * retention_se,
    retention_ci_high = retention_mean + 1.96 * retention_se,
    motivation_mean = mean(avg_motivation, na.rm = TRUE),
    motivation_se   = sd(avg_motivation, na.rm = TRUE) / sqrt(n()),
    motivation_ci_low  = motivation_mean - 1.96 * motivation_se,
    motivation_ci_high = motivation_mean + 1.96 * motivation_se,
    cefr_mean = mean(avg_language_cefr, na.rm = TRUE),
    cefr_se   = sd(avg_language_cefr, na.rm = TRUE) / sqrt(n()),
    cefr_ci_low  = cefr_mean - 1.96 * cefr_se,
    cefr_ci_high = cefr_mean + 1.96 * cefr_se,
    tie_mean = mean(cross_group_tie_ratio, na.rm = TRUE),
    tie_se   = sd(cross_group_tie_ratio, na.rm = TRUE) / sqrt(n()),
    tie_ci_low  = tie_mean - 1.96 * tie_se,
    tie_ci_high = tie_mean + 1.96 * tie_se,
    .groups = "drop"
  ) %>%
  arrange(dose)

print(summary_df %>% select(dose, scenario, n, retention_mean, motivation_mean, cefr_mean, tie_mean))

write.csv(summary_df, file.path(TAB_DIR, "table_dose_response.csv"), row.names = FALSE)

# --- Mann-Kendall trend test + linear regression per outcome -----------------
cat("\n=== Mann-Kendall trend test + linear regression ===\n")
trend_tests <- lapply(OUTCOMES, function(outcome) {
  d <- results_df %>% select(dose, value = !!sym(outcome)) %>% filter(!is.na(value))
  # Kendall's tau test on (dose, outcome) pairs - tests rank correlation
  ct <- suppressWarnings(cor.test(d$dose, d$value, method = "kendall", exact = FALSE))
  # Linear regression: outcome ~ dose
  lr <- lm(value ~ dose, data = d)
  lr_sum <- summary(lr)
  # Quadratic regression: outcome ~ dose + dose^2 (test for curvature)
  lr_quad <- lm(value ~ dose + I(dose^2), data = d)
  lr_quad_sum <- summary(lr_quad)
  # AIC comparison: positive delta = quadratic preferred over linear
  aic_lin  <- AIC(lr)
  aic_quad <- AIC(lr_quad)
  delta_aic <- aic_lin - aic_quad
  # Likelihood-ratio test for the quadratic term
  lrt <- anova(lr, lr_quad)
  lrt_pvalue <- lrt[["Pr(>F)"]][2]
  list(
    outcome           = outcome,
    kendall_tau       = as.numeric(ct$estimate),
    kendall_pvalue    = as.numeric(ct$p.value),
    lr_slope          = coef(lr)["dose"],
    lr_se             = lr_sum$coefficients["dose", "Std. Error"],
    lr_pvalue         = lr_sum$coefficients["dose", "Pr(>|t|)"],
    lr_r2             = lr_sum$r.squared,
    lr_quad_dose      = coef(lr_quad)["dose"],
    lr_quad_dose2     = coef(lr_quad)["I(dose^2)"],
    lr_quad_r2        = lr_quad_sum$r.squared,
    aic_linear        = aic_lin,
    aic_quadratic     = aic_quad,
    delta_aic         = delta_aic,
    lrt_pvalue        = lrt_pvalue,
    direction         = ifelse(coef(lr)["dose"] > 0, "increasing", "decreasing")
  )
})

trend_df <- bind_rows(lapply(trend_tests, as.data.frame))
print(trend_df)
write.csv(trend_df, file.path(TAB_DIR, "table_dose_trend_tests.csv"), row.names = FALSE)

# --- Visualization: 2x2 panel ------------------------------------------------
cat("\n=== Generating dose-response figure ===\n")

plot_data <- bind_rows(
  summary_df %>% select(dose, mean = retention_mean, ci_low = retention_ci_low, ci_high = retention_ci_high) %>%
    mutate(outcome = "Retention rate (%)"),
  summary_df %>% select(dose, mean = motivation_mean, ci_low = motivation_ci_low, ci_high = motivation_ci_high) %>%
    mutate(outcome = "Mean motivation (week 52)"),
  summary_df %>% select(dose, mean = cefr_mean, ci_low = cefr_ci_low, ci_high = cefr_ci_high) %>%
    mutate(outcome = "Language CEFR gain"),
  summary_df %>% select(dose, mean = tie_mean, ci_low = tie_ci_low, ci_high = tie_ci_high) %>%
    mutate(outcome = "Cross-group tie ratio")
)

# Order panels: retention -> motivation -> CEFR -> tie
plot_data$outcome <- factor(plot_data$outcome, levels = c(
  "Retention rate (%)", "Mean motivation (week 52)",
  "Language CEFR gain", "Cross-group tie ratio"))

p <- ggplot(plot_data, aes(x = dose, y = mean)) +
  geom_line(color = "#440154", linewidth = 0.7, alpha = 0.7) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high),
                  color = "#440154", size = 0.6, fatten = 3.5) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = 1:5,
                     labels = c("1\n(Suboptimal)", "2", "3", "4", "5\n(Baseline)")) +
  labs(
    x = "Dose: locals per park (average)",
    y = NULL,
    title = "Dose-response of group-composition manipulation",
    subtitle = sprintf("Each pointrange = mean across %d runs (95%% CI)",
                       round(mean(summary_df$n)))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    strip.text    = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIG_DIR, "fig_dose_response.png"),
       p, width = 9, height = 6, dpi = 200, bg = "white")
cat(sprintf("Figure saved: %s\n", file.path(FIG_DIR, "fig_dose_response.png")))

# --- Save full results object -----------------------------------------------
saveRDS(list(
  results_df = results_df,
  summary_df = summary_df,
  trend_df   = trend_df,
  metadata   = list(date = Sys.time(), n_doses = 5, n_outcomes = length(OUTCOMES))
), file.path(OUT_DIR, "dose_response.rds"))

cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_dose_response.csv")))
cat(sprintf("Saved: %s\n", file.path(TAB_DIR, "table_dose_trend_tests.csv")))
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "dose_response.rds")))
cat("\n=== Item 13 dose-response analysis complete ===\n")
