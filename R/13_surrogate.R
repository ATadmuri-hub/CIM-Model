# 13_surrogate.R — Scenario comparison table (surrogate regression removed)
# CIM v6.4 — Calisthenics Integration Model
#
# NOTE: A surrogate/emulator regression was previously included in this script
# but has been removed because all available predictors were post-treatment
# simulation outputs (motivation, cross-tie ratio, language, dropout rates),
# not ex ante design variables. Predicting one outcome from other co-produced
# outcomes creates information leakage and circular reasoning.
#
# Decision support is grounded instead on:
#   - Scenario contrasts (section 3 below)
#   - Monte Carlo uncertainty quantification (95% CIs)
#   - Discrete-time hazard analysis (R/11_agent_hazard.R)
#   - Policy target probabilities (R/09_policy_outputs.R)

library(tidyverse)

DATA_DIR <- "data"
FIGS_DIR <- "figures"
dir.create(FIGS_DIR, showWarnings = FALSE)

results_df <- readRDS(file.path(DATA_DIR, "results_df.rds"))

# ── 1. Scenario-level summary statistics ─────────────────────────────────────
scenario_features <- results_df %>%
  group_by(scenario) %>%
  summarise(
    mean_retention    = mean(retention_rate,        na.rm = TRUE),
    mean_motivation   = mean(avg_motivation,        na.rm = TRUE),
    mean_language     = mean(avg_language_cefr,     na.rm = TRUE),
    mean_cross_tie    = mean(cross_group_tie_ratio, na.rm = TRUE),
    mean_cost         = mean(cost_per_retained,     na.rm = TRUE),
    mean_female_drop  = mean(female_dropout_rate,   na.rm = TRUE),
    mean_male_drop    = mean(male_dropout_rate,     na.rm = TRUE),
    sd_retention      = sd(retention_rate,          na.rm = TRUE),
    p10_retention     = quantile(retention_rate, 0.10, na.rm = TRUE),
    p90_retention     = quantile(retention_rate, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

cat("Scenario features summary:\n")
print(scenario_features)

write_csv(scenario_features, file.path(DATA_DIR, "scenario_features.csv"))
cat("Saved: scenario_features.csv\n")

# ── 2. Scenario-level comparison table (delta vs Baseline) ──────────────────
baseline_ret <- scenario_features %>%
  filter(scenario == "Baseline") %>%
  pull(mean_retention)

comparison_tbl <- scenario_features %>%
  mutate(
    delta_retention = mean_retention - baseline_ret,
    pct_change      = delta_retention / baseline_ret * 100
  ) %>%
  arrange(desc(mean_retention))

cat("\n=== Scenario Retention vs Baseline ===\n")
print(comparison_tbl %>%
        select(scenario, mean_retention, delta_retention, pct_change, sd_retention))

write_csv(comparison_tbl, file.path(DATA_DIR, "scenario_comparison.csv"))
cat("Saved: scenario_comparison.csv\n")

cat("\n=== Script 13 complete ===\n")
