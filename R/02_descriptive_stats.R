
# 02_descriptive_stats.R — Summary statistics by scenario
library(tidyverse)

DATA_DIR <- "data"

results_df <- readRDS(file.path(DATA_DIR, "results_df.rds"))

SCENARIOS <- levels(results_df$scenario)

# ---- Table 2: Summary statistics by scenario ----
metrics <- c("retention_rate", "avg_motivation", "avg_language_cefr",
             "cross_group_tie_ratio", "female_dropout_rate", "male_dropout_rate",
             "total_dropouts", "cost_per_retained",
             "stable_participation_wk46_52", "stable_motivation_wk46_52",
             "stable_language_wk46_52", "stable_integration_wk46_52")

summary_table <- results_df %>%
  group_by(scenario) %>%
  summarise(across(all_of(metrics), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd   = ~ sd(.x, na.rm = TRUE),
    cv   = ~ ifelse(mean(.x, na.rm=TRUE) != 0,
                    sd(.x, na.rm=TRUE) / abs(mean(.x, na.rm=TRUE)) * 100, NA)
  ), .names = "{.col}__{.fn}"), .groups = "drop")

cat("=== Summary Statistics ===\n\n")

# Compact display: mean ± SD per scenario per metric
compact <- results_df %>%
  group_by(scenario) %>%
  summarise(
    n             = n(),
    retention     = sprintf("%.1f ± %.1f", mean(retention_rate, na.rm=T), sd(retention_rate, na.rm=T)),
    motivation    = sprintf("%.3f ± %.3f", mean(avg_motivation, na.rm=T), sd(avg_motivation, na.rm=T)),
    language      = sprintf("%.3f ± %.3f", mean(avg_language_cefr, na.rm=T), sd(avg_language_cefr, na.rm=T)),
    cross_tie     = sprintf("%.3f ± %.3f", mean(cross_group_tie_ratio, na.rm=T), sd(cross_group_tie_ratio, na.rm=T)),
    female_drop   = sprintf("%.1f ± %.1f", mean(female_dropout_rate, na.rm=T), sd(female_dropout_rate, na.rm=T)),
    male_drop     = sprintf("%.1f ± %.1f", mean(male_dropout_rate, na.rm=T), sd(male_dropout_rate, na.rm=T)),
    cost_retained = sprintf("%.0f ± %.0f", mean(cost_per_retained, na.rm=T), sd(cost_per_retained, na.rm=T)),
    .groups = "drop"
  )

print(compact, n=Inf, width=150)

# Save
write_csv(compact, file.path(DATA_DIR, "table2_descriptive_stats.csv"))
saveRDS(summary_table, file.path(DATA_DIR, "summary_table_full.rds"))
cat("\nSaved: table2_descriptive_stats.csv, summary_table_full.rds\n")
