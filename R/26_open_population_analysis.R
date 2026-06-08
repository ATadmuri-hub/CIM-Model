# R/26_open_population_analysis.R
#
# Item 12 (Phase 3): Open-population robustness analysis.
#
# Tests whether base outcomes are robust to open-cohort dynamics by comparing
# Baseline (closed cohort) vs OpenPopulation (continuous inflow/outflow at
# 30% / 20% weekly Bernoulli rates) on six outcomes.
#
# OpenPopulation scenario was built in Tier 3 Block J (apply-inflow-outflow
# at NetLogo line 1951). The 300-run experiment was already executed; this
# script provides the v6.4 thesis-pipeline analysis of those results.
#
# Methods:
#   1. Per-outcome Welch t-test (Baseline vs OpenPopulation)
#   2. Cohen's d effect size
#   3. Holm correction within ROBUSTNESS family (D2 from architecture audit)
#   4. Comparative magnitude vs H1-H4 confirmatory effect sizes
#
# Output:
#   tables/table_open_pop.csv         -- side-by-side comparison + tests
#   tables/table_open_pop_vs_h1h4.csv -- relative magnitude vs confirmatory
#   figures/fig_open_pop.png          -- 6-panel side-by-side figure
#   outputs/open_pop.rds              -- full results object
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T3.12 (Phase 3 Item 12)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(effsize)
  library(tidyr)
})

DATA_DIR <- "data"
TAB_DIR  <- "tables"
FIG_DIR  <- "figures"
OUT_DIR  <- "outputs"

OUTCOMES <- c("retention_rate", "avg_motivation", "avg_language_cefr",
              "cross_group_tie_ratio", "female_dropout_rate", "male_dropout_rate")

OUTCOME_LABELS <- c(
  retention_rate         = "Retention rate (%)",
  avg_motivation         = "Mean motivation",
  avg_language_cefr      = "Language CEFR gain",
  cross_group_tie_ratio  = "Cross-group tie ratio",
  female_dropout_rate    = "Female dropout rate (%)",
  male_dropout_rate      = "Male dropout rate (%)"
)

# --- Load data ---------------------------------------------------------------
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
    vals <- suppressWarnings(as.numeric(sub("^[^,]+,", "", pairs)))
    df <- data.frame(t(vals)); colnames(df) <- keys
    df$scenario <- scenario
    df
  })
  bind_rows(dfs)
}

cat("=== Loading Baseline and OpenPopulation results ===\n")
baseline_df <- load_scenario_results("Baseline")
openpop_df  <- load_scenario_results("OpenPopulation")
cat(sprintf("Baseline: %d runs\n", nrow(baseline_df)))
cat(sprintf("OpenPopulation: %d runs\n", nrow(openpop_df)))

# --- Welch t-tests + Cohen's d ----------------------------------------------
cat("\n=== Welch t-tests + Cohen's d ===\n")
results_tests <- lapply(OUTCOMES, function(outcome) {
  x_base <- na.omit(baseline_df[[outcome]])
  x_open <- na.omit(openpop_df[[outcome]])
  if (length(x_base) < 2 || length(x_open) < 2) return(NULL)
  t_res <- t.test(x_open, x_base, var.equal = FALSE)
  d_res <- cohen.d(x_open, x_base)
  data.frame(
    outcome     = outcome,
    label       = OUTCOME_LABELS[outcome],
    n_baseline  = length(x_base),
    n_openpop   = length(x_open),
    mean_baseline = mean(x_base),
    mean_openpop  = mean(x_open),
    diff        = mean(x_open) - mean(x_base),
    diff_pct    = (mean(x_open) - mean(x_base)) / abs(mean(x_base)) * 100,
    ci_low      = t_res$conf.int[1],
    ci_high     = t_res$conf.int[2],
    t_stat      = t_res$statistic,
    df          = t_res$parameter,
    p_raw       = t_res$p.value,
    cohens_d    = d_res$estimate,
    d_magnitude = as.character(d_res$magnitude)
  )
}) %>% bind_rows()

# Holm correction within ROBUSTNESS family (Item 12 contributes 6 tests)
results_tests$p_adj_holm <- p.adjust(results_tests$p_raw, method = "holm")
results_tests$sig_holm   <- results_tests$p_adj_holm < 0.05

print(results_tests %>%
      select(label, mean_baseline, mean_openpop, diff, cohens_d,
             d_magnitude, p_raw, p_adj_holm, sig_holm))

write.csv(results_tests, file.path(TAB_DIR, "table_open_pop.csv"), row.names = FALSE)

# --- Comparative magnitude vs H1-H4 effect sizes -----------------------------
# Read existing confirmatory test results
cat("\n=== Comparative magnitude: OpenPop effect vs H1-H4 effect sizes ===\n")
# Raw .rds, not the csv: R/07 reformats table3_hypothesis_tests.csv and drops `family`.
welch_holm <- tryCatch(
  as.data.frame(readRDS(file.path(DATA_DIR, "hypothesis_tests.rds"))),
  error = function(e) NULL)

if (!is.null(welch_holm)) {
  confirmatory <- welch_holm %>%
    filter(family == "confirmatory") %>%
    select(scenario, outcome, h_diff = diff, h_d = cohens_d)

  # For each outcome, compare OpenPop diff to confirmatory diffs
  comparison <- results_tests %>%
    select(outcome, openpop_diff = diff, openpop_d = cohens_d) %>%
    rowwise() %>%
    mutate(
      h1_diff = confirmatory$h_diff[confirmatory$scenario == "Weak Peer Influence"        & confirmatory$outcome == outcome][1],
      h2_diff = confirmatory$h_diff[confirmatory$scenario == "Suboptimal Composition"     & confirmatory$outcome == outcome][1],
      h3_diff = confirmatory$h_diff[confirmatory$scenario == "No Indoor Continuity"       & confirmatory$outcome == outcome][1],
      h4_diff = confirmatory$h_diff[confirmatory$scenario == "Minimal Support"            & confirmatory$outcome == outcome][1],
      ratio_h1 = openpop_diff / h1_diff,
      ratio_h2 = openpop_diff / h2_diff,
      ratio_h3 = openpop_diff / h3_diff,
      ratio_h4 = openpop_diff / h4_diff
    ) %>%
    ungroup()

  print(comparison %>% select(outcome, openpop_diff, h1_diff, h2_diff, h3_diff, h4_diff))

  # Interpretation column: smaller-than-H? → robust; larger-than-H? → fragile
  comparison <- comparison %>%
    mutate(robustness_note = case_when(
      abs(openpop_diff) < abs(h1_diff) & abs(openpop_diff) < abs(h2_diff) &
      abs(openpop_diff) < abs(h3_diff) & abs(openpop_diff) < abs(h4_diff) ~ "ROBUST: open-cohort effect smaller than all H1-H4 manipulations",
      TRUE ~ "MIXED: open-cohort effect comparable to or larger than some H1-H4"
    ))

  write.csv(comparison, file.path(TAB_DIR, "table_open_pop_vs_h1h4.csv"), row.names = FALSE)
  cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_open_pop_vs_h1h4.csv")))
} else {
  cat("(table3_hypothesis_tests.csv not found, skipping comparative magnitude)\n")
}

# --- Visualization: 6-panel side-by-side ------------------------------------
cat("\n=== Generating figure ===\n")

plot_data <- bind_rows(
  baseline_df %>% mutate(scenario = "Baseline (closed)") %>%
    select(scenario, all_of(OUTCOMES)),
  openpop_df %>% mutate(scenario = "OpenPopulation (open)") %>%
    select(scenario, all_of(OUTCOMES))
) %>%
  pivot_longer(cols = all_of(OUTCOMES), names_to = "outcome", values_to = "value") %>%
  mutate(
    outcome_label = factor(OUTCOME_LABELS[outcome], levels = OUTCOME_LABELS),
    scenario      = factor(scenario, levels = c("Baseline (closed)", "OpenPopulation (open)"))
  )

# Per-scenario means and 95% CIs
summary_for_plot <- plot_data %>%
  group_by(scenario, outcome_label) %>%
  summarise(
    mean    = mean(value, na.rm = TRUE),
    se      = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    ci_low  = mean - 1.96 * se,
    ci_high = mean + 1.96 * se,
    .groups = "drop"
  )

p <- ggplot(summary_for_plot, aes(x = mean, y = scenario, color = scenario)) +
  geom_line(aes(group = outcome_label), color = "gray60", linewidth = 0.4) +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), size = 0.7, fatten = 4) +
  facet_wrap(~ outcome_label, scales = "free_x", ncol = 3) +
  scale_color_viridis_d(end = 0.85) +
  scale_fill_viridis_d(end = 0.85) +
  labs(
    x = NULL, y = NULL, color = NULL, fill = NULL,
    title = "Robustness under open-cohort dynamics",
    subtitle = sprintf("Baseline (closed cohort, n=%d) vs OpenPopulation (30%% inflow / 20%% outflow per week, n=%d). Error bars: 95%% CI.",
                       nrow(baseline_df), nrow(openpop_df))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    strip.text    = element_text(face = "bold", size = 10),
    axis.text.y   = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIG_DIR, "fig_open_pop.png"), p, width = 11, height = 6, dpi = 200, bg = "white")
cat(sprintf("Figure saved: %s\n", file.path(FIG_DIR, "fig_open_pop.png")))

# --- Save full results -------------------------------------------------------
saveRDS(list(
  results_tests = results_tests,
  baseline_df   = baseline_df,
  openpop_df    = openpop_df,
  metadata      = list(date = Sys.time(), n_baseline = nrow(baseline_df), n_openpop = nrow(openpop_df))
), file.path(OUT_DIR, "open_pop.rds"))

cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_open_pop.csv")))
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "open_pop.rds")))
cat("\n=== Item 12 open-population robustness analysis complete ===\n")
