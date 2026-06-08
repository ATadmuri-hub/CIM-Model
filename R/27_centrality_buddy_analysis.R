# R/27_centrality_buddy_analysis.R
#
# Item 10 (Phase 3): CentralityBuddy 3-way comparison.
#
# Tests whether degree-based mid-programme buddy pairing outperforms the
# existing distance-based setup-time BuddyProgram. Key contrasts:
#   - Baseline vs BuddyProgram      (does buddy program help at all?)
#   - Baseline vs CentralityBuddy   (does targeted version help?)
#   - BuddyProgram vs CentralityBuddy (does TARGETING help vs random matching?)
#
# Both buddy variants give 8 weeks of +15% attendance boost; differ in:
#   - Pairing TIME: BuddyProgram week 0; CentralityBuddy week 8 (mid-programme)
#   - Pairing CRITERION: BuddyProgram nearest by distance;
#                        CentralityBuddy highest local friendship degree
#
# Outputs:
#   tables/table_centrality_buddy.csv         -- 3-way comparison + tests
#   figures/fig_centrality_buddy.png          -- 6-panel side-by-side
#   outputs/centrality_buddy.rds              -- full results object
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T3.10 (Phase 3 Item 10)

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

SCENARIOS <- c("Baseline", "BuddyProgram", "RandomBuddy", "CentralityBuddy")

# --- Load data ---------------------------------------------------------------
load_scenario_results <- function(scenario) {
  d <- file.path(DATA_DIR, scenario)
  files <- list.files(d, pattern = "^CIM_results_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
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

cat("=== Loading 3 scenarios ===\n")
data_list <- lapply(SCENARIOS, function(s) {
  d <- load_scenario_results(s)
  cat(sprintf("  %s: %d runs\n", s, ifelse(is.null(d), 0, nrow(d))))
  d
})
names(data_list) <- SCENARIOS

# --- Pairwise Welch t-tests + Cohen's d --------------------------------------
cat("\n=== 3-way pairwise tests (Welch + Cohen's d + Holm within Item 10) ===\n")

pairwise_test <- function(scen_a, scen_b, outcome) {
  x_a <- na.omit(data_list[[scen_a]][[outcome]])
  x_b <- na.omit(data_list[[scen_b]][[outcome]])
  if (length(x_a) < 2 || length(x_b) < 2) return(NULL)
  t_res <- t.test(x_a, x_b, var.equal = FALSE)
  d_res <- cohen.d(x_a, x_b)
  data.frame(
    scenario_a  = scen_a,
    scenario_b  = scen_b,
    outcome     = outcome,
    label       = OUTCOME_LABELS[outcome],
    n_a         = length(x_a),
    n_b         = length(x_b),
    mean_a      = mean(x_a),
    mean_b      = mean(x_b),
    diff        = mean(x_a) - mean(x_b),
    diff_pct    = (mean(x_a) - mean(x_b)) / abs(mean(x_b)) * 100,
    ci_low      = t_res$conf.int[1],
    ci_high     = t_res$conf.int[2],
    t_stat      = t_res$statistic,
    df          = t_res$parameter,
    p_raw       = t_res$p.value,
    cohens_d    = d_res$estimate,
    d_magnitude = as.character(d_res$magnitude)
  )
}

# Run pairwise contrasts × 6 outcomes
# 4-way comparison: 6 pairs × 6 outcomes = 36 tests
contrasts <- list(
  c("BuddyProgram",    "Baseline"),
  c("RandomBuddy",     "Baseline"),
  c("CentralityBuddy", "Baseline"),
  c("RandomBuddy",     "BuddyProgram"),
  c("CentralityBuddy", "BuddyProgram"),
  c("CentralityBuddy", "RandomBuddy")
)

results_tests <- bind_rows(lapply(contrasts, function(pair) {
  bind_rows(lapply(OUTCOMES, function(outcome) {
    pairwise_test(pair[1], pair[2], outcome)
  }))
}))

# Holm correction within Item 10 family (18 tests)
results_tests$p_adj_holm <- p.adjust(results_tests$p_raw, method = "holm")
results_tests$sig_holm   <- results_tests$p_adj_holm < 0.05

print(results_tests %>%
        select(scenario_a, scenario_b, label, mean_a, mean_b, diff,
               cohens_d, d_magnitude, p_adj_holm, sig_holm))

write.csv(results_tests, file.path(TAB_DIR, "table_centrality_buddy.csv"),
          row.names = FALSE)

# --- Visualization: 6-panel 3-way comparison ---------------------------------
cat("\n=== Generating figure ===\n")

plot_data <- bind_rows(lapply(SCENARIOS, function(s) {
  d <- data_list[[s]]
  d %>% mutate(scenario = s) %>% select(scenario, all_of(OUTCOMES))
})) %>%
  pivot_longer(cols = all_of(OUTCOMES), names_to = "outcome", values_to = "value") %>%
  mutate(
    outcome_label = factor(OUTCOME_LABELS[outcome], levels = OUTCOME_LABELS),
    scenario      = factor(scenario, levels = SCENARIOS)
  )

summary_for_plot <- plot_data %>%
  group_by(scenario, outcome_label) %>%
  summarise(
    mean    = mean(value, na.rm = TRUE),
    se      = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    ci_low  = mean - 1.96 * se,
    ci_high = mean + 1.96 * se,
    .groups = "drop"
  )

p <- ggplot(summary_for_plot, aes(x = scenario, y = mean,
                                   color = scenario, fill = scenario)) +
  geom_col(width = 0.6, alpha = 0.6) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.18, linewidth = 0.4) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 2) +
  scale_color_viridis_d(end = 0.85) +
  scale_fill_viridis_d(end = 0.85) +
  labs(
    x = NULL, y = NULL, color = NULL, fill = NULL,
    title = "Four-way buddy-design comparison",
    subtitle = sprintf("BuddyProgram (week-0 distance), RandomBuddy (week-8 random), CentralityBuddy (week-8 degree). All buddy variants: 8-week +15%% attendance boost. n=300 each. Error bars: 95%% CI.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    strip.text    = element_text(face = "bold", size = 10),
    axis.text.x   = element_text(angle = 25, hjust = 1, size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIG_DIR, "fig_centrality_buddy.png"), p,
       width = 10, height = 9, dpi = 300, bg = "white")
cat(sprintf("Figure saved: %s\n", file.path(FIG_DIR, "fig_centrality_buddy.png")))

# --- Save full results -------------------------------------------------------
saveRDS(list(
  results_tests = results_tests,
  data_list     = data_list,
  metadata      = list(date = Sys.time(), scenarios = SCENARIOS)
), file.path(OUT_DIR, "centrality_buddy.rds"))

cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_centrality_buddy.csv")))
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "centrality_buddy.rds")))
cat("\n=== Item 10 CentralityBuddy 3-way comparison complete ===\n")
