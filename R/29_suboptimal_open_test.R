# R/29_suboptimal_open_test.R
#
# Item 12 follow-up (Phase 3 verification round): Direct test of whether the
# Baseline-vs-Suboptimal ranking preserves under open-population dynamics.
#
# Compares 4 scenarios on retention + 5 other outcomes:
#   - Baseline                 (closed cohort, no manipulation)
#   - Suboptimal Composition   (closed cohort, 1 local/park)
#   - OpenPopulation           (open cohort, p_in=0.30, p_out=0.20)
#   - SuboptimalOpen           (open cohort + 1 local/park; NEW Phase 3)
#
# Two-by-two factorial design:
#                | Closed              | Open
#   ------------ | ------------------- | -------------------
#   5 locals/pk  | Baseline            | OpenPopulation
#   1 local/pk   | Suboptimal          | SuboptimalOpen
#
# Tests:
#   1. Pairwise Welch t-tests on each outcome
#   2. Cohen's d for each contrast
#   3. Holm correction within the 4×6 = 24-test family
#   4. Ranking preservation: is Baseline > Suboptimal in BOTH cohort regimes?
#
# Outputs:
#   tables/table_suboptimal_open.csv  -- 4-scenario summary + 6 contrasts
#   outputs/suboptimal_open.rds       -- full results object
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T3.12-followup-2 (Phase 3 verification round)

suppressPackageStartupMessages({
  library(dplyr)
  library(effsize)
})

DATA_DIR <- "data"
TAB_DIR  <- "tables"
OUT_DIR  <- "outputs"

OUTCOMES <- c("retention_rate", "avg_motivation", "avg_language_cefr",
              "cross_group_tie_ratio", "female_dropout_rate", "male_dropout_rate")

SCENARIOS <- c("Baseline", "Suboptimal Composition", "OpenPopulation", "SuboptimalOpen")

# --- Load data (last-block dedup) -------------------------------------------
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

cat("=== Loading 4 scenarios for 2x2 factorial test ===\n")
data_list <- lapply(SCENARIOS, function(s) {
  d <- load_scenario_results(s)
  cat(sprintf("  %s: %d runs\n", s, ifelse(is.null(d), 0, nrow(d))))
  d
})
names(data_list) <- SCENARIOS

# --- Per-scenario summary ----------------------------------------------------
cat("\n=== Per-scenario summary ===\n")
summary_df <- bind_rows(lapply(SCENARIOS, function(s) {
  d <- data_list[[s]]
  data.frame(
    scenario       = s,
    n              = nrow(d),
    mean_retention = mean(d$retention_rate, na.rm = TRUE),
    se_retention   = sd(d$retention_rate, na.rm = TRUE) / sqrt(nrow(d)),
    mean_cefr      = mean(d$avg_language_cefr, na.rm = TRUE),
    mean_cross_tie = mean(d$cross_group_tie_ratio, na.rm = TRUE),
    mean_motivation = mean(d$avg_motivation, na.rm = TRUE)
  )
}))
print(summary_df)

# --- Pairwise Welch t-tests + Cohen's d -------------------------------------
contrasts <- list(
  c("Suboptimal Composition", "Baseline"),
  c("OpenPopulation",         "Baseline"),
  c("SuboptimalOpen",         "Baseline"),
  c("OpenPopulation",         "Suboptimal Composition"),
  c("SuboptimalOpen",         "Suboptimal Composition"),
  c("SuboptimalOpen",         "OpenPopulation")
)

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
    n_a         = length(x_a),
    n_b         = length(x_b),
    mean_a      = mean(x_a),
    mean_b      = mean(x_b),
    diff        = mean(x_a) - mean(x_b),
    cohens_d    = d_res$estimate,
    d_magnitude = as.character(d_res$magnitude),
    t_stat      = t_res$statistic,
    p_raw       = t_res$p.value
  )
}

cat("\n=== 6 pairwise contrasts × 6 outcomes = 36 tests; Holm-corrected ===\n")
results_tests <- bind_rows(lapply(contrasts, function(pair) {
  bind_rows(lapply(OUTCOMES, function(outcome) {
    pairwise_test(pair[1], pair[2], outcome)
  }))
}))
results_tests$p_adj_holm <- p.adjust(results_tests$p_raw, method = "holm")
results_tests$sig_holm   <- results_tests$p_adj_holm < 0.05

# Show retention-only results (the headline test)
cat("\n--- Retention-only summary (the headline ranking-preservation test) ---\n")
print(results_tests %>%
        filter(outcome == "retention_rate") %>%
        select(scenario_a, scenario_b, mean_a, mean_b, diff, cohens_d,
               d_magnitude, p_adj_holm, sig_holm))

# Ranking preservation check
cat("\n--- Ranking-preservation check ---\n")
ret_means <- summary_df$mean_retention
names(ret_means) <- summary_df$scenario
cat(sprintf("  Closed: Baseline %.2f%%  >  Suboptimal %.2f%%  (gap: %+.2f pp)\n",
            ret_means["Baseline"], ret_means["Suboptimal Composition"],
            ret_means["Baseline"] - ret_means["Suboptimal Composition"]))
cat(sprintf("  Open:   OpenPop  %.2f%%  >  SuboptOpen %.2f%%  (gap: %+.2f pp)\n",
            ret_means["OpenPopulation"], ret_means["SuboptimalOpen"],
            ret_means["OpenPopulation"] - ret_means["SuboptimalOpen"]))
cat(sprintf("  Ranking preserved: %s\n",
            (ret_means["Baseline"] > ret_means["Suboptimal Composition"]) &&
            (ret_means["OpenPopulation"] > ret_means["SuboptimalOpen"])))

write.csv(results_tests, file.path(TAB_DIR, "table_suboptimal_open.csv"),
          row.names = FALSE)
saveRDS(list(
  summary_df    = summary_df,
  results_tests = results_tests,
  data_list     = data_list,
  metadata      = list(date = Sys.time(), scenarios = SCENARIOS)
), file.path(OUT_DIR, "suboptimal_open.rds"))

cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_suboptimal_open.csv")))
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "suboptimal_open.rds")))
cat("\n=== Item 12 ranking-preservation test complete ===\n")
