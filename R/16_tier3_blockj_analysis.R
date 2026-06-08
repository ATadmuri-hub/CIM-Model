# CIM v6.5 — Tier 3 Block J analysis
# Ranking-stability analysis across:
#   T3.A Open Population (closed vs open) for BuddyProgram / Baseline / Suboptimal Composition
#   T3.B gamma Bracket (0.011 / 0.018 / 0.025) for same three scenarios
#   T3.C v6.5 Sensitivity rerun PRCC comparison against v6.2 PRCC (=-0.96)
#
# Required data (run BehaviorSpace first):
#   data/OpenPopulation/     (from OpenPopulation_300runs experiment)
#   data/GammaBracket_Low/   (from GammaBracket_Low_300runs experiment)
#   data/GammaBracket_High/  (from GammaBracket_High_300runs experiment)
#   data/Sensitivity_v65/    (from Sensitivity_3level experiment on v6.5)
#
# Baseline data already in data/Baseline/, etc. from the main v6.4 run.

library(tidyverse)
library(effsize)

DATA_DIR <- "data"
OUT_DIR  <- "outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, showWarnings = FALSE)

# ============================================================================
# T3.A Open Population — ranking stability
# ============================================================================
cat("\n=== T3.A Open Population ===\n")

tryCatch({
  results_df <- readRDS(file.path(DATA_DIR, "results_df.rds"))
  # If OpenPopulation rows were appended to results_df by 01_load_data.R, they are here.
  # Otherwise load manually from data/OpenPopulation/ directory.
  if (!"OpenPopulation" %in% unique(results_df$scenario)) {
    op_files <- list.files(file.path(DATA_DIR, "OpenPopulation"),
                           pattern = "CIM_results_.*\\.csv", full.names = TRUE)
    if (length(op_files) > 0) {
      op_data <- map_dfr(op_files, ~ {
        d <- read_csv(.x, show_col_types = FALSE)
        tibble(
          scenario = "OpenPopulation",
          retention_rate = as.numeric(d$value[d$metric == "retention_rate"]),
          avg_motivation = as.numeric(d$value[d$metric == "avg_motivation"]),
          cross_group_tie_ratio = as.numeric(d$value[d$metric == "cross_group_tie_ratio"])
        )
      })
      results_df <- bind_rows(results_df, op_data)
    } else {
      stop("OpenPopulation data not found. Run BehaviorSpace OpenPopulation_300runs first.")
    }
  }

  # Compare rankings: BuddyProgram > Baseline > Suboptimal Composition
  ref_scenarios <- c("Baseline", "BuddyProgram", "Suboptimal Composition", "OpenPopulation")
  ranking_table <- results_df %>%
    filter(scenario %in% ref_scenarios) %>%
    group_by(scenario) %>%
    summarise(across(c(retention_rate, avg_motivation, cross_group_tie_ratio),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x, na.rm = TRUE))),
              .groups = "drop")
  print(ranking_table)
  saveRDS(ranking_table, file.path(OUT_DIR, "t3a_open_population_ranking.rds"))
  cat("\nInterpretation: if OpenPopulation retention falls between BuddyProgram (48.5)\n")
  cat("and Suboptimal (27.9), and closer to Baseline (45.1), the ranking is stable.\n")
}, error = function(e) {
  cat("SKIP (T3.A): ", conditionMessage(e), "\n", sep = "")
})

# ============================================================================
# T3.B gamma Bracket — ranking stability across gamma in {0.011, 0.018, 0.025}
# ============================================================================
cat("\n=== T3.B gamma Bracket ===\n")

tryCatch({
  load_bracket <- function(dir, gamma_val) {
    files <- list.files(file.path(DATA_DIR, dir),
                        pattern = "CIM_results_.*\\.csv", full.names = TRUE)
    if (length(files) == 0) stop(paste0("No files in ", dir))
    map_dfr(files, ~ {
      d <- read_csv(.x, show_col_types = FALSE)
      scen <- d$value[d$metric == "scenario"]
      tibble(
        gamma = gamma_val,
        scenario = scen,
        retention_rate = as.numeric(d$value[d$metric == "retention_rate"]),
        avg_motivation = as.numeric(d$value[d$metric == "avg_motivation"])
      )
    })
  }
  low  <- load_bracket("GammaBracket_Low", 0.011)
  mid  <- readRDS(file.path(DATA_DIR, "results_df.rds")) %>%
          filter(scenario %in% c("Baseline", "BuddyProgram", "Suboptimal Composition")) %>%
          select(scenario, retention_rate, avg_motivation) %>%
          mutate(gamma = 0.018)
  high <- load_bracket("GammaBracket_High", 0.025)
  bracket <- bind_rows(low, mid, high)

  bracket_summary <- bracket %>%
    group_by(gamma, scenario) %>%
    summarise(retention_mean = mean(retention_rate, na.rm = TRUE),
              retention_sd   = sd(retention_rate, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(gamma, scenario)
  print(bracket_summary)
  saveRDS(bracket_summary, file.path(OUT_DIR, "t3b_gamma_bracket.rds"))

  # Test: does BuddyProgram > Baseline > Suboptimal hold at every gamma?
  rank_check <- bracket_summary %>%
    group_by(gamma) %>%
    summarise(
      buddy_beats_baseline = retention_mean[scenario == "BuddyProgram"] >
                             retention_mean[scenario == "Baseline"],
      baseline_beats_subopt = retention_mean[scenario == "Baseline"] >
                              retention_mean[scenario == "Suboptimal Composition"],
      .groups = "drop"
    )
  print(rank_check)
  cat("\nInterpretation: TRUE TRUE in all three rows => ranking stable across gamma bracket.\n")
}, error = function(e) {
  cat("SKIP (T3.B): ", conditionMessage(e), "\n", sep = "")
})

# ============================================================================
# T3.C v6.5 Sensitivity rerun — PRCC comparison
# ============================================================================
cat("\n=== T3.C Sensitivity v6.5 PRCC ===\n")

tryCatch({
  # Expected path: after rerun of Sensitivity_3level on v6.5, data goes to data/sensitivity_prcc_v65.csv
  v65_path <- file.path(DATA_DIR, "sensitivity_prcc_v65.csv")
  v62_path <- file.path(DATA_DIR, "sensitivity_prcc.csv")
  if (!file.exists(v65_path)) stop("v6.5 sensitivity PRCC not yet computed")
  v65 <- read_csv(v65_path, show_col_types = FALSE)
  v62 <- read_csv(v62_path, show_col_types = FALSE)
  merged <- v65 %>%
    rename_with(~ paste0(., "_v65"), -contains("parameter")) %>%
    left_join(v62 %>% rename_with(~ paste0(., "_v62"), -contains("parameter")),
              by = "parameter")
  merged$delta <- merged$prcc_v65 - merged$prcc_v62
  print(merged)
  max_abs_delta <- max(abs(merged$delta), na.rm = TRUE)
  cat(sprintf("\nMax |delta| PRCC: %.4f\n", max_abs_delta))
  if (max_abs_delta > 0.02) {
    cat("WARNING: PRCC drift exceeds 0.02 threshold. Update thesis abstract.\n")
  } else {
    cat("PRCC stable within +/- 0.02. Existing abstract value remains valid.\n")
  }
  saveRDS(merged, file.path(OUT_DIR, "t3c_sensitivity_v62_vs_v65.rds"))
}, error = function(e) {
  cat("SKIP (T3.C): ", conditionMessage(e), "\n", sep = "")
})

cat("\n=== Tier 3 Block J analysis complete ===\n")
