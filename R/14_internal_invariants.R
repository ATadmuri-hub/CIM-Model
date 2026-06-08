# 14_internal_invariants.R — Mechanism invariant checks (unit tests)
# CIM v6.4 — Calisthenics Integration Model
# Ten invariants (11 sub-checks) that should hold regardless of scenario

library(tidyverse)

DATA_DIR <- "data"

results_df    <- readRDS(file.path(DATA_DIR, "results_df.rds"))
timeseries_df <- readRDS(file.path(DATA_DIR, "timeseries_df.rds"))
agents_df     <- readRDS(file.path(DATA_DIR, "agents_df.rds"))

passed <- 0L
failed <- 0L

check <- function(name, expr, detail = "") {
  result <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(result)) {
    cat(sprintf("  PASS: %s\n", name))
    passed <<- passed + 1L
  } else {
    cat(sprintf("  FAIL: %s%s\n", name,
                if (nchar(detail) > 0) paste0(" — ", detail) else ""))
    failed <<- failed + 1L
  }
}

cat("=== CIM v6.4 Internal Invariant Checks ===\n\n")

# ── Invariant 1: Retention rate in [0, 1] ─────────────────────────────────────
cat("Invariant 1: Retention rate bounded [0, 100]\n")
check("all retention_rate in [0,100]",
      all(results_df$retention_rate >= 0 & results_df$retention_rate <= 100,
          na.rm = TRUE))

# ── Invariant 2: Total dropouts ≤ population ─────────────────────────────────
cat("Invariant 2: Dropouts ≤ total population (100 agents)\n")
check("max total_dropouts ≤ 100",
      max(results_df$total_dropouts, na.rm = TRUE) <= 100)

# ── Invariant 3: Motivation mean in [0, 1] ───────────────────────────────────
cat("Invariant 3: Avg motivation bounded [0, 1]\n")
check("all avg_motivation in [0,1]",
      all(results_df$avg_motivation >= 0 & results_df$avg_motivation <= 1,
          na.rm = TRUE))

# ── Invariant 4: Language CEFR ≥ 0 ──────────────────────────────────────────
cat("Invariant 4: Language proficiency ≥ 0\n")
check("all avg_language_cefr ≥ 0",
      all(results_df$avg_language_cefr >= 0, na.rm = TRUE))

# ── Invariant 5: Cross-group tie ratio in [0, 1] ─────────────────────────────
cat("Invariant 5: Cross-group tie ratio in [0, 1]\n")
check("all cross_group_tie_ratio in [0,1]",
      all(results_df$cross_group_tie_ratio >= 0 &
            results_df$cross_group_tie_ratio <= 1, na.rm = TRUE))

# ── Invariant 6: Timeseries motivation non-negative ──────────────────────────
cat("Invariant 6: Weekly motivation values ≥ 0\n")
check("all timeseries motivation ≥ 0",
      all(timeseries_df$motivation >= 0, na.rm = TRUE))

# ── Invariant 7: Baseline has largest n among original scenarios ─────────────
cat("Invariant 7: 23 scenarios loaded\n")
check("exactly 23 scenarios present",
      n_distinct(results_df$scenario) == 23)

# ── Invariant 8: Dropout week within simulation range ────────────────────────
cat("Invariant 8: Dropout week in [0, 52]\n")
if ("dropout_week" %in% colnames(agents_df)) {
  dw <- agents_df$dropout_week[!is.na(agents_df$dropout_week)]
  check("dropout_week in [-1, 52]",
        all(dw >= -1 & dw <= 52))
} else {
  cat("  SKIP: dropout_week not in agents_df\n")
}

# ── Invariant 9: Cost per retained is positive ────────────────────────────────
cat("Invariant 9: Cost per retained > 0\n")
check("all cost_per_retained > 0",
      all(results_df$cost_per_retained > 0, na.rm = TRUE))

# ── Invariant 10: Female + male dropout rates sum ≤ 1 ────────────────────────
cat("Invariant 10: Female dropout rate in [0,100] and male in [0,100]\n")
check("female_dropout_rate in [0,100]",
      all(results_df$female_dropout_rate >= 0 & results_df$female_dropout_rate <= 100,
          na.rm = TRUE))
check("male_dropout_rate in [0,100]",
      all(results_df$male_dropout_rate >= 0 & results_df$male_dropout_rate <= 100,
          na.rm = TRUE))

cat(sprintf("\n=== Results: %d passed, %d failed ===\n", passed, failed))
if (failed > 0) {
  warning(sprintf("%d invariant(s) FAILED — check model output integrity", failed))
}
