# R/23_randomization_inference.R
#
# Item 7 (Phase 2 Session 5): Randomization-inference robustness check on the
# four confirmatory hypotheses (H1-H4 x 6 outcomes = 24 tests).
#
# Method: distribution-free permutation test of the sharp null (tau_i = 0 for all
# units). For each (scenario, outcome) pair:
#   1. Pool the run-level outcomes from Baseline + treated scenario
#   2. Compute observed mean difference (alt - base)
#   3. Permute scenario labels 10,000 times preserving group sizes
#   4. RI p-value = fraction of permutations with |diff_perm| >= |diff_obs|
#
# After raw RI p-values are computed across all 24 tests, apply Holm correction
# within the confirmatory family (matching the methodology in R/03 line 18-25).
#
# This provides a SECOND, INDEPENDENT inference layer that does not depend on
# the normal-approximation underlying Welch's t-test. If RI and Welch+Holm
# agree, the confirmatory results are robust to the parametric assumption.
#
# Outputs:
#   tables/table_ri_robustness.csv  -- comparison table (RI vs Welch+Holm)
#   outputs/ri_h1_h4.rds             -- full results object incl. permutation distributions
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T2.7 (Phase 2 Session 5)

suppressPackageStartupMessages({
  library(tidyverse)
})

DATA_DIR <- "data"
N_PERM   <- 10000
SEED     <- 42

set.seed(SEED)

results_df <- readRDS(file.path(DATA_DIR, "results_df.rds"))

BASELINE     <- "Baseline"
CONFIRMATORY <- c("Weak Peer Influence", "Suboptimal Composition",
                  "No Indoor Continuity", "Minimal Support")
OUTCOMES <- c("retention_rate", "avg_motivation", "avg_language_cefr",
              "cross_group_tie_ratio", "female_dropout_rate", "male_dropout_rate")

baseline_data <- results_df %>% filter(scenario == BASELINE)

# --- Permutation test helper ---------------------------------------------------
permutation_test <- function(x_alt, x_base, n_perm = N_PERM) {
  x_alt  <- as.numeric(na.omit(x_alt))
  x_base <- as.numeric(na.omit(x_base))
  if (length(x_alt) < 2 || length(x_base) < 2) {
    return(list(diff_obs = NA, p_ri = NA, n_alt = length(x_alt), n_base = length(x_base)))
  }
  n_alt  <- length(x_alt)
  n_base <- length(x_base)
  pooled <- c(x_alt, x_base)
  N      <- length(pooled)
  diff_obs <- mean(x_alt) - mean(x_base)
  # Permutation: randomly assign n_alt units to "alt" group, rest to "base"
  # Vectorized via matrix approach for speed
  perm_diffs <- numeric(n_perm)
  for (i in seq_len(n_perm)) {
    idx_alt <- sample.int(N, n_alt, replace = FALSE)
    perm_diffs[i] <- mean(pooled[idx_alt]) - mean(pooled[-idx_alt])
  }
  # Two-sided p-value (add 1 to numerator and denominator for unbiasedness;
  # this is the standard "exact-test" correction; with N_PERM=10000 the
  # difference is negligible but the correction prevents reporting p=0)
  p_ri <- (sum(abs(perm_diffs) >= abs(diff_obs)) + 1) / (n_perm + 1)
  list(
    diff_obs   = diff_obs,
    p_ri       = p_ri,
    n_alt      = n_alt,
    n_base     = n_base,
    perm_diffs = perm_diffs
  )
}

# --- Run all 24 confirmatory tests ---------------------------------------------
cat(sprintf("Running %d permutation tests with %d permutations each (seed=%d) ...\n",
            length(CONFIRMATORY) * length(OUTCOMES), N_PERM, SEED))

all_results <- list()
for (scen in CONFIRMATORY) {
  alt_data <- results_df %>% filter(scenario == scen)
  for (outcome in OUTCOMES) {
    cat(sprintf("  %s | %s ... ", scen, outcome))
    res <- permutation_test(alt_data[[outcome]], baseline_data[[outcome]])
    cat(sprintf("diff=%+.4f, p_ri=%.4g (n_alt=%d, n_base=%d)\n",
                res$diff_obs, res$p_ri, res$n_alt, res$n_base))
    all_results[[paste(scen, outcome, sep = "|")]] <- c(
      list(scenario = scen, outcome = outcome), res)
  }
}

# --- Build comparison table vs existing Holm-corrected Welch p-values ---------
# Read existing Welch + Holm results from R/03 output.
# Use the raw .rds, not table3_hypothesis_tests.csv: R/07 reformats that csv for
# display and drops the `family` column, so the csv read here would break the filter.
welch_holm <- as.data.frame(readRDS(file.path(DATA_DIR, "hypothesis_tests.rds")))
welch_holm <- welch_holm %>%
  filter(family == "confirmatory") %>%
  select(scenario, outcome, p_raw, p_adj_holm, sig_holm)

ri_summary <- map_dfr(all_results, function(r) {
  tibble(
    scenario   = r$scenario,
    outcome    = r$outcome,
    diff_obs   = r$diff_obs,
    n_alt      = r$n_alt,
    n_base     = r$n_base,
    p_ri_raw   = r$p_ri
  )
})

# Apply Holm correction within the confirmatory family (24 tests)
ri_summary$p_ri_holm <- p.adjust(ri_summary$p_ri_raw, method = "holm")
ri_summary$sig_ri_holm <- ri_summary$p_ri_holm < 0.05

# Merge with Welch+Holm for side-by-side comparison
comparison <- ri_summary %>%
  left_join(welch_holm, by = c("scenario", "outcome")) %>%
  mutate(
    agree_sig = (sig_ri_holm == sig_holm),
    p_welch_raw = p_raw,
    p_welch_holm = p_adj_holm,
    sig_welch_holm = sig_holm
  ) %>%
  select(scenario, outcome, diff_obs, n_alt, n_base,
         p_welch_raw, p_welch_holm, sig_welch_holm,
         p_ri_raw, p_ri_holm, sig_ri_holm,
         agree_sig)

cat("\n=== RI VS WELCH+HOLM COMPARISON (24 confirmatory tests) ===\n")
print(comparison %>%
        mutate(across(starts_with("p_"), ~ formatC(.x, format = "g", digits = 3))))

cat(sprintf("\nAgreement on significance (after Holm): %d / %d (%.0f%%)\n",
            sum(comparison$agree_sig), nrow(comparison),
            100 * mean(comparison$agree_sig)))

# --- Save outputs --------------------------------------------------------------
dir.create("tables", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
write.csv(comparison, "tables/table_ri_robustness.csv", row.names = FALSE)
saveRDS(list(
  all_results = all_results,
  comparison  = comparison,
  metadata    = list(n_perm = N_PERM, seed = SEED, date = Sys.time())
), "outputs/ri_h1_h4.rds")

cat(sprintf("\nSaved: tables/table_ri_robustness.csv (%d rows)\n", nrow(comparison)))
cat(sprintf("Saved: outputs/ri_h1_h4.rds\n"))
