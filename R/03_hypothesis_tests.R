# 03_hypothesis_tests.R — Welch t-tests, Holm correction, Cohen's d, run-level OLS
library(tidyverse)
library(effsize)
options(na.print = "NA")

DATA_DIR <- "data"

results_df <- readRDS(file.path(DATA_DIR, "results_df.rds"))

BASELINE      <- "Baseline"

# T1.18 (plan v2) + Phase 3 (D2): partition ALT_SCENARIOS into confirmatory,
# exploratory, and robustness families. Holm correction is applied separately
# within each family to preserve family-wise error rate at alpha = 0.05 within
# each interpretive category. See thesis.Rmd Methods Box "Pre-registration and
# multiple-comparisons structure" for justification.
CONFIRMATORY <- c(
  "Weak Peer Influence",        # H1
  "Suboptimal Composition",     # H2
  "No Indoor Continuity",       # H3
  "Minimal Support"             # H4
)
EXPLORATORY <- c(
  "Low Park Density", "High SES Heterogeneity", "Women-Only Groups",
  "NoIndoor Minimal", "Targeting50", "Targeting70", "Targeting90",
  "BuddyProgram", "RotatingGroups", "Winter50", "WomenChildcare"
)
# Phase 3 robustness extensions: scenarios that test ROBUSTNESS of base findings
# (dose-response of H2; open-cohort robustness; targeted-vs-random buddy design;
# verification-round controls SuboptimalOpen and RandomBuddy).
ROBUSTNESS <- c(
  "Composition2",     # Item 13 dose-2
  "Composition3",     # Item 13 dose-3
  "Composition4",     # Item 13 dose-4
  "OpenPopulation",   # Item 12 open-cohort
  "SuboptimalOpen",   # Item 12 ranking-preservation control
  "CentralityBuddy",  # Item 10 targeted matching
  "RandomBuddy"       # Item 10 timing-vs-criterion control
)
ALT_SCENARIOS <- c(CONFIRMATORY, EXPLORATORY, ROBUSTNESS)
stopifnot(length(CONFIRMATORY) == 4, length(EXPLORATORY) == 11,
          length(ROBUSTNESS) == 7)
OUTCOMES <- c("retention_rate", "avg_motivation", "avg_language_cefr",
              "cross_group_tie_ratio", "female_dropout_rate", "male_dropout_rate")

baseline_data <- results_df %>% filter(scenario == BASELINE)
n_comparisons <- length(ALT_SCENARIOS) * length(OUTCOMES)

# ---- Raw Welch t-tests ----
raw_tests <- map_dfr(ALT_SCENARIOS, function(scen) {
  alt_data <- results_df %>% filter(scenario == scen)
  map_dfr(OUTCOMES, function(outcome) {
    x_base <- na.omit(baseline_data[[outcome]])
    x_alt  <- na.omit(alt_data[[outcome]])
    if (length(x_base) < 2 || length(x_alt) < 2) return(NULL)
    t_res <- tryCatch(t.test(x_alt, x_base, var.equal = FALSE), error = function(e) NULL)
    d_res <- tryCatch(cohen.d(x_alt, x_base), error = function(e) NULL)
    if (is.null(t_res)) return(NULL)
    tibble(
      scenario    = scen,
      outcome     = outcome,
      mean_base   = mean(x_base),
      mean_alt    = mean(x_alt),
      diff        = mean(x_alt) - mean(x_base),
      diff_pct    = (mean(x_alt) - mean(x_base)) / abs(mean(x_base)) * 100,
      ci_low      = t_res$conf.int[1],
      ci_high     = t_res$conf.int[2],
      t_stat      = t_res$statistic,
      df          = t_res$parameter,
      p_raw       = t_res$p.value,
      cohens_d    = if (!is.null(d_res)) d_res$estimate else NA_real_,
      d_magnitude = if (!is.null(d_res)) as.character(d_res$magnitude) else NA_character_
    )
  })
})

# ---- Holm correction split by family (T1.18, plan v2 + Phase 3 D2) ----
# Confirmatory family: 4 scenarios x 6 outcomes = 24 comparisons
# Exploratory family:  11 scenarios x 6 outcomes = 66 comparisons
# Robustness family:    7 scenarios x 6 outcomes = 42 comparisons (Phase 3 + verification round)
# Each family receives its own p.adjust(method = "holm"); family-wise error rate
# is preserved at alpha = 0.05 within each interpretive category.
raw_tests <- raw_tests %>%
  mutate(family = case_when(
    scenario %in% CONFIRMATORY ~ "confirmatory",
    scenario %in% EXPLORATORY  ~ "exploratory",
    scenario %in% ROBUSTNESS   ~ "robustness",
    TRUE                       ~ NA_character_
  ))
stopifnot(all(!is.na(raw_tests$family)))
n_conf <- sum(raw_tests$family == "confirmatory")
n_expl <- sum(raw_tests$family == "exploratory")
n_robu <- sum(raw_tests$family == "robustness")
stopifnot(n_conf == length(CONFIRMATORY) * length(OUTCOMES))
stopifnot(n_expl == length(EXPLORATORY)  * length(OUTCOMES))
stopifnot(n_robu == length(ROBUSTNESS)   * length(OUTCOMES))

results_tests <- raw_tests %>%
  group_by(family) %>%
  mutate(
    p_adj_holm  = p.adjust(p_raw, method = "holm"),
    p_adj_bonf  = pmin(p_raw * n(), 1.0),
    sig_holm    = p_adj_holm < 0.05,
    sig_bonf    = p_adj_bonf < 0.05
  ) %>%
  ungroup()

alpha_holm_note <- sprintf("Holm correction applied to %d comparisons", n_comparisons)
cat("=== Hypothesis Test Results (Welch t-test, Holm corrected) ===\n")
cat(alpha_holm_note, "\n\n")

display <- results_tests %>%
  mutate(
    mean_diff   = sprintf("%+.2f (%+.1f%%)", diff, diff_pct),
    CI_95       = sprintf("[%+.2f, %+.2f]", ci_low, ci_high),
    p_display   = case_when(p_adj_holm < 0.001 ~ "<0.001",
                            p_adj_holm < 0.01  ~ "<0.01",
                            p_adj_holm < 0.05  ~ "<0.05",
                            TRUE               ~ sprintf("%.3f", p_adj_holm)),
    sig_mark    = ifelse(sig_holm, "***", "")
  ) %>%
  select(scenario, outcome, mean_base, mean_alt, mean_diff, CI_95,
         t_stat, p_display, sig_mark, cohens_d, d_magnitude)

print(as.data.frame(display), row.names = FALSE)

# ---- Run-level OLS: outcome ~ scenario (contrasts vs. Baseline) ----
cat("\n=== Run-Level OLS: Outcome ~ Scenario (contrast vs. Baseline) ===\n")
ols_results <- map_dfr(OUTCOMES, function(outcome) {
  m <- lm(as.formula(paste(outcome, "~ scenario")), data = results_df)
  s <- summary(m)
  coef_mat <- coef(s)
  tibble(
    outcome  = outcome,
    term     = rownames(coef_mat),
    estimate = coef_mat[, "Estimate"],
    se       = coef_mat[, "Std. Error"],
    t        = coef_mat[, "t value"],
    p_raw    = coef_mat[, "Pr(>|t|)"],
    r2       = s$r.squared
  )
}) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    scenario = gsub("^scenario", "", term),
    p_holm   = p.adjust(p_raw, method = "holm"),
    sig      = p_holm < 0.05
  ) %>%
  select(outcome, scenario, estimate, se, t, p_holm, sig, r2)

cat("Primary outcomes (Retention + Cross-tie), significant contrasts:\n")
sig_ols <- filter(ols_results, outcome %in% c("retention_rate", "cross_group_tie_ratio"), sig == TRUE)
print(as.data.frame(sig_ols), row.names = FALSE)

# ---- Save ----
write_csv(results_tests, "tables/table3_hypothesis_tests.csv")
write_csv(results_tests, file.path(DATA_DIR, "table3_hypothesis_tests.csv"))
saveRDS(results_tests, file.path(DATA_DIR, "hypothesis_tests.rds"))
saveRDS(ols_results,   file.path(DATA_DIR, "ols_results.rds"))
cat("\nSaved: table3_hypothesis_tests.csv, hypothesis_tests.rds, ols_results.rds\n")
