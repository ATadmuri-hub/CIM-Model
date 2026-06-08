# 11_agent_hazard.R — Discrete-time dropout hazard from real agent-week panel
# CIM v6.4 — Calisthenics Integration Model
#
# Uses CIM_panel_*.csv exports (real weekly motivation per agent)
# instead of synthetic linear interpolation from agent cross-section.
# Run from CIM_Model/ directory.

library(tidyverse)
options(na.print = "NA")
source("R/constants.R")

DATA_DIR <- "data"
TAB_DIR  <- "tables"
dir.create(TAB_DIR, showWarnings = FALSE)

# ── Panel loader: last-block logic (handles duplicated files) ─────────────────
load_panel_files <- function(scenario, max_runs = NULL) {
  files <- list.files(file.path(DATA_DIR, scenario),
                      pattern = "CIM_panel_.*[.]csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  if (!is.null(max_runs) && length(files) > max_runs) {
    set.seed(42)
    files <- sample(files, max_runs)
  }
  map_dfr(files, function(fp) {
    lines <- readLines(fp, warn = FALSE)
    header_rows <- which(lines == lines[1])
    block_start <- tail(header_rows, 1)
    block_lines <- lines[block_start:length(lines)]
    block_lines <- block_lines[nzchar(block_lines)]
    read_csv(I(paste(block_lines, collapse = "\n")), show_col_types = FALSE)
  })
}

# ── Feature engineering shared function ──────────────────────────────────────
engineer_features <- function(df) {
  df %>%
    mutate(
      week       = as.integer(as.numeric(week)),
      event_wk   = as.integer(event),
      # Winter = weeks 9-28 (verified from CIM_v6_3.nlogo: indoor-season-start=9, end=28)
      is_winter  = as.integer(week >= WINTER_ONSET_WEEK & week <= WINTER_END_WEEK),
      ses_hi     = as.integer(ses_level > 0.6),
      ses_lo     = as.integer(ses_level < 0.3),
      is_female  = as.integer(is_female),
      is_recent  = as.integer(arrival_cohort == "recent"),
      is_settled = as.integer(arrival_cohort == "settled"),
      prior_ex   = as.integer(prior_exercise)
    ) %>%
    filter(week <= 52) %>%
    # Risk set: keep only weeks up to and including the event week.
    # Panel exports one residual row after dropout; remove those post-event rows
    # to avoid risk set contamination (verified: 1 post-dropout row per dropped agent).
    group_by(run, agent_id) %>%
    arrange(week) %>%
    mutate(cum_event = cumsum(event_wk),
           drop_row  = cum_event > 1 | (cum_event == 1 & lag(cum_event, default = 0) == 1)) %>%
    filter(!drop_row) %>%
    select(-cum_event, -drop_row) %>%
    ungroup()
}

# ── 1. Baseline model ─────────────────────────────────────────────────────────
cat("Loading Baseline panel (100 runs)...\n")
panel_baseline <- load_panel_files("Baseline", max_runs = 100) %>%
  engineer_features()

cat(sprintf("Baseline panel: %s rows (%d agents × weeks)\n",
            format(nrow(panel_baseline), big.mark = ","),
            n_distinct(panel_baseline$agent_id)))

cat("\n=== Discrete-Time Hazard Model (Baseline, real motivation) ===\n")
cat("logit P(dropout_t) ~ week + is_winter + motivation +\n")
cat("                     is_female + ses_hi + ses_lo + prior_ex +\n")
cat("                     is_recent + is_settled\n\n")

dth_baseline <- glm(
  event_wk ~ week + is_winter + motivation +
    is_female + ses_hi + ses_lo + prior_ex + is_recent + is_settled,
  data   = panel_baseline,
  family = binomial(link = "logit")
)

s <- summary(dth_baseline)
coef_df <- as.data.frame(s$coefficients)
coef_df$term  <- rownames(coef_df)
coef_df$OR    <- exp(coef_df$Estimate)
coef_df$OR_lo <- exp(coef_df$Estimate - 1.96 * coef_df$`Std. Error`)
coef_df$OR_hi <- exp(coef_df$Estimate + 1.96 * coef_df$`Std. Error`)

display_coef <- coef_df %>%
  select(term, Estimate, `Std. Error`, `z value`, `Pr(>|z|)`, OR, OR_lo, OR_hi) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
cat("Coefficients + Odds Ratios:\n")
print(as.data.frame(display_coef), row.names = FALSE)

# ── 2. Mechanism sign checks ──────────────────────────────────────────────────
cat("\n=== Mechanism Invariant Checks ===\n")
check_sign <- function(term_name, expected_sign, label = term_name) {
  est <- coef_df$Estimate[coef_df$term == term_name]
  if (!length(est)) { cat(sprintf("  SKIP: %s not found\n", label)); return() }
  status <- ifelse(sign(est) == expected_sign, "PASS", "FAIL")
  dir    <- ifelse(expected_sign == 1, "positive", "negative")
  cat(sprintf("  [%s] %s: beta=%.4f (expected %s)\n", status, label, est, dir))
}

check_sign("motivation",  -1, "motivation → lower dropout")
check_sign("is_female",    1, "female → higher dropout")
check_sign("ses_lo",       1, "low SES → higher dropout")
check_sign("ses_hi",      -1, "high SES → lower dropout")
check_sign("prior_ex",    -1, "prior exercise → lower dropout")
check_sign("is_winter",    1, "winter → higher dropout")
check_sign("week",        -1, "week → declining baseline hazard")

# ── 3. Cross-scenario hazard (scenarios with panel files) ────────────────────
PANEL_SCENARIOS <- c(
  "Baseline", "No Indoor Continuity", "Minimal Support",
  "NoIndoor Minimal", "BuddyProgram", "RotatingGroups",
  "Targeting50", "Targeting70", "Targeting90",
  "Winter50", "WomenChildcare"
)

cat("\nLoading cross-scenario panel (30 runs each)...\n")
panel_all <- map_dfr(PANEL_SCENARIOS, function(sc) {
  df <- load_panel_files(sc, max_runs = 30)
  if (is.null(df)) return(NULL)
  cat(sprintf("  %s: %s rows\n", sc, format(nrow(df), big.mark=",")))
  df
}) %>%
  engineer_features() %>%
  mutate(scenario = factor(scenario, levels = PANEL_SCENARIOS))

cat(sprintf("\nCross-scenario panel: %s rows, %d scenarios\n",
            format(nrow(panel_all), big.mark = ","),
            n_distinct(panel_all$scenario)))

cat("\n=== Cross-Scenario Discrete-Time Hazard ===\n")
dth_all <- glm(
  event_wk ~ week + is_winter + motivation +
    is_female + ses_hi + ses_lo + prior_ex + scenario,
  data   = panel_all,
  family = binomial(link = "logit")
)

s_all   <- summary(dth_all)
coef_all <- as.data.frame(s_all$coefficients)
coef_all$term <- rownames(coef_all)

scen_coefs <- coef_all %>%
  filter(grepl("^scenario", term)) %>%
  mutate(
    scenario = gsub("^scenario", "", term),
    OR       = exp(Estimate),
    OR_lo    = exp(Estimate - 1.96 * `Std. Error`),
    OR_hi    = exp(Estimate + 1.96 * `Std. Error`)
  ) %>%
  arrange(Estimate) %>%
  select(scenario, Estimate, `Std. Error`, `Pr(>|z|)`, OR, OR_lo, OR_hi) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("Scenario dropout hazard ratios (vs. Baseline):\n")
print(as.data.frame(scen_coefs), row.names = FALSE)

# ── 4. Dropout reason breakdown (from agents_df) ─────────────────────────────
agents_df <- readRDS(file.path(DATA_DIR, "agents_df.rds"))

cat("\n=== Dropout Reasons by Scenario ===\n")
reason_tab <- agents_df %>%
  filter(dropped_out == TRUE) %>%
  count(scenario, dropout_reason) %>%
  group_by(scenario) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

reason_wide <- reason_tab %>%
  select(scenario, dropout_reason, pct) %>%
  pivot_wider(names_from = dropout_reason, values_from = pct, values_fill = 0)

print(as.data.frame(reason_wide), row.names = FALSE)

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(panel_baseline, file.path(DATA_DIR, "panel_baseline.rds"))
saveRDS(panel_all,      file.path(DATA_DIR, "panel_all.rds"))
saveRDS(dth_baseline,   file.path(DATA_DIR, "dth_model_baseline.rds"))
saveRDS(dth_all,        file.path(DATA_DIR, "dth_model_all.rds"))
saveRDS(scen_coefs,     file.path(DATA_DIR, "dth_scenario_coefs.rds"))
write_csv(scen_coefs,   file.path(TAB_DIR,  "table_dth_scenario.csv"))

cat("\nSaved: panel_baseline.rds, panel_all.rds, dth models, table_dth_scenario.csv\n")
cat("Discrete-time hazard analysis complete (real panel data).\n")
