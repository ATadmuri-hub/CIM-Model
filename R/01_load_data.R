# 01_load_data.R — Load and merge all scenario CSVs
# CIM v6.4 — Calisthenics Integration Model

library(tidyverse)

DATA_DIR <- "data"

SCENARIOS <- c(
  ## Original 8 scenarios
  "Baseline", "No Indoor Continuity", "Minimal Support",
  "Low Park Density", "Weak Peer Influence", "Suboptimal Composition",
  "High SES Heterogeneity", "Women-Only Groups",
  ## Additional scenarios
  "NoIndoor Minimal", "Targeting50", "Targeting70", "Targeting90",
  "BuddyProgram", "RotatingGroups", "Winter50", "WomenChildcare",
  ## Phase 3 robustness extensions (May 2026)
  "Composition2", "Composition3", "Composition4",   # Item 13: dose-response
  "OpenPopulation",                                  # Item 12: open-cohort robustness
  "SuboptimalOpen",                                  # Item 12 ranking-preservation control
  "CentralityBuddy",                                 # Item 10: targeted-pairing
  "RandomBuddy"                                      # Item 10 timing-vs-criterion control
)

# ── Required columns (verified from actual exported files) ───────────────────
REQUIRED_RESULTS_METRICS <- c(
  "retention_rate", "avg_motivation", "avg_language_cefr",
  "cross_group_tie_ratio", "female_dropout_rate", "male_dropout_rate",
  "total_dropouts", "cost_per_retained"
)
REQUIRED_TIMESERIES_COLS <- c("week", "participation", "motivation", "language",
                               "integration", "dropouts", "cost")
REQUIRED_AGENTS_COLS <- c(
  "run", "scenario", "breed", "gender", "ses", "arrival_cohort", "prior_exercise",
  "initial_motivation", "final_motivation", "weeks_attended", "dropped_out",
  "dropout_week", "dropout_reason", "language_gain", "cross_group_friends",
  "distance_to_park"
)
REQUIRED_PANEL_COLS <- c(
  "run", "scenario", "agent_id", "week", "motivation", "event",
  "is_female", "ses_level", "arrival_cohort", "prior_exercise", "distance"
)
REQUIRED_EDGE_COLS <- c(
  "run", "scenario", "end1_id", "end2_id", "end1_breed", "end2_breed",
  "tie_strength", "is_cross_group", "formed_week", "weeks_active"
)

validate_columns <- function(df, required, label, file = "") {
  missing <- setdiff(required, colnames(df))
  if (length(missing) > 0) {
    stop(sprintf("[VALIDATION FAIL] %s missing required columns: %s (in %s)",
                 label, paste(missing, collapse = ", "), file))
  }
}

# ── Manifest tracking ─────────────────────────────────────────────────────────
manifest_rows <- list()

# ---- Load final results CSVs ----
# Handles files with multiple appended blocks — uses last block only
load_results <- function(scenario) {
  d <- file.path(DATA_DIR, scenario)
  files <- list.files(d, pattern = "CIM_results_.*[.]csv$", full.names = TRUE)
  if (length(files) == 0) {
    warning(sprintf("[MANIFEST] No results files found for scenario: %s", scenario))
    return(NULL)
  }

  dfs <- map(files, function(fp) {
    lines <- readLines(fp, warn = FALSE)
    header_rows <- which(lines == "metric,value")
    if (length(header_rows) == 0) return(NULL)
    # Use LAST block only (handles duplicate-appended files from re-runs)
    block_start <- tail(header_rows, 1) + 1
    block_lines <- lines[block_start:length(lines)]
    block_lines <- block_lines[nzchar(block_lines)]
    pairs <- block_lines[grepl(",", block_lines)]
    if (length(pairs) == 0) return(NULL)
    keys <- sub(",.*$", "", pairs)
    vals <- sub("^[^,]+,", "", pairs)
    df <- data.frame(key = keys, value = vals, stringsAsFactors = FALSE)
    df_wide <- df %>% pivot_wider(names_from = key, values_from = value)
    df_wide$scenario <- scenario
    df_wide
  })
  bind_rows(dfs)
}

results_raw <- map_dfr(SCENARIOS, function(sc) {
  d <- file.path(DATA_DIR, sc)
  n_files <- length(list.files(d, pattern = "CIM_results_.*[.]csv$"))
  out <- load_results(sc)
  n_loaded <- if (is.null(out)) 0 else nrow(out)
  manifest_rows[[sc]] <<- list(
    scenario = sc,
    n_results_files = n_files,
    n_runs_loaded   = n_loaded
  )
  out
})

cat("Raw rows loaded:", nrow(results_raw), "\n")

# Validate required metrics appear in results
loaded_metrics <- colnames(results_raw)
missing_metrics <- setdiff(REQUIRED_RESULTS_METRICS, loaded_metrics)
if (length(missing_metrics) > 0) {
  stop(sprintf("[VALIDATION FAIL] results_df missing required metrics: %s",
               paste(missing_metrics, collapse = ", ")))
}

# ---- Clean and type-cast ----
int_cols <- intersect(c("run", "final_week", "total_dropouts",
                        "overall_success", "winter_paused_count"),
                      colnames(results_raw))
num_cols <- intersect(c("retention_rate", "avg_motivation", "avg_language_cefr",
                        "cross_group_tie_ratio", "cost_per_retained",
                        "female_dropout_rate", "male_dropout_rate",
                        "recent_cohort_lang_gain", "established_cohort_lang_gain",
                        "settled_cohort_lang_gain", "prior_exercise_retention",
                        "no_exercise_retention", "stable_participation_wk46_52",
                        "stable_motivation_wk46_52", "stable_language_wk46_52",
                        "stable_integration_wk46_52"),
                      colnames(results_raw))

results_df <- results_raw %>%
  mutate(across(all_of(int_cols), ~ as.integer(as.numeric(.x))),
         across(all_of(num_cols), ~ as.numeric(.x)),
         scenario = factor(scenario, levels = SCENARIOS))

cat("Results loaded:", nrow(results_df), "runs across",
    n_distinct(results_df$scenario), "scenarios\n")
print(count(results_df, scenario))

# ---- Load timeseries CSVs ----
load_timeseries <- function(scenario) {
  d <- file.path(DATA_DIR, scenario)
  files <- list.files(d, pattern = "CIM_timeseries_.*[.]csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  map_dfr(files, function(fp) {
    run_num <- as.integer(sub(".*_([0-9]+)[.]csv$", "\\1", basename(fp)))
    lines <- readLines(fp, warn = FALSE)
    header_rows <- which(lines == lines[1])
    block_start <- tail(header_rows, 1)
    block_lines <- lines[block_start:length(lines)]
    block_lines <- block_lines[nzchar(block_lines)]
    df <- read_csv(
      I(paste(block_lines, collapse = "\n")),
      show_col_types = FALSE,
      col_types = cols(
        week = col_integer(), participation = col_integer(),
        motivation = col_double(), language = col_double(),
        integration = col_double(), dropouts = col_integer(),
        cost = col_double(),
        .default = col_double()
      )
    )
    validate_columns(df, REQUIRED_TIMESERIES_COLS, "timeseries", fp)
    df$scenario <- scenario
    df$run <- run_num
    df
  })
}

timeseries_df <- map_dfr(SCENARIOS, load_timeseries)
timeseries_df <- timeseries_df %>%
  mutate(scenario = factor(scenario, levels = SCENARIOS))
cat("\nTimeseries loaded:", nrow(timeseries_df), "rows\n")

# ---- Load agent-level CSVs ----
load_agents <- function(scenario) {
  d <- file.path(DATA_DIR, scenario)
  files <- list.files(d, pattern = "CIM_agents_.*[.]csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  map_dfr(files, function(fp) {
    run_num <- as.integer(sub(".*_([0-9]+)[.]csv$", "\\1", basename(fp)))
    lines <- readLines(fp, warn = FALSE)
    header_rows <- which(lines == lines[1])
    block_start <- tail(header_rows, 1)
    block_lines <- lines[block_start:length(lines)]
    block_lines <- block_lines[nzchar(block_lines)]
    df <- read_csv(I(paste(block_lines, collapse = "\n")), show_col_types = FALSE)
    validate_columns(df, REQUIRED_AGENTS_COLS, "agents", fp)
    df$scenario <- scenario
    df$run <- run_num
    df
  })
}

agents_df <- map_dfr(SCENARIOS, load_agents)
agents_df <- agents_df %>%
  mutate(scenario = factor(scenario, levels = SCENARIOS))
cat("Agent data loaded:", nrow(agents_df), "agent-run records\n")

# ---- Generate manifest/experiment accounting ─────────────────────────────────
# Count panel and edge files per scenario
for (sc in SCENARIOS) {
  d <- file.path(DATA_DIR, sc)
  manifest_rows[[sc]]$n_panel_files <-
    length(list.files(d, pattern = "CIM_panel_.*[.]csv$"))
  manifest_rows[[sc]]$n_edge_files  <-
    length(list.files(d, pattern = "CIM_edges_.*[.]csv$"))
  manifest_rows[[sc]]$has_panel <- manifest_rows[[sc]]$n_panel_files > 0
  manifest_rows[[sc]]$has_edges <- manifest_rows[[sc]]$n_edge_files > 0
}

manifest_df <- bind_rows(manifest_rows) %>%
  mutate(
    scenario_type = case_when(
      scenario == "High SES Heterogeneity" ~ "Context",
      TRUE ~ "Lever"
    ),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

total_policy_runs <- sum(manifest_df$n_runs_loaded)
cat(sprintf("\n=== Experiment Accounting ===\n"))
cat(sprintf("Policy scenarios: %d\n", nrow(manifest_df)))
cat(sprintf("Total policy runs loaded: %d\n", total_policy_runs))
cat(sprintf("(Expected 7,500 after Phase 3 verification round: 16 original + 7 robustness + 3 topups)\n"))
if (total_policy_runs != 7500) {
  warning(sprintf("[MANIFEST] Loaded %d runs != expected 7,500. Reconcile before reporting.", total_policy_runs))
}
print(manifest_df %>% select(scenario, scenario_type, n_runs_loaded, n_panel_files, n_edge_files))

TAB_DIR <- "tables"
dir.create(TAB_DIR, showWarnings = FALSE)
write_csv(manifest_df,
          file.path(TAB_DIR, "table_experiment_accounting.csv"))
cat("Saved: tables/table_experiment_accounting.csv\n")

# ---- Save RDS ----
saveRDS(results_df,    file.path(DATA_DIR, "results_df.rds"))
saveRDS(timeseries_df, file.path(DATA_DIR, "timeseries_df.rds"))
saveRDS(agents_df,     file.path(DATA_DIR, "agents_df.rds"))
cat("\nSaved: results_df.rds, timeseries_df.rds, agents_df.rds\n")
