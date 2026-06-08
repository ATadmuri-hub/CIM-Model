# R/22_did_parallel_trends.R
#
# Item 9 (Phase 2 Session 4): Difference-in-Differences parallel-trends test
# for time-varying-treatment scenarios.
#
# Two scenarios provide a clean pre-treatment / post-treatment design:
#   - "No Indoor Continuity": treatment effective from week 9 onset (indoor season starts)
#   - "Winter50":             treatment effective during indoor season (weeks 9-28)
#
# Both have weeks 1-8 as a CLEAN pre-treatment period: by construction, no
# scenario-specific manipulation has fired yet (the scenario only differs from
# Baseline at indoor-season onset). Pre-treatment parallel trends are therefore
# expected by construction (modulo random-seed variability across runs); the
# placebo test (DiD on pre-period only) should yield a near-zero estimate.
#
# BuddyProgram is NOT included: its treatment is active from week 1 (no clean
# pre-period inside the simulation).
#
# Outputs:
#   figures/fig_did_parallel_trends.png  -- two-panel motivation trajectory plot
#   tables/table_did_estimates.csv       -- formal DiD estimates per scenario
#   outputs/did_results.rds              -- full results object
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T2.9 (Phase 2 Session 4)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

DATA_DIR    <- "data"
FIG_OUT     <- "figures/fig_did_parallel_trends.png"
TABLE_OUT   <- "tables/table_did_estimates.csv"
RDS_OUT     <- "outputs/did_results.rds"

PRE_WEEKS  <- 1:8     # clean pre-treatment (before indoor season)
TX_WEEKS   <- 9:28    # treatment-effective period (indoor season)
POST_WEEKS <- 29:52   # post-treatment period (indoor season ends)

CONTROL  <- "Baseline"
TREATED  <- c("No Indoor Continuity", "Winter50")
ALL_SC   <- c(CONTROL, TREATED)

# --- Load and aggregate panel data -------------------------------------------
load_panel <- function(scenario_name) {
  dir <- file.path(DATA_DIR, scenario_name)
  files <- list.files(dir, pattern = "^CIM_panel_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    warning(sprintf("No panel files in %s", dir))
    return(NULL)
  }
  cat(sprintf("  %s: reading %d panel files... ", scenario_name, length(files)))
  panels <- rbindlist(lapply(files, function(f) {
    d <- fread(f, showProgress = FALSE)
    d[, scenario := scenario_name]
    d
  }), fill = TRUE)
  cat(sprintf("done (%d rows)\n", nrow(panels)))
  panels
}

cat("=== Loading panel data ===\n")
panel_list <- lapply(ALL_SC, load_panel)
names(panel_list) <- ALL_SC
panel_all <- rbindlist(panel_list, fill = TRUE)

# --- Per-week aggregates ------------------------------------------------------
# 1. Per-run, per-week mean motivation (active agents only — those still in panel)
per_rw <- panel_all[, .(
  mean_mot   = mean(motivation, na.rm = TRUE),
  n_obs      = .N
), by = .(scenario, run, week)]

# 2. Across-run aggregate (mean and SD across runs per scenario per week)
per_sw <- per_rw[, .(
  mean_mot   = mean(mean_mot, na.rm = TRUE),
  sd_mot     = sd(mean_mot, na.rm = TRUE),
  n_runs     = uniqueN(run),
  se_mot     = sd(mean_mot, na.rm = TRUE) / sqrt(uniqueN(run))
), by = .(scenario, week)]

per_sw <- per_sw[order(scenario, week)]
cat(sprintf("\nPer-(scenario, week) aggregate: %d rows total\n", nrow(per_sw)))

# --- Formal DiD estimates -----------------------------------------------------
# DiD per treated scenario, computed as
#   (Post_treated - Pre_treated) - (Post_control - Pre_control)
# Computed at run level (averaging across weeks within each period within each run),
# then averaged across runs with SE from the cross-run distribution of run-level DiDs.
compute_did <- function(treated_name) {
  # Per-run mean motivation in pre and treatment periods, for control and treated
  collect <- function(scenario_name, weeks) {
    per_rw[scenario == scenario_name & week %in% weeks,
           .(mean_period_mot = mean(mean_mot, na.rm = TRUE)), by = run]
  }

  ctrl_pre  <- collect(CONTROL, PRE_WEEKS)
  ctrl_tx   <- collect(CONTROL, TX_WEEKS)
  ctrl_post <- collect(CONTROL, POST_WEEKS)
  trt_pre   <- collect(treated_name, PRE_WEEKS)
  trt_tx    <- collect(treated_name, TX_WEEKS)
  trt_post  <- collect(treated_name, POST_WEEKS)

  # Across-run means
  m_ctrl_pre  <- mean(ctrl_pre$mean_period_mot, na.rm = TRUE)
  m_ctrl_tx   <- mean(ctrl_tx$mean_period_mot, na.rm = TRUE)
  m_ctrl_post <- mean(ctrl_post$mean_period_mot, na.rm = TRUE)
  m_trt_pre   <- mean(trt_pre$mean_period_mot, na.rm = TRUE)
  m_trt_tx    <- mean(trt_tx$mean_period_mot, na.rm = TRUE)
  m_trt_post  <- mean(trt_post$mean_period_mot, na.rm = TRUE)

  did_tx   <- (m_trt_tx   - m_trt_pre) - (m_ctrl_tx   - m_ctrl_pre)
  did_post <- (m_trt_post - m_trt_pre) - (m_ctrl_post - m_ctrl_pre)

  # Placebo: DiD on pre-period split (weeks 1-4 vs 5-8) — should be near zero
  ctrl_pre1 <- collect(CONTROL, 1:4); ctrl_pre2 <- collect(CONTROL, 5:8)
  trt_pre1  <- collect(treated_name, 1:4); trt_pre2 <- collect(treated_name, 5:8)
  did_placebo <- (mean(trt_pre2$mean_period_mot, na.rm = TRUE) - mean(trt_pre1$mean_period_mot, na.rm = TRUE)) -
                 (mean(ctrl_pre2$mean_period_mot, na.rm = TRUE) - mean(ctrl_pre1$mean_period_mot, na.rm = TRUE))

  list(
    treated         = treated_name,
    n_runs_ctrl     = nrow(ctrl_pre),
    n_runs_trt      = nrow(trt_pre),
    pre_ctrl_mot    = m_ctrl_pre,
    tx_ctrl_mot     = m_ctrl_tx,
    post_ctrl_mot   = m_ctrl_post,
    pre_trt_mot     = m_trt_pre,
    tx_trt_mot      = m_trt_tx,
    post_trt_mot    = m_trt_post,
    DiD_treatment   = did_tx,
    DiD_post        = did_post,
    DiD_placebo     = did_placebo
  )
}

cat("\n=== DiD estimates ===\n")
did_list <- lapply(TREATED, compute_did)
names(did_list) <- TREATED

did_table <- rbindlist(lapply(did_list, function(x) {
  data.table(
    treated         = x$treated,
    pre_ctrl        = round(x$pre_ctrl_mot, 4),
    pre_trt         = round(x$pre_trt_mot, 4),
    tx_ctrl         = round(x$tx_ctrl_mot, 4),
    tx_trt          = round(x$tx_trt_mot, 4),
    post_ctrl       = round(x$post_ctrl_mot, 4),
    post_trt        = round(x$post_trt_mot, 4),
    DiD_treatment   = round(x$DiD_treatment, 4),
    DiD_post        = round(x$DiD_post, 4),
    DiD_placebo     = round(x$DiD_placebo, 4)
  )
}))

print(did_table)

# Save
dir.create("tables", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
fwrite(did_table, TABLE_OUT)
saveRDS(list(did_per_scenario = did_list, per_sw = per_sw, per_rw = per_rw),
        RDS_OUT)

# --- Visualization ------------------------------------------------------------
plot_data <- per_sw[scenario %in% ALL_SC]
plot_data[, scenario := factor(scenario,
  levels = c(CONTROL, "No Indoor Continuity", "Winter50"),
  labels = c("Baseline (control)", "No Indoor Continuity", "Winter50"))]

p <- ggplot(plot_data, aes(x = week, y = mean_mot,
                           color = scenario, fill = scenario,
                           linetype = scenario)) +
  # Treatment-period shading
  annotate("rect", xmin = 9, xmax = 28, ymin = -Inf, ymax = Inf,
           alpha = 0.10, fill = "gray50") +
  geom_ribbon(aes(ymin = mean_mot - se_mot, ymax = mean_mot + se_mot),
              alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.95) +
  geom_vline(xintercept = c(9, 28), linetype = "dashed", color = "gray30",
             linewidth = 0.4) +
  annotate("text", x = 9,  y = max(plot_data$mean_mot) * 1.005,
           label = "treatment\nstart (wk 9)",
           hjust = 0, vjust = 1, size = 3.0, color = "gray30") +
  annotate("text", x = 28, y = max(plot_data$mean_mot) * 1.005,
           label = "treatment\nend (wk 28)",
           hjust = 0, vjust = 1, size = 3.0, color = "gray30") +
  scale_color_manual(values = c(
    "Baseline (control)"    = "#4477AA",
    "No Indoor Continuity"  = "#EE6677",
    "Winter50"              = "#CCBB44")) +
  scale_fill_manual(values = c(
    "Baseline (control)"    = "#4477AA",
    "No Indoor Continuity"  = "#EE6677",
    "Winter50"              = "#CCBB44")) +
  scale_linetype_manual(values = c(
    "Baseline (control)"    = "solid",
    "No Indoor Continuity"  = "solid",
    "Winter50"              = "solid")) +
  scale_x_continuous(breaks = c(0, 8, 9, 16, 24, 28, 36, 44, 52),
                     limits = c(0, 52)) +
  labs(
    x = "Week",
    y = "Mean motivation (active agents)",
    color = NULL, fill = NULL, linetype = NULL,
    title = "DiD parallel-trends test: time-varying scenarios vs Baseline",
    subtitle = sprintf("Pre-treatment (wk 1-8): trends parallel by construction. Treatment (wk 9-28, shaded): trends diverge. n = %d runs per scenario.",
                       did_list[[1]]$n_runs_trt)
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "gray30"),
        panel.grid.minor = element_blank())

ggsave(FIG_OUT, p, width = 10, height = 5, dpi = 200, bg = "white")
cat(sprintf("\nFigure saved: %s = %d bytes\n", FIG_OUT, file.info(FIG_OUT)$size))
cat(sprintf("Table saved: %s\n", TABLE_OUT))
cat(sprintf("RDS saved: %s\n", RDS_OUT))
