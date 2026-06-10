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
  library(patchwork)
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

# --- Visualization: 6-panel 4-way comparison (dot-and-whisker) ---------------
# These are means of bounded rates whose between-design differences sit far from
# zero, so a zero-baseline bar buries the signal and shrinks the 95% CIs to
# ticks. We use a dot-and-whisker (Cleveland 1985; Tufte 2018): points carry no
# zero-baseline obligation, so each panel auto-zooms to its own data window.
# Colour is reduced to a single contrast (grey = Baseline control, teal = buddy
# variant). For lay readers each point is value-labelled, every panel carries a
# plain-language reading cue (higher/lower is better, or no difference), and a
# footnote spells out dot/line/colour so the figure is readable on its own.
cat("\n=== Generating figure ===\n")

ACCENT  <- "#1f8a8a"   # buddy variants
CONTROL <- "grey50"    # Baseline reference
# Plain-language reading cue per panel (avoids the jargon "n.s.")
READING <- c(
  retention_rate = "higher is better", avg_motivation = "no difference vs Baseline",
  avg_language_cefr = "no difference vs Baseline", cross_group_tie_ratio = "higher is better",
  female_dropout_rate = "lower is better", male_dropout_rate = "lower is better")
# Decimal places for the on-point value label, by outcome
DIGITS <- c(retention_rate = 1, avg_motivation = 3, avg_language_cefr = 2,
            cross_group_tie_ratio = 2, female_dropout_rate = 1, male_dropout_rate = 1)
# Panel titles double as the x-axis labels; units are loaded in so the unitless
# metrics (motivation, CEFR, tie ratio) read clearly. Kept separate from
# OUTCOME_LABELS so the stats table's label column does not change.
PANEL_TITLES <- c(
  retention_rate        = "Retention rate (%)",
  avg_motivation        = "Mean motivation (0-1 scale)",
  avg_language_cefr     = "Language CEFR gain (CEFR units)",
  cross_group_tie_ratio = "Cross-group tie ratio (0-1)",
  female_dropout_rate   = "Female dropout rate (%)",
  male_dropout_rate     = "Male dropout rate (%)")

summary_for_plot <- bind_rows(lapply(SCENARIOS, function(s) {
  data_list[[s]] %>% mutate(scenario = s) %>% select(scenario, all_of(OUTCOMES))
})) %>%
  pivot_longer(cols = all_of(OUTCOMES), names_to = "outcome", values_to = "value") %>%
  group_by(scenario, outcome) %>%
  summarise(
    mean    = mean(value, na.rm = TRUE),
    se      = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    ci_low  = mean - 1.96 * se,
    ci_high = mean + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(scenario = factor(scenario, levels = rev(SCENARIOS)),  # Baseline on top
         grp      = ifelse(scenario == "Baseline", "Baseline", "Variant"),
         lab      = vapply(seq_along(mean),
                           function(i) formatC(mean[i], format = "f", digits = DIGITS[[outcome[i]]]),
                           character(1)))

baseline_ref <- summary_for_plot %>%
  filter(scenario == "Baseline") %>%
  select(outcome, baseline_mean = mean)

# One panel. Left column keeps the scenario labels; the right column drops them
# (patchwork aligns the rows, so each point still maps to its scenario). The
# value sits above each dot; the plain-language cue sits in the top-right corner.
# Font sizes are parameters so the same panel logic emits a denser thesis figure
# and a larger-text slide variant.
make_panel <- function(metric_key, show_y,
                       title_sz = 10, axis_sz = 8.5, ylab_sz = 9.5,
                       val_sz = 3, cue_sz = 2.9) {
  d <- dplyr::filter(summary_for_plot, outcome == metric_key)
  b <- baseline_ref$baseline_mean[baseline_ref$outcome == metric_key]
  ggplot(d, aes(x = mean, y = scenario, colour = grp)) +
    geom_vline(xintercept = b, linetype = "dashed", colour = "grey78", linewidth = 0.3) +
    geom_pointrange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.8, size = 0.5) +
    geom_text(aes(label = lab), colour = "grey20", size = val_sz, nudge_y = 0.34) +
    annotate("text", x = Inf, y = Inf, label = READING[metric_key],
             hjust = 1.04, vjust = 1.5, size = cue_sz, fontface = "italic", colour = "grey50") +
    scale_colour_manual(values = c(Baseline = CONTROL, Variant = ACCENT), guide = "none") +
    scale_x_continuous(n.breaks = 4, expand = expansion(mult = c(0.10, 0.10))) +
    scale_y_discrete(expand = expansion(add = c(0.45, 1.0))) +
    labs(x = NULL, y = NULL, title = PANEL_TITLES[metric_key]) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = title_sz, hjust = 0.5, margin = margin(b = 4)),
      panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = axis_sz, colour = "grey35"),
      axis.text.y        = if (show_y) element_text(size = ylab_sz, colour = "grey15") else element_blank(),
      axis.ticks         = element_blank(),
      plot.margin        = margin(4, 10, 4, 4)
    )
}

# Assemble the six panels into the 2-column, 3-row grid (no annotation yet).
build_grid <- function(title_sz, axis_sz, ylab_sz, val_sz, cue_sz) {
  mp <- function(k, y) make_panel(k, y, title_sz, axis_sz, ylab_sz, val_sz, cue_sz)
  (mp("retention_rate",     TRUE) | mp("avg_motivation",        FALSE)) /
  (mp("avg_language_cefr",   TRUE) | mp("cross_group_tie_ratio", FALSE)) /
  (mp("female_dropout_rate", TRUE) | mp("male_dropout_rate",     FALSE))
}

# (1) THESIS figure: self-contained with title, subtitle, and key footnote.
p <- build_grid(10, 8.5, 9.5, 3, 2.9) +
  plot_annotation(
    title = "Any buddy pairing lifts retention and cuts dropout; timing beats targeting",
    subtitle = "Four programme designs compared across six outcomes, each against the current Baseline (no buddy).",
    caption = paste0("How to read this: dot = average of 300 simulated runs; horizontal line = 95% confidence interval. ",
                     "Grey = Baseline (no buddy programme),\nteal = a buddy variant; the dashed line marks the Baseline average. ",
                     "The note in each panel says which direction is better."),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 10.5, colour = "grey35", margin = margin(b = 12)),
      plot.caption  = element_text(size = 8.5, colour = "grey45", hjust = 0, margin = margin(t = 12)),
      plot.margin   = margin(14, 12, 10, 10))) &
  # Per-subplot margins are the gutters between panels: ~30pt between columns
  # (right + left) and ~26pt between rows (bottom + top) keeps each metric visually
  # separate instead of reading as one grid.
  theme(plot.margin = margin(13, 20, 13, 10))

ggsave(file.path(FIG_DIR, "fig_centrality_buddy.png"), p,
       width = 9.8, height = 8.7, dpi = 300, bg = "white")
cat(sprintf("Figure saved: %s\n", file.path(FIG_DIR, "fig_centrality_buddy.png")))

# (2) SLIDE variant: no embedded title/subtitle/footnote (the slide frame and its
# side column carry those) and larger fonts so it stays legible at half-slide width.
p_slide <- build_grid(13, 11, 12, 4, 3.7) &
  theme(plot.margin = margin(11, 16, 11, 8))

ggsave(file.path(FIG_DIR, "fig_centrality_buddy_slide.png"), p_slide,
       width = 8.8, height = 9.0, dpi = 300, bg = "white")
cat(sprintf("Figure saved: %s\n", file.path(FIG_DIR, "fig_centrality_buddy_slide.png")))

# --- Save full results -------------------------------------------------------
saveRDS(list(
  results_tests = results_tests,
  data_list     = data_list,
  metadata      = list(date = Sys.time(), scenarios = SCENARIOS)
), file.path(OUT_DIR, "centrality_buddy.rds"))

cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_centrality_buddy.csv")))
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "centrality_buddy.rds")))
cat("\n=== Item 10 CentralityBuddy 3-way comparison complete ===\n")
