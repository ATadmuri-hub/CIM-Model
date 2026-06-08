# 12_distributional.R — Distributional analysis: quantile ribbons, tail risks, distributions
# CIM v6.4 — Calisthenics Integration Model

library(tidyverse)
source("R/constants.R")

DATA_DIR <- "data"
FIGS_DIR <- "figures"
dir.create(FIGS_DIR, showWarnings = FALSE)

results_df    <- readRDS(file.path(DATA_DIR, "results_df.rds"))
timeseries_df <- readRDS(file.path(DATA_DIR, "timeseries_df.rds"))
agents_df     <- readRDS(file.path(DATA_DIR, "agents_df.rds"))

SCENARIOS <- levels(results_df$scenario)

# ── 1. Quantile ribbon plot: retention rate distribution across scenarios ─────
retention_quantiles <- results_df %>%
  group_by(scenario) %>%
  summarise(
    q10 = quantile(retention_rate, 0.10, na.rm = TRUE),
    q25 = quantile(retention_rate, 0.25, na.rm = TRUE),
    q50 = quantile(retention_rate, 0.50, na.rm = TRUE),
    q75 = quantile(retention_rate, 0.75, na.rm = TRUE),
    q90 = quantile(retention_rate, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

p_ribbon <- ggplot(retention_quantiles, aes(x = reorder(scenario, q50))) +
  geom_linerange(aes(ymin = q10, ymax = q90), linewidth = 6, alpha = 0.2,
                 colour = "#2166AC") +
  geom_linerange(aes(ymin = q25, ymax = q75), linewidth = 6, alpha = 0.4,
                 colour = "#2166AC") +
  geom_point(aes(y = q50), size = 3, colour = "#2166AC") +
  coord_flip() +
  labs(
    title    = "Retention Rate Distribution by Scenario",
    subtitle = "Inner band: IQR (25–75%); outer band: 10–90%",
    x = NULL, y = "Retention Rate (%)"
  ) +
  theme_thesis +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(FIGS_DIR, "12_retention_quantiles.png"), p_ribbon,
       width = 10, height = 7, dpi = 300)
cat("Saved: 12_retention_quantiles.png\n")

# ── 2. Tail risk: probability of retention < TARGET_RETENTION (policy target) ─
tail_risk <- results_df %>%
  group_by(scenario) %>%
  summarise(
    p_low_retention = mean(retention_rate < TARGET_RETENTION, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(p_low_retention)

write_csv(tail_risk, file.path(DATA_DIR, "tail_risk.csv"))
cat("Saved: tail_risk.csv\n")

p_tail <- ggplot(tail_risk,
                 aes(x = reorder(scenario, -p_low_retention),
                     y = p_low_retention)) +
  geom_col(fill = "#D6604D", alpha = 0.85) +
  geom_text(aes(label = scales::percent(p_low_retention, accuracy = 1)),
            hjust = -0.1, size = 3) +
  coord_flip(ylim = c(0, max(tail_risk$p_low_retention) * 1.3)) +
  labs(
    title = paste0("Tail Risk: P(Retention Rate < Policy Target of ", TARGET_RETENTION, "%)"),
    x = NULL, y = "Probability"
  ) +
  theme_thesis +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(FIGS_DIR, "12_tail_risk.png"), p_tail,
       width = 10, height = 7, dpi = 300)
cat("Saved: 12_tail_risk.png\n")

# ── 3. Weekly motivation trajectories: median + IQR ribbon ───────────────────
# Focus on key comparison scenarios
focus_scenarios <- c("Baseline", "BuddyProgram", "RotatingGroups",
                     "Targeting90", "WomenChildcare", "Winter50")

ts_focus <- timeseries_df %>%
  filter(scenario %in% focus_scenarios) %>%
  group_by(scenario, week) %>%
  summarise(
    med   = median(motivation, na.rm = TRUE),
    q25   = quantile(motivation, 0.25, na.rm = TRUE),
    q75   = quantile(motivation, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_ts <- ggplot(ts_focus, aes(x = week, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, colour = NA) +
  geom_line(aes(y = med), linewidth = 0.9) +
  geom_vline(xintercept = c(9, 28),
             linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  annotate("rect", xmin = 9, xmax = 28,
           ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "steelblue") +
  scale_colour_manual(values = SCEN_COLORS) +
  scale_fill_manual(values = SCEN_COLORS) +
  labs(
    title    = "Motivation Trajectory (Median ± IQR)",
    subtitle = "Selected scenarios, weeks 0–52",
    x = "Week", y = "Avg Motivation", colour = NULL, fill = NULL
  ) +
  theme_thesis +
  theme(legend.position = "bottom")

ggsave(file.path(FIGS_DIR, "12_motivation_trajectories.png"), p_ts,
       width = 11, height = 6, dpi = 300)
cat("Saved: 12_motivation_trajectories.png\n")

# ── 4. Agent-level distributions: dropout week histogram ─────────────────────
dropout_agents <- agents_df %>%
  filter(!is.na(dropout_week) & dropout_week > 0)

# Winter onset: weeks 9-28 (verified from CIM_v6_3.nlogo: indoor-season-start=9)
.winter_wk <- if (exists("WINTER_ONSET_WEEK")) WINTER_ONSET_WEEK else 9L
# Ridgeline v3: legend outside, clearer text, better proportions

scen_stats <- results_df %>%
  group_by(scenario) %>%
  summarise(
    drop_rate = round(mean(100 - retention_rate), 1),
    .groups   = "drop"
  )

med_wks <- dropout_agents %>%
  group_by(scenario) %>%
  summarise(med_wk = median(dropout_week, na.rm = TRUE), .groups = "drop")

scen_stats <- scen_stats %>%
  left_join(med_wks, by = "scenario") %>%
  arrange(-med_wk)

# Compute density
dens_list <- lapply(seq_len(nrow(scen_stats)), function(i) {
  sc <- scen_stats$scenario[i]
  vals <- dropout_agents$dropout_week[dropout_agents$scenario == sc]
  d <- density(vals, from = 1, to = 52, adjust = 1.4, n = 512)
  data.frame(scenario = sc, x = d$x, density = d$y,
             offset = (i - 1) * 0.028, stringsAsFactors = FALSE)
})
dens <- do.call(rbind, dens_list)

# Normalise heights
max_d <- max(dens$density)
dens$density_scaled <- dens$density / max_d * 0.05
dens$ymin <- dens$offset
dens$ymax <- dens$offset + dens$density_scaled

# Labels
labels_df <- scen_stats %>%
  mutate(
    offset = (row_number() - 1) * 0.028,
    y_pos  = offset + 0.003
  )

# Baseline offset for highlighting
baseline_offset <- labels_df$offset[labels_df$scenario == "Baseline"]

.ws <- if (exists("WINTER_ONSET_WEEK")) WINTER_ONSET_WEEK else 9L
.we <- if (exists("WINTER_END_WEEK"))   WINTER_END_WEEK   else 28L

p_dropout <- ggplot(dens) +
  # Winter band
  annotate("rect", xmin = .ws, xmax = .we,
    ymin = -0.003, ymax = max(dens$ymax) + 0.005,
    fill = "#DEEBF7", alpha = 0.50) +
  # Ridgeline ribbons
  geom_ribbon(
    aes(x = x, ymin = ymin, ymax = ymax, group = scenario, fill = x),
    alpha = 0.82, colour = "white", linewidth = 0.3
  ) +
  # Subtle top edge
  geom_line(
    aes(x = x, y = ymax, group = scenario),
    colour = "grey25", linewidth = 0.12, alpha = 0.4
  ) +
  # Highlight Baseline with thicker outline
  geom_line(
    data = dens[dens$scenario == "Baseline", ],
    aes(x = x, y = ymax),
    colour = "grey10", linewidth = 0.4, alpha = 0.7
  ) +
  # Scenario labels — bold name, normal rate
  geom_text(
    data = labels_df,
    aes(x = 0, y = y_pos,
        label = paste0(scenario, "  \u2014  ", drop_rate, "%")),
    hjust = 1, size = 2.8, colour = "grey15",
    inherit.aes = FALSE
  ) +
  # Median marker dot on each ridge
  geom_point(
    data = data.frame(
      scenario = scen_stats$scenario,
      x = scen_stats$med_wk,
      y = labels_df$offset + 0.001
    ),
    aes(x = x, y = y), shape = 21, size = 2.5,
    fill = "white", colour = "grey30", stroke = 0.6,
    inherit.aes = FALSE
  ) +
  # Winter lines
  annotate("text", x = (.ws + .we) / 2, y = max(dens$ymax) + 0.003,
           label = "Indoor Season", size = 3.0, colour = "#1A3A5C",
           fontface = "italic", alpha = 0.7) +
  geom_vline(xintercept = .ws, colour = "#1A3A5C",
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = .we, colour = "#1A3A5C",
             linetype = "dashed", linewidth = 0.65) +
  # Winter labels at bottom
  annotate("text", x = .ws, y = max(dens$ymax) + 0.008,
           label = "Week 9", size = 2.8, colour = "#1A3A5C", fontface = "bold") +
  annotate("text", x = .we, y = max(dens$ymax) + 0.008,
           label = "Week 28", size = 2.8, colour = "#1A3A5C", fontface = "bold") +
  # Gradient
  scale_fill_viridis_c(
    option = "C",
    name   = "Dropout Timing     ",
    breaks = c(8, 26, 46),
    labels = c("Early (wk 1\u201313)", "Mid (wk 14\u201333)", "Late (wk 34\u201352)"),
    guide  = guide_colourbar(
      barwidth  = 15, barheight = 0.7,
      title.position = "left", title.hjust = 0.5,
      label.position = "bottom"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 52, 13),
    limits = c(0, 53), expand = c(0, 0)
  ) +
  labs(
    title    = "When Do Agents Drop Out?",
    subtitle = paste0(
      "Each ridge shows the dropout timing distribution for one scenario. ",
      "Purple = early dropout, yellow = late. Sorted by median dropout week. ",
      "White dot = median. Baseline outlined in bold."
    ),
    x = "Week of Dropout",
    y = NULL,
    caption  = paste0(
      "Kernel density (bw \u00d71.4), normalised peak height. ",
      "Labels: scenario name and cumulative dropout rate. ",
      "Shaded band: indoor season (weeks 9\u201328)."
    )
  ) +
  coord_cartesian(xlim = c(0, 53),
                  ylim = c(-0.01, max(dens$ymax) + 0.012),
                  clip = "off") +
  theme_thesis +
  theme(
    # Legend: BOTTOM, OUTSIDE, horizontal
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_text(size = 9, face = "bold", colour = "grey20"),
    legend.text        = element_text(size = 8, colour = "grey30"),
    legend.margin      = margin(t = 5),
    legend.box.margin  = margin(t = -5),
    # Axes
    axis.text.x        = element_text(size = 9, colour = "grey25"),
    axis.text.y        = element_blank(),
    axis.ticks         = element_blank(),
    axis.title.x       = element_text(size = 10, colour = "grey15",
                                       face = "bold", margin = margin(t = 8)),
    # Grid
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.15),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    # Titles
    plot.title         = element_text(size = 15, face = "bold", colour = "grey5",
                                       margin = margin(b = 3)),
    plot.subtitle      = element_text(size = 8.5, colour = "grey35",
                                       margin = margin(b = 10), lineheight = 1.2),
    plot.caption       = element_text(size = 7.5, colour = "grey45", hjust = 0,
                                       margin = margin(t = 6)),
    # Margins
    plot.margin        = margin(10, 15, 10, 145),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

ggsave(file.path(FIGS_DIR, "12_dropout_distribution.png"), p_dropout,
       width = 14, height = 10, dpi = 300)
cat("Saved: 12_dropout_distribution.png (ridgeline v3)\n")

# ── 5. Language proficiency distribution at final week ───────────────────────
lang_dist <- agents_df %>%
  filter(!is.na(language_gain))

if (nrow(lang_dist) > 0) {
  p_lang <- ggplot(lang_dist,
                   aes(x = reorder(scenario, language_gain, FUN = median),
                       y = language_gain)) +
    geom_violin(fill = "#74ADD1", alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.size = 0.5, alpha = 0.9) +
    coord_flip() +
    labs(
      title = "Suboptimal Composition collapses language gain to near-zero",
      x = NULL, y = "Language Gain"
    ) +
    theme_thesis

  ggsave(file.path(FIGS_DIR, "12_language_distribution.png"), p_lang,
         width = 10, height = 7, dpi = 300)
  cat("Saved: 12_language_distribution.png\n")
} else {
  cat("Skipped language distribution: language_gain column not found in agents_df\n")
}

cat("\n=== Script 12 complete ===\n")
