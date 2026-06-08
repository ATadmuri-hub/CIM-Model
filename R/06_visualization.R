# 06_visualization.R — Publication-quality thesis figures (CIM v6.4, 16 scenarios)
library(tidyverse)
library(patchwork)

DATA_DIR <- "data"
FIG_DIR  <- "figures"
dir.create(FIG_DIR, showWarnings = FALSE)

source("R/constants.R")

results_df    <- readRDS(file.path(DATA_DIR, "results_df.rds"))
timeseries_df <- readRDS(file.path(DATA_DIR, "timeseries_df.rds"))

SCENARIOS <- levels(results_df$scenario)

# SCEN_COLORS loaded from R/constants.R (centralized palette)

# Group labels for faceting
ORIGINAL_8 <- c("Baseline","No Indoor Continuity","Minimal Support",
                 "Low Park Density","Weak Peer Influence",
                 "Suboptimal Composition","High SES Heterogeneity","Women-Only Groups")
NEW_8 <- c("NoIndoor Minimal","Targeting50","Targeting70","Targeting90",
           "BuddyProgram","RotatingGroups","Winter50","WomenChildcare")

# Run counts per scenario (for accurate captions)
run_counts <- results_df %>% count(scenario)

# theme_thesis loaded from constants.R (unified)

# ── FIG 1: Scenario comparison — all 16, split into two panels ────────────────
plot_dotrange <- function(df, metric, xlab, title, vline = NULL) {
  summ <- df %>%
    group_by(scenario) %>%
    summarise(m  = mean(.data[[metric]], na.rm = TRUE),
              lo = quantile(.data[[metric]], 0.25, na.rm = TRUE),
              hi = quantile(.data[[metric]], 0.75, na.rm = TRUE),
              .groups = "drop")

  base_val <- summ$m[summ$scenario == "Baseline"]

  p <- ggplot(summ, aes(x = m, y = reorder(scenario, m), colour = scenario)) +
    geom_vline(xintercept = base_val, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 2.5, alpha = 0.35) +
    geom_point(size = 3) +
    scale_colour_manual(values = SCEN_COLORS) +
    labs(title = title, x = xlab, y = NULL,
         caption = "Point = mean; bar = IQR. Dashed line = Baseline mean.") +
    guides(colour = "none") +
    theme_thesis

  if (!is.null(vline)) p <- p + geom_vline(xintercept = vline,
                                             linetype = "dotted", colour = "grey60")
  p
}

fig1a <- plot_dotrange(results_df, "retention_rate",        "Retention Rate (%)",      "A: Retention at Week 52")
fig1b <- plot_dotrange(results_df, "avg_motivation",         "Average Motivation (0–1)", "B: Agent Motivation")
fig1c <- plot_dotrange(results_df, "cross_group_tie_ratio",  "Cross-Group Tie Ratio",   "C: Social Integration")
fig1d <- plot_dotrange(results_df, "avg_language_cefr",      "Language (CEFR)",         "D: Language Proficiency")

fig1 <- (fig1a | fig1b) / (fig1c | fig1d)
ggsave(file.path(FIG_DIR, "fig1_scenario_comparison.png"), fig1,
       width = 14, height = 11, dpi = 300)
cat("Saved: fig1_scenario_comparison.png\n")

# ── FIG 2: Motivation + participation trajectories (key scenarios only) ────────
FOCUS <- c("Baseline", "No Indoor Continuity", "Minimal Support",
           "BuddyProgram", "WomenChildcare", "Targeting90")

ts_focus <- timeseries_df %>%
  filter(scenario %in% FOCUS) %>%
  mutate(scenario = factor(scenario, levels = FOCUS)) %>%
  group_by(scenario, week) %>%
  summarise(
    mot_med = median(motivation,    na.rm = TRUE),
    mot_lo  = quantile(motivation,  0.25, na.rm = TRUE),
    mot_hi  = quantile(motivation,  0.75, na.rm = TRUE),
    par_med = median(participation, na.rm = TRUE),
    par_lo  = quantile(participation, 0.25, na.rm = TRUE),
    par_hi  = quantile(participation, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

fig2a <- ggplot(ts_focus, aes(x = week, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = mot_lo, ymax = mot_hi), alpha = 0.12, colour = NA) +
  geom_line(aes(y = mot_med), linewidth = 0.9) +
  geom_vline(xintercept = c(WINTER_ONSET_WEEK, WINTER_END_WEEK),
             linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  annotate("rect", xmin = WINTER_ONSET_WEEK, xmax = WINTER_END_WEEK,
           ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "steelblue") +
  annotate("text", x = (WINTER_ONSET_WEEK + WINTER_END_WEEK) / 2, y = Inf,
           label = "Indoor season", vjust = 1.5, size = 2.5, colour = "grey40", fontface = "italic") +
  scale_colour_manual(values = SCEN_COLORS) +
  scale_fill_manual(values = SCEN_COLORS) +
  labs(title = "A: Motivation Dynamics", x = "Week", y = "Average Motivation",
       caption = "Median ± IQR across runs") +
  # Direct labels at week 52 (UC3M standard: direct label > legend)
  ggrepel::geom_text_repel(data = ts_focus %>% filter(week == max(week)) %>% 
              distinct(scenario, .keep_all = TRUE),
            aes(x = week + 1, y = mot_med, label = scenario),
            hjust = 0, size = 2.5, show.legend = FALSE,
            direction = "y", min.segment.length = 0, segment.size = 0.2,
            segment.color = "grey70", box.padding = 0.12, max.overlaps = Inf, seed = 42) +
  theme_thesis + theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 65), clip = "off")

fig2b <- ggplot(ts_focus, aes(x = week, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = par_lo, ymax = par_hi), alpha = 0.12, colour = NA) +
  geom_line(aes(y = par_med), linewidth = 0.9) +
  geom_vline(xintercept = c(WINTER_ONSET_WEEK, WINTER_END_WEEK),
             linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  annotate("rect", xmin = WINTER_ONSET_WEEK, xmax = WINTER_END_WEEK,
           ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "steelblue") +
  annotate("text", x = (WINTER_ONSET_WEEK + WINTER_END_WEEK) / 2, y = Inf,
           label = "Indoor season", vjust = 1.5, size = 2.5, colour = "grey40", fontface = "italic") +
  scale_colour_manual(values = SCEN_COLORS) +
  scale_fill_manual(values = SCEN_COLORS) +
  labs(title = "B: Active Participants", x = "Week", y = "Participants per Week",
       caption = "Median ± IQR across runs") +
  ggrepel::geom_text_repel(data = ts_focus %>% filter(week == max(week)) %>% 
              distinct(scenario, .keep_all = TRUE),
            aes(x = week + 1, y = par_med, label = scenario),
            hjust = 0, size = 2.5, show.legend = FALSE,
            direction = "y", min.segment.length = 0, segment.size = 0.2,
            segment.color = "grey70", box.padding = 0.12, max.overlaps = Inf, seed = 42) +
  theme_thesis + theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 65), clip = "off")

fig2 <- fig2a | fig2b
ggsave(file.path(FIG_DIR, "fig2_trajectories.png"), fig2,
       width = 14, height = 6, dpi = 300)
cat("Saved: fig2_trajectories.png\n")

# ── FIG 3: Gender dropout gap ─────────────────────────────────────────────────
fig3 <- results_df %>%
  select(scenario, female_dropout_rate, male_dropout_rate) %>%
  pivot_longer(-scenario, names_to = "group", values_to = "rate") %>%
  mutate(group = recode(group,
    "female_dropout_rate" = "Female",
    "male_dropout_rate"   = "Male")) %>%
  group_by(scenario, group) %>%
  summarise(m = mean(rate, na.rm = TRUE), s = sd(rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(gap_group = ifelse(scenario %in% ORIGINAL_8, "Original scenarios", "Additional scenarios")) %>%
  ggplot(aes(x = reorder(scenario, m), y = m, fill = group)) +
  geom_col(position = position_dodge(0.75), width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s),
                position = position_dodge(0.75), width = 0.3) +
  scale_fill_manual(values = c("Female" = "#E84855", "Male" = "#2E86AB")) +
  coord_flip() +
  labs(title = "Women drop out more in every scenario except WomenChildcare",
       x = NULL, y = "Dropout Rate (%)", fill = NULL,
       caption = "Mean ± 1 SD. Female (red) vs Male (blue).") +
  theme_thesis + theme(legend.position = "bottom")

ggsave(file.path(FIG_DIR, "fig3_gender_dropout.png"), fig3,
       width = 11, height = 9, dpi = 300)
cat("Saved: fig3_gender_dropout.png\n")

# ── FIG 4: Cohort language gain ───────────────────────────────────────────────
cohort_summ <- results_df %>%
  select(scenario, recent_cohort_lang_gain, established_cohort_lang_gain,
         settled_cohort_lang_gain) %>%
  pivot_longer(-scenario, names_to = "cohort", values_to = "gain") %>%
  mutate(cohort = recode(cohort,
    "recent_cohort_lang_gain"      = "Recent (<6 mo)",
    "established_cohort_lang_gain" = "Established (6–18 mo)",
    "settled_cohort_lang_gain"     = "Settled (>18 mo)")) %>%
  mutate(cohort = factor(cohort, levels = c(
    "Recent (<6 mo)", "Established (6–18 mo)", "Settled (>18 mo)"))) %>%
  group_by(scenario, cohort) %>%
  summarise(m = mean(gain, na.rm = TRUE), s = sd(gain, na.rm = TRUE), .groups = "drop")

cohort_summ <- cohort_summ %>%
  mutate(scenario = reorder(scenario, m, FUN = mean))

base_ref <- cohort_summ %>%
  filter(scenario == "Baseline") %>%
  select(cohort, base_m = m)

fig4 <- ggplot(cohort_summ, aes(x = scenario, y = m, colour = m)) +
  geom_hline(data = base_ref, aes(yintercept = base_m),
             linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_linerange(aes(ymin = m - s, ymax = m + s), linewidth = 0.7, alpha = 0.35) +
  geom_point(size = 3) +
  scale_colour_viridis_c(option = "D", end = 0.88) +
  guides(colour = "none") +
  facet_wrap(~ cohort, ncol = 3) +
  coord_flip() +
  labs(title = "Language Gain by Arrival Cohort",
       subtitle = "Recent arrivals gain most; Suboptimal Composition collapses all cohorts",
       x = NULL, y = "CEFR Gain (weeks 1–52)",
       caption = paste0("Point = mean; bar = ± 1 SD; dashed line = Baseline mean. ",
                        "N = ", format(nrow(results_df), big.mark = ","),
                        " runs across 23 scenarios.")) +
  theme_thesis +
  theme(strip.text = element_text(size = 10, face = "bold"))

ggsave(file.path(FIG_DIR, "fig4_cohort_language.png"), fig4,
       width = 12, height = 9, dpi = 300)
cat("Saved: fig4_cohort_language.png\n")

# ── FIG 5: Prior exercise retention ──────────────────────────────────────────
ex_summ <- results_df %>%
  select(scenario, prior_exercise_retention, no_exercise_retention) %>%
  pivot_longer(-scenario, names_to = "group", values_to = "ret") %>%
  mutate(group = recode(group,
    "prior_exercise_retention" = "Prior exercise",
    "no_exercise_retention"    = "No prior exercise")) %>%
  group_by(scenario, group) %>%
  summarise(m = mean(ret, na.rm = TRUE), s = sd(ret, na.rm = TRUE), .groups = "drop")

ex_wide <- ex_summ %>%
  pivot_wider(names_from = group, values_from = c(m, s), names_sep = "_") %>%
  rename(prior_m = `m_Prior exercise`, no_m = `m_No prior exercise`,
         prior_s = `s_Prior exercise`, no_s = `s_No prior exercise`) %>%
  mutate(gap = prior_m - no_m,
         overall_m = (prior_m + no_m) / 2,
         scenario = reorder(scenario, overall_m))

base_overall <- ex_wide$overall_m[ex_wide$scenario == "Baseline"]

fig5 <- ggplot(ex_wide, aes(y = scenario)) +
  geom_vline(xintercept = base_overall, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  geom_segment(aes(x = no_m, xend = prior_m, yend = scenario),
               colour = "grey65", linewidth = 1.2) +
  geom_linerange(aes(x = no_m, xmin = no_m - no_s, xmax = no_m + no_s),
                 colour = "#CC7722", linewidth = 0.35, alpha = 0.5) +
  geom_linerange(aes(x = prior_m, xmin = prior_m - prior_s, xmax = prior_m + prior_s),
                 colour = "#2E86AB", linewidth = 0.35, alpha = 0.5) +
  geom_point(aes(x = no_m), colour = "#CC7722", size = 3) +
  geom_point(aes(x = prior_m), colour = "#2E86AB", size = 3) +
  geom_text(aes(x = (no_m + prior_m) / 2, label = paste0("+", round(gap, 0), " pp")),
            size = 2.4, colour = "grey35", vjust = -0.8) +
  labs(
    title = "Prior exercise consistently increases retention across all scenarios",
    subtitle = "Blue = prior exercise; orange = no prior exercise; segment = exercise effect (pp)",
    x = "Retention Rate (%)", y = NULL,
    caption = paste0("Points = group means; whiskers = ± 1 SD; ",
                     "dashed line = Baseline overall mean. ",
                     "N = ", format(nrow(results_df), big.mark = ","),
                     " runs across 23 scenarios.")) +
  theme_thesis +
  theme(legend.position = "none", panel.grid.major.y = element_blank())

ggsave(file.path(FIG_DIR, "fig5_prior_exercise_retention.png"), fig5,
       width = 10, height = 9, dpi = 300)
cat("Saved: fig5_prior_exercise_retention.png\n")

# ── FIG 6: PRCC Tornado Plot ──────────────────────────────────────────────────
prcc_df <- tryCatch(readRDS(file.path(DATA_DIR, "sensitivity_prcc.rds")), error = function(e) NULL)

if (is.null(prcc_df)) {
  cat("Skipped fig6: sensitivity_prcc.rds not found\n")
} else {
  param_labels <- c(
    motivation_decay_rate      = "\u03b3 (Motivation Decay)",
    peer_influence_coefficient = "\u03b2 (Peer Influence)",
    tie_formation_probability  = "p_tie (Tie Formation)",
    dropout_threshold          = "\u03b8 (Dropout Threshold)"
  )
  outcome_labels <- c(
    retention_rate_percent   = "Retention Rate",
    avg_motivation_level     = "Avg Motivation",
    avg_language_proficiency = "Language Proficiency",
    cross_group_tie_ratio    = "Cross-Group Tie Ratio",
    total_dropouts           = "Total Dropouts"
  )

  param_order <- prcc_df %>%
    filter(outcome == "retention_rate_percent") %>%
    arrange(abs(original)) %>%
    pull(parameter)

  plot_df <- prcc_df %>%
    filter(outcome %in% names(outcome_labels),
           parameter %in% names(param_labels)) %>%
    mutate(
      parameter = factor(parameter, levels = param_order,
                         labels = param_labels[param_order]),
      outcome   = factor(outcome, levels = names(outcome_labels),
                         labels = outcome_labels),
      direction = ifelse(original >= 0, "Positive", "Negative")
    )

  fig6 <- ggplot(plot_df, aes(x = original, y = parameter, fill = direction)) +
    geom_col(width = 0.55, alpha = 0.85) +
    geom_errorbar(aes(xmin = `min. c.i.`, xmax = `max. c.i.`),
                  orientation = "y", width = 0.2, linewidth = 0.4, colour = "grey30") +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    facet_wrap(~outcome, nrow = 1, scales = "free_x") +
    scale_fill_manual(values = c("Positive" = "#2E86AB", "Negative" = "#E84855")) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    labs(title    = "Sensitivity Analysis: PRCC Tornado Plot",
         subtitle = "Partial Rank Correlation Coefficients (bootstrapped 95% CI)",
         x = "PRCC", y = NULL, fill = NULL,
         caption = "N = 810 runs (3-level factorial: 81 parameter combinations x 10 replicates); 1,000 bootstrap replicates; seed = 42. Note: Dropouts panel mirrors Retention by construction.") +
    guides(fill = "none") +
    theme_thesis +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 9, face = "bold"),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 10, margin = margin(t = 8)),
          plot.caption = element_text(margin = margin(t = 10)),
          panel.spacing.x = unit(0.8, "lines"),
          plot.margin = margin(10, 15, 10, 10))

  ggsave(file.path(FIG_DIR, "fig6_prcc_tornado.png"), fig6,
         width = 15, height = 5.5, dpi = 300)
  cat("Saved: fig6_prcc_tornado.png\n")
}

# ── FIG: Additional scenarios — retention + cost comparison ──────────────────
new_scen_df <- results_df %>% filter(scenario %in% NEW_8)

fig_new <- new_scen_df %>%
  group_by(scenario) %>%
  summarise(
    ret_m = mean(retention_rate,    na.rm = TRUE),
    ret_s = sd(retention_rate,      na.rm = TRUE),
    cost_m = mean(cost_per_retained, na.rm = TRUE),
    cost_s = sd(cost_per_retained,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(baseline_ret  = results_df %>% filter(scenario == "Baseline") %>%
           pull(retention_rate) %>% mean(na.rm = TRUE),
         baseline_cost = results_df %>% filter(scenario == "Baseline") %>%
           pull(cost_per_retained) %>% mean(na.rm = TRUE),
         delta_ret  = ret_m  - baseline_ret,
         delta_cost = cost_m - baseline_cost) %>%
  ggplot(aes(x = delta_cost, y = delta_ret, colour = scenario, label = scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 4) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  scale_colour_manual(values = SCEN_COLORS) +
  labs(title    = "Additional Scenarios: Retention vs Cost Trade-off",
       subtitle = "Relative to Baseline (origin = Baseline mean)",
       x = "Δ Cost per Retained (€)", y = "Δ Retention Rate (pp)") +
  theme_thesis + theme(legend.position = "none")

# Try ggrepel, fall back if not installed
tryCatch({
  ggsave(file.path(FIG_DIR, "fig_new_scenarios_tradeoff.png"), fig_new,
         width = 9, height = 7, dpi = 300)
  cat("Saved: fig_new_scenarios_tradeoff.png\n")
}, error = function(e) {
  # Rebuild without ggrepel
  fig_new2 <- new_scen_df %>%
    group_by(scenario) %>%
    summarise(
      ret_m  = mean(retention_rate,    na.rm = TRUE),
      cost_m = mean(cost_per_retained, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      baseline_ret  = results_df %>% filter(scenario=="Baseline") %>% pull(retention_rate) %>% mean(na.rm=TRUE),
      baseline_cost = results_df %>% filter(scenario=="Baseline") %>% pull(cost_per_retained) %>% mean(na.rm=TRUE),
      delta_ret  = ret_m  - baseline_ret,
      delta_cost = cost_m - baseline_cost
    ) %>%
    ggplot(aes(x = delta_cost, y = delta_ret, colour = scenario, label = scenario)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey50") +
    geom_vline(xintercept=0, linetype="dashed", colour="grey50") +
    geom_point(size=4) +
    geom_text(vjust=-0.8, size=3) +
    scale_colour_manual(values=SCEN_COLORS) +
    labs(title="Additional Scenarios: Retention vs Cost Trade-off",
         subtitle="Relative to Baseline (origin = Baseline mean)",
         x="Δ Cost per Retained (€)", y="Δ Retention Rate (pp)") +
    theme_thesis

  ggsave(file.path(FIG_DIR, "fig_new_scenarios_tradeoff.png"), fig_new2,
         width=9, height=7, dpi=300)
  cat("Saved: fig_new_scenarios_tradeoff.png (no ggrepel)\n")
})

cat("\nAll figures saved to:", FIG_DIR, "\n")
