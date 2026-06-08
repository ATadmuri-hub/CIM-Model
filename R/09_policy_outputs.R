# 09_policy_outputs.R — Policy outputs: cost-effectiveness frontier, P(targets), equity
# Run from CIM_Model/ directory
library(tidyverse)
library(patchwork)

DATA_DIR <- "data"
FIG_DIR  <- "figures"
TAB_DIR  <- "tables"
dir.create(FIG_DIR, showWarnings = FALSE)
dir.create(TAB_DIR, showWarnings = FALSE)

results_df <- readRDS(file.path(DATA_DIR, "results_df.rds"))

## Load shared constants (targets, scenario classification)
source("R/constants.R")

## Policy levers (can be directly modified by programme implementers)
# Suboptimal Composition = LEVER: adjusting locals-per-group is actionable
POLICY_LEVER_SCENARIOS <- c(
  "Baseline", "No Indoor Continuity", "Minimal Support",
  "Low Park Density", "Weak Peer Influence", "Suboptimal Composition",
  "Women-Only Groups", "NoIndoor Minimal", "Targeting50", "Targeting70", "Targeting90",
  "BuddyProgram", "RotatingGroups", "Winter50", "WomenChildcare"
)
## Context variable (NOT a lever — reflects community SES composition; stress-test only)
CONTEXT_SCENARIOS <- c("High SES Heterogeneity")

# SCEN_COLORS loaded from R/constants.R (centralized palette)

# theme_thesis loaded from constants.R (unified)

# ============================================================
# 1. Cost per retained participant (mean ± 95% CI by scenario)
# ============================================================
cost_summary <- results_df %>%
  group_by(scenario) %>%
  summarise(
    n         = n(),
    mean_cost = mean(cost_per_retained, na.rm = TRUE),
    sd_cost   = sd(cost_per_retained, na.rm = TRUE),
    ci_lo     = mean_cost - qt(0.975, n - 1) * sd_cost / sqrt(n),
    ci_hi     = mean_cost + qt(0.975, n - 1) * sd_cost / sqrt(n),
    mean_ret  = mean(retention_rate, na.rm = TRUE),
    sd_ret    = sd(retention_rate, na.rm = TRUE),
    se_ret    = sd_ret / sqrt(n),
    mean_tie  = mean(cross_group_tie_ratio, na.rm = TRUE),
    mean_lang = mean(avg_language_cefr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(mean_cost, sd_cost, ci_lo, ci_hi), ~ round(.x, 0)))

cat("=== Cost per Retained Participant by Scenario ===\n")
print(cost_summary %>% select(scenario, mean_cost, sd_cost, ci_lo, ci_hi, mean_ret), n = Inf)

# ============================================================
# 2. P(meeting policy targets) by scenario
#    Targets: retention >= 40%, cross_tie >= 0.40, cost <= 3500, CEFR >= 1.0
# ============================================================
# Use constants from constants.R if loaded; fallback to hardcoded defaults
RET_TARGET  <- if (exists("TARGET_RETENTION")) TARGET_RETENTION else 40
TIE_TARGET  <- if (exists("TARGET_CROSS_TIE")) TARGET_CROSS_TIE else 0.40
COST_TARGET <- if (exists("TARGET_COST")) TARGET_COST else 3500
LANG_TARGET <- if (exists("TARGET_LANGUAGE")) TARGET_LANGUAGE else 1.0

target_prob <- results_df %>%
  group_by(scenario) %>%
  summarise(
    n               = n(),
    p_retention     = mean(retention_rate >= RET_TARGET, na.rm = TRUE) * 100,
    p_cross_tie     = mean(cross_group_tie_ratio >= TIE_TARGET, na.rm = TRUE) * 100,
    p_cost_ok       = mean(cost_per_retained <= COST_TARGET, na.rm = TRUE) * 100,
    p_language      = mean(avg_language_cefr >= LANG_TARGET, na.rm = TRUE) * 100,
    p_joint_primary = mean(retention_rate >= RET_TARGET &
                             cross_group_tie_ratio >= TIE_TARGET, na.rm = TRUE) * 100,
    p_joint_all     = mean(retention_rate >= RET_TARGET &
                             cross_group_tie_ratio >= TIE_TARGET &
                             cost_per_retained <= COST_TARGET &
                             avg_language_cefr >= LANG_TARGET, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("p_"), ~ round(.x, 1)))

cat(sprintf("\n=== P(Meeting Targets): retention>=%g%%, tie>=%.2f, cost<=EUR%g, CEFR>=%g ===\n",
            RET_TARGET, TIE_TARGET, COST_TARGET, LANG_TARGET))
print(target_prob, n = Inf, width = 200)

# ============================================================
# 3. Equity: female − male dropout gap by scenario
# ============================================================
equity_df <- results_df %>%
  group_by(scenario) %>%
  summarise(
    n           = n(),
    gender_gap  = mean(female_dropout_rate - male_dropout_rate, na.rm = TRUE),
    sd_gap      = sd(female_dropout_rate - male_dropout_rate, na.rm = TRUE),
    ci_lo       = gender_gap - qt(0.975, n - 1) * sd_gap / sqrt(n),
    ci_hi       = gender_gap + qt(0.975, n - 1) * sd_gap / sqrt(n),
    mean_f_drop = mean(female_dropout_rate, na.rm = TRUE),
    mean_m_drop = mean(male_dropout_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(gender_gap, sd_gap, ci_lo, ci_hi, mean_f_drop, mean_m_drop), ~ round(.x, 2)))

cat("\n=== Gender Equity: Female - Male Dropout Gap ===\n")
print(equity_df, n = Inf, width = 150)

# ============================================================
# FIG 9: Cost-effectiveness frontier (scatter: cost vs retention)
# ============================================================
fig9 <- ggplot(cost_summary, aes(x = mean_cost, y = mean_ret, color = scenario)) +
  annotate("rect", xmin = -Inf, xmax = COST_TARGET, ymin = RET_TARGET, ymax = Inf,
           fill = "#2E86AB", alpha = 0.04) +
  geom_errorbar(aes(ymin = mean_ret - se_ret, ymax = mean_ret + se_ret),
                width = 20, alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(size = mean_tie * 100), alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = scenario), size = 2.5, show.legend = FALSE,
                            box.padding = 0.7, point.padding = 0.3, min.segment.length = 0,
                            max.overlaps = Inf, force = 3, segment.size = 0.3,
                            segment.color = "grey60", seed = 42) +
  geom_hline(yintercept = RET_TARGET, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  geom_vline(xintercept = COST_TARGET, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  scale_size_area(name = "Cross-tie\nratio (%)", max_size = 9, breaks = c(35, 40, 45, 50)) +
  scale_color_manual(values = SCEN_COLORS, guide = "none") +
  labs(
    title    = "Cost-Effectiveness Frontier",
    subtitle = paste0("Bubble size = mean cross-group tie ratio; dashed = policy targets ",
                      "(retention ", RET_TARGET, "%, cost EUR ",
                      format(COST_TARGET, big.mark = ","), ")"),
    x        = "Mean Cost per Retained Participant (EUR)",
    y        = "Mean Retention Rate (%)",
    caption  = paste0("Point = scenario mean; error bars = \u00b1 1 SE. ",
                      "N = ", min(cost_summary$n), "\u2013", max(cost_summary$n),
                      " runs per scenario. Top-left quadrant meets both targets.")
  ) +
  theme_thesis +
  theme(legend.position = "right")

ggsave(file.path(FIG_DIR, "fig9_cost_effectiveness.png"), fig9,
       width = 12, height = 8, dpi = 300)
cat("\nSaved: fig9_cost_effectiveness.png\n")

# ============================================================
# FIG 10: P(meeting targets) heatmap
# ============================================================
target_long <- target_prob %>%
  pivot_longer(
    cols      = c(p_retention, p_cross_tie, p_cost_ok, p_language,
                  p_joint_primary, p_joint_all),
    names_to  = "target",
    values_to = "probability"
  ) %>%
  mutate(target = recode(target,
    p_retention     = "Retention >= 40%",
    p_cross_tie     = "Cross-tie >= 0.40",
    p_cost_ok       = "Cost <= EUR3,500",
    p_language      = "CEFR >= 1.0",
    p_joint_primary = "Retention & Tie",
    p_joint_all     = "All targets"
  ),
  target = factor(target, levels = c("Retention >= 40%", "Cross-tie >= 0.40",
                                      "Cost <= EUR3,500", "CEFR >= 1.0",
                                      "Retention & Tie", "All targets")))

# Sort scenarios by "All targets" probability (best at top)
  all_targets_order <- target_long %>%
    filter(target == "All targets") %>%
    arrange(probability) %>%
    pull(scenario)
  target_long <- target_long %>%
    mutate(scenario = factor(scenario, levels = all_targets_order))

  fig10 <- ggplot(target_long, aes(x = target, y = scenario, fill = probability)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(probability, "%")),
            size = 3.1, fontface = "bold",
            color = ifelse(target_long$probability > 50, "white", "black")) +
  scale_fill_viridis_c(option = "D",
    values  = scales::rescale(c(0, 25, 50, 75, 100)),
    limits  = c(0, 100),
    name    = "P (%)"
  ) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  labs(
    title   = "Probability of Meeting Policy Targets by Scenario",
    x       = NULL, y = NULL,
    caption = sprintf("Based on %d--%d simulation runs per scenario. White text: P > 50%%.", min(target_prob$n), max(target_prob$n))
  ) +
  theme_thesis +
  theme(legend.position = "right", axis.text.y = element_text(size = 9))

ggsave(file.path(FIG_DIR, "fig10_target_probabilities.png"), fig10,
       width = 12, height = 7, dpi = 300)
cat("Saved: fig10_target_probabilities.png\n")

# ============================================================
# FIG 11: Gender equity gap by scenario
# ============================================================
fig11 <- equity_df %>%
  ggplot(aes(x = reorder(scenario, gender_gap), y = gender_gap,
             fill = gender_gap > 0)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.3, linewidth = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
  scale_fill_manual(values = c("TRUE" = "#E84855", "FALSE" = "#2E86AB"), guide = "none") +
  coord_flip() +
  labs(
    title   = "Gender Dropout Gap by Scenario",
    x       = NULL,
    y       = "Female minus Male Dropout Rate (percentage points)",
    caption = sprintf("Positive = female dropout higher than male; error bars = 95%% CI (N = %d--%d runs)", min(equity_df$n), max(equity_df$n))
  ) +
  theme_thesis

ggsave(file.path(FIG_DIR, "fig11_gender_equity_gap.png"), fig11,
       width = 10, height = 6, dpi = 300)
cat("Saved: fig11_gender_equity_gap.png\n")

# ============================================================
# EQUITY FRONTIER: Cost per retained vs isolation reduction
# Shows trade-off between cost efficiency and social inclusion
# (Policy levers only — context variables excluded)
# ============================================================
if ("cross_group_friends" %in% colnames(readRDS(file.path(DATA_DIR, "agents_df.rds")))) {
  agents_df <- readRDS(file.path(DATA_DIR, "agents_df.rds"))
  
  isolation_by_scenario <- agents_df %>%
    filter(breed == "refugee") %>%
    group_by(scenario) %>%
    summarise(
      isolation_rate = mean(cross_group_friends == 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  equity_frontier <- cost_summary %>%
    left_join(isolation_by_scenario, by = "scenario") %>%
    mutate(
      is_policy_lever = scenario %in% POLICY_LEVER_SCENARIOS,
      is_context = scenario %in% CONTEXT_SCENARIOS
    )
  
  p_equity <- ggplot(equity_frontier, 
                     aes(x = mean_cost, y = isolation_rate,
                         color = is_context, shape = is_context)) +
    geom_point(aes(size = mean_ret), alpha = 0.9) +
    ggrepel::geom_text_repel(aes(label = scenario), size = 2.2,
                box.padding = 0.5, max.overlaps = 25,
                force = 3, force_pull = 0.5,
                segment.size = 0.3, segment.color = "grey60",
                min.segment.length = 0.2,
              show.legend = FALSE) +
    scale_color_manual(
      values = c("FALSE" = "#2E86AB", "TRUE" = "#888888"),
      labels = c("Policy lever", "Context variable (not actionable)"),
      name = NULL
    ) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4), guide = "none") +
    scale_size_area(name = "Retention (%)", max_size = 8) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Equity Frontier: Cost vs Social Isolation",
      subtitle = "Bubble size = retention rate; grey X = context variable (not a lever); isolation = % migrants with zero cross-group ties at week 52",
      x = "Cost per retained participant (EUR)",
      y = "Migrant social isolation rate (% with 0 cross-group friends)"
    ) +
    # Preferred direction annotation
    annotate("segment", x = 4200, xend = 3700, y = 0.83, yend = 0.80,
             arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
             colour = "grey50", linewidth = 0.5) +
    annotate("text", x = 4250, y = 0.835,
             label = "Preferred
(low cost, low isolation)",
             size = 2.5, colour = "grey45", fontface = "italic", hjust = 0) +
    theme_thesis +
    theme(legend.position = "right")
  
  ggsave(file.path(FIG_DIR, "fig_equity_frontier.png"), p_equity,
         width = 13, height = 7, dpi = 300)
  cat("Saved: fig_equity_frontier.png
")
  saveRDS(equity_frontier, file.path(DATA_DIR, "policy_equity_frontier.rds"))
}

# ---- Save ----
saveRDS(cost_summary,  file.path(DATA_DIR, "policy_cost_summary.rds"))
saveRDS(target_prob,   file.path(DATA_DIR, "policy_target_prob.rds"))
saveRDS(equity_df,     file.path(DATA_DIR, "policy_equity.rds"))
write_csv(cost_summary, file.path(TAB_DIR, "table_cost_effectiveness.csv"))
write_csv(target_prob,  file.path(TAB_DIR, "table_target_probabilities.csv"))
write_csv(equity_df,    file.path(TAB_DIR, "table_equity_gender.csv"))

cat("\nPolicy outputs complete.\n")
