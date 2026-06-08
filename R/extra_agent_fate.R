library(tidyverse)
source("R/constants.R")

panel <- readRDS("data/panel_all.rds")
agents <- readRDS("data/agents_df.rds")

focus <- c("Baseline", "No Indoor Continuity", "BuddyProgram")

# Pick representative run per scenario (closest to median retention)
rep_runs <- sapply(focus, function(sc) {
  runs <- agents[agents$scenario == sc, ] %>%
    group_by(run) %>%
    summarise(ret = sum(dropped_out == FALSE) / n() * 100, .groups = "drop")
  target <- median(runs$ret)
  runs$run[which.min(abs(runs$ret - target))]
})

cat("Representative runs:", paste(focus, rep_runs, sep="=", collapse=", "), "
")

# Build heatmap data
hm_list <- lapply(focus, function(sc) {
  rid <- rep_runs[sc]

  # Agent dropout info
  ag <- agents %>%
    filter(scenario == sc, run == rid) %>%
    arrange(breed, gender) %>%
    mutate(orig_id = row_number())

  # Sort: survivors at top, early dropouts at bottom
  ag <- ag %>%
    mutate(sort_val = ifelse(dropped_out, dropout_week, 99)) %>%
    arrange(desc(sort_val)) %>%
    mutate(y_pos = row_number())

  # Weekly panel data
  wk <- panel %>%
    filter(scenario == sc, run == rid) %>%
    select(agent_id, week, motivation)

  if (nrow(wk) == 0) return(NULL)

  # Map agent_id to y_pos
  id_map <- ag %>% select(orig_id, y_pos, dropout_week, dropped_out)

  wk_mapped <- wk %>%
    inner_join(id_map, by = c("agent_id" = "orig_id")) %>%
    mutate(
      mot_display = ifelse(dropped_out & week > dropout_week, NA_real_, motivation),
      scenario_label = sc
    )

  wk_mapped
})

hm <- bind_rows(hm_list)
hm$scenario_label <- factor(hm$scenario_label, levels = focus)

cat("Heatmap data:", nrow(hm), "rows
")

ws <- if (exists("WINTER_ONSET_WEEK")) WINTER_ONSET_WEEK else 9L
we <- if (exists("WINTER_END_WEEK")) WINTER_END_WEEK else 28L

# Guard: Part 1 (auxiliary heatmap, NOT used in the thesis) maps panel motivation to
# agents_df via a row-number proxy because agents_df carries no agent_id; under some data
# orderings this join yields zero rows. Skip the plot rather than error, so Part 2 (the
# thesis figure agent_fate_map.png) always regenerates.
if (nrow(hm) > 0) {

p <- ggplot(hm, aes(x = week, y = y_pos, fill = mot_display)) +
  annotate("rect", xmin = ws, xmax = we, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.3) +
  geom_tile(width = 1, height = 1) +
  scale_fill_viridis_c(
    option = "C", na.value = "white", name = "Motivation",
    limits = c(0, 1), breaks = seq(0, 1, 0.25)
  ) +
  geom_vline(xintercept = ws, colour = "#1A3A5C", linetype = "dashed", linewidth = 0.4) +
  facet_wrap(~ scenario_label, ncol = 1) +
  scale_x_continuous(
    breaks = c(0, 9, 26, 39, 52),
    labels = c("0", "9", "26", "39", "52"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "52 Weeks in the Life of 100 Agents",
    subtitle = paste0(
      "Each row = one agent. Each column = one week. Colour = motivation. ",
      "White = dropped out. Sorted by survival time (survivors at top)."
    ),
    x = "Week of Programme",
    y = "Agents (sorted by dropout week)",
    caption = paste0(
      "One representative run per scenario (closest to median retention). ",
      "Viridis C: dark = low motivation, bright = high. ",
      "Dashed line = winter onset (week 9). ",
      "The white cliff in No Indoor Continuity shows the winter cascade."
    )
  ) +
  theme_thesis +
  theme(
    legend.position = "right",
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 9, colour = "grey35", lineheight = 1.2,
                                  margin = margin(b = 10)),
    plot.caption = element_text(size = 7.5, colour = "grey45", hjust = 0,
                                 lineheight = 1.2, margin = margin(t = 10)),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave("figures/agent_fate_heatmap.png", p, width = 14, height = 12, dpi = 300)
cat("Saved: figures/agent_fate_heatmap.png
")
} else {
  cat("[agent_fate] Part 1 heatmap skipped: panel-to-agents join produced no rows (auxiliary figure, not used in the thesis).
")
}


# ═══════════════════════════════════════════════════════════════════════════════
# Part 2: Agent Fate Map (scatter plot)
# Each dot = one migrant agent. x = initial motivation, y = SES.
# Colour = dropout week. Black outline = survivors.
# One representative run per scenario, all 16 policy scenarios.
# ═══════════════════════════════════════════════════════════════════════════════

# Pick representative run per scenario (closest to median retention) — all 16
rep_runs_all <- sapply(POLICY_SCENARIOS, function(sc) {
  runs <- agents[agents$scenario == sc, ] %>%
    group_by(run) %>%
    summarise(ret = sum(dropped_out == FALSE) / n() * 100, .groups = "drop")
  if (nrow(runs) == 0) return(NA_integer_)
  target <- median(runs$ret)
  runs$run[which.min(abs(runs$ret - target))]
})

# Build scatter data — migrants only
scatter_list <- lapply(POLICY_SCENARIOS, function(sc) {
  rid <- rep_runs_all[sc]
  if (is.na(rid)) return(NULL)
  
  ag <- agents %>%
    filter(scenario == sc, run == rid, breed == "refugee") %>%
    mutate(
      survived = !dropped_out,
      dw = ifelse(dropped_out, pmax(dropout_week, 1L), 52L)
    )
  
  if (nrow(ag) == 0) return(NULL)
  
  # Compute retention for panel label
  ret_pct <- round(sum(ag$survived) / nrow(ag) * 100)
  ag$panel_label <- paste0(sc, "
(", ret_pct, "% migrant ret.)")
  ag$ret_for_sort <- ret_pct
  ag
})

scatter_df <- bind_rows(scatter_list)

# Sort panels by retention rate (worst top-left, best bottom-right)
panel_order <- scatter_df %>%
  distinct(panel_label, ret_for_sort) %>%
  arrange(ret_for_sort) %>%
  pull(panel_label)

scatter_df$panel_label <- factor(scatter_df$panel_label, levels = panel_order)

cat("Agent fate map data:", nrow(scatter_df), "agents across",
    length(unique(scatter_df$scenario)), "scenarios
")

p2 <- ggplot(scatter_df, aes(x = initial_motivation, y = ses)) +
  # Dropouts: coloured by week, no outline
  geom_point(
    data = filter(scatter_df, !survived),
    aes(colour = dw),
    size = 2.5, alpha = 0.8
  ) +
  # Survivors: yellow fill, black outline
  geom_point(
    data = filter(scatter_df, survived),
    aes(colour = dw),
    size = 3.0, shape = 21, fill = "gold", colour = "black", stroke = 0.8
  ) +
  scale_colour_viridis_c(
    option = "C", name = "Dropout
Week",
    limits = c(1, 52), breaks = c(1, 13, 26, 39, 52)
  ) +
  facet_wrap(~ panel_label, ncol = 4) +
  scale_x_continuous(limits = c(0.25, 0.85), breaks = seq(0.3, 0.8, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Who Survives? The Fate of Every Agent",
    subtitle = paste0(
      "Each dot = one migrant (75 per panel). Position = starting conditions. ",
      "Colour = dropout week (dark = early). Black-outlined dots = survived to week 52.
",
      "Panels sorted by retention rate (worst top-left, best bottom-right)."
    ),
    x = "Initial Motivation",
    y = "Socioeconomic Status (SES)",
    caption = paste0(
      "One representative run per scenario (closest to median retention). ",
      "Coloured dots without outline = dropped out. ",
      "Panel labels show scenario name and retention rate."
    )
  ) +
  theme_thesis +
  theme(
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    strip.text = element_text(size = 8, face = "bold"),
    panel.spacing = unit(0.5, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 8.5, colour = "grey35", lineheight = 1.2,
                                  margin = margin(b = 8)),
    plot.caption = element_text(size = 7, colour = "grey45", hjust = 0,
                                 lineheight = 1.2, margin = margin(t = 8)),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave("figures/agent_fate_map.png", p2, width = 14, height = 12, dpi = 300)
cat("Saved: figures/agent_fate_map.png
")
