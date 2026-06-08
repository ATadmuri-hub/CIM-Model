# 08_network_analysis.R — Network analysis from agent-level cross-group tie data
# Note: uses cross_group_friends (degree proxy) — full edge list not exported by model
# Run from CIM_Model/ directory
library(tidyverse)
library(patchwork)

DATA_DIR <- "data"
FIG_DIR  <- "figures"
TAB_DIR  <- "tables"
dir.create(FIG_DIR, showWarnings = FALSE)
dir.create(TAB_DIR, showWarnings = FALSE)

source("R/constants.R")

agents_df <- readRDS(file.path(DATA_DIR, "agents_df.rds"))

# SCEN_COLORS loaded from R/constants.R (centralized palette)

# theme_thesis loaded from constants.R (unified)

cat("Agent data:", nrow(agents_df), "rows,", ncol(agents_df), "columns\n")
cat("Scenarios:", paste(levels(agents_df$scenario), collapse = ", "), "\n\n")

# ============================================================
# 1. Cross-group degree summary by scenario × run
# ============================================================
degree_run <- agents_df %>%
  group_by(scenario, run) %>%
  summarise(
    mean_cross_degree  = mean(cross_group_friends, na.rm = TRUE),
    median_cross_degree= median(cross_group_friends, na.rm = TRUE),
    iso_rate           = mean(cross_group_friends == 0, na.rm = TRUE),
    high_degree_rate   = mean(cross_group_friends >= 3, na.rm = TRUE),
    n_agents           = n(),
    .groups = "drop"
  )

degree_scenario <- degree_run %>%
  group_by(scenario) %>%
  summarise(
    n_runs        = n(),
    mean_cross    = mean(mean_cross_degree),
    sd_cross      = sd(mean_cross_degree),
    mean_iso_pct  = mean(iso_rate) * 100,
    sd_iso_pct    = sd(iso_rate) * 100,
    mean_high_pct = mean(high_degree_rate) * 100,
    sd_high_pct   = sd(high_degree_rate) * 100,
    .groups = "drop"
  )

cat("=== Cross-Group Degree by Scenario (per run mean, then across-run mean) ===\n")
print(degree_scenario, n = Inf, width = 160)

# ============================================================
# 2. Mixing matrix: breed × gender × scenario
# ============================================================
mixing_df <- agents_df %>%
  group_by(scenario, breed, gender) %>%
  summarise(
    n                  = n(),
    mean_cross_friends = round(mean(cross_group_friends, na.rm = TRUE), 3),
    prop_isolated_pct  = round(mean(cross_group_friends == 0, na.rm = TRUE) * 100, 1),
    prop_high_pct      = round(mean(cross_group_friends >= 3, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

cat("\n=== Mixing Matrix: Breed x Gender (Baseline) ===\n")
print(filter(mixing_df, scenario == "Baseline"), n = Inf, width = 120)

# ============================================================
# 3. Assortativity proxies (Spearman correlations)
# ============================================================
assort_df <- agents_df %>%
  group_by(scenario) %>%
  summarise(
    rho_ses_cross    = cor(ses, cross_group_friends,
                           use = "complete.obs", method = "spearman"),
    rho_motiv_cross  = cor(initial_motivation, cross_group_friends,
                           use = "complete.obs", method = "spearman"),
    rho_dist_cross   = cor(distance_to_park, cross_group_friends,
                           use = "complete.obs", method = "spearman"),
    rho_lang_cross   = if ("language_gain" %in% names(agents_df))
                         cor(language_gain, cross_group_friends,
                             use = "complete.obs", method = "spearman")
                       else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("rho_"), ~ round(.x, 3)))

cat("\n=== Assortativity Proxies (Spearman rho with cross-group degree) ===\n")
print(assort_df, n = Inf, width = 160)

# ============================================================
# 4. Arrival cohort × breed cross-group degree
# ============================================================
cohort_net <- agents_df %>%
  filter(breed == "refugee") %>%
  group_by(scenario, arrival_cohort) %>%
  summarise(
    mean_cross   = mean(cross_group_friends, na.rm = TRUE),
    iso_rate_pct = mean(cross_group_friends == 0, na.rm = TRUE) * 100,
    n            = n(),
    .groups = "drop"
  )

cat("\n=== Cross-Group Degree by Arrival Cohort (refugees only) ===\n")
print(filter(cohort_net, scenario == "Baseline"), n = Inf)

# ============================================================
# FIG 7: Cross-group degree distribution (boxplot by scenario)
# ============================================================
fig7 <- agents_df %>%
  ggplot(aes(x = reorder(scenario, cross_group_friends, FUN = mean),
             y = cross_group_friends, fill = scenario)) +
  geom_boxplot(outlier.alpha = 0.25, outlier.size = 0.7, width = 0.7) +
  scale_fill_manual(values = SCEN_COLORS, guide = "none") +
  coord_flip() +
  labs(
    title   = "Most agents end the year with zero or one cross-group friend",
    subtitle = "All medians = 0; scenarios sorted by mean cross-group friends per agent",
    x       = NULL,
    y       = "Cross-Group Friends per Agent (week 52)",
    caption = paste0("Distribution across all agents and runs; boxes = median/IQR, ",
                     "whiskers = 1.5xIQR. N = ", format(nrow(agents_df), big.mark = ","),
                     " agents across 23 scenarios.")
  ) +
  theme_thesis

ggsave(file.path(FIG_DIR, "fig7_cross_degree_distribution.png"), fig7,
       width = 12, height = 7, dpi = 300)
cat("\nSaved: fig7_cross_degree_distribution.png\n")

# ============================================================
# FIG 8: Isolation rate by scenario and agent type
# ============================================================
iso_type <- agents_df %>%
  group_by(scenario, breed) %>%
  summarise(
    iso_rate_pct = mean(cross_group_friends == 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

fig8 <- iso_type %>%
  mutate(scenario = reorder(scenario, iso_rate_pct * (breed == "refugee"), FUN = max)) %>%
  ggplot(aes(x = scenario, y = iso_rate_pct, fill = breed)) +
  geom_col(position = position_dodge(0.75), width = 0.7, alpha = 0.9) +
  scale_fill_manual(name = NULL, values = c("refugee" = "#E84855", "local" = "#2E86AB"),
                    labels = c("refugee" = "Migrant", "local" = "Local")) +
  coord_flip() +
  labs(
    title   = "Migrant isolation exceeds local isolation in every scenario",
    x       = NULL,
    y       = "Isolation Rate (%)",
    caption = "Proportion of agents with zero cross-group friends at week 52 (all runs pooled)"
  ) +
  theme_thesis

ggsave(file.path(FIG_DIR, "fig8_isolation_rate.png"), fig8,
       width = 12, height = 6, dpi = 300)
cat("Saved: fig8_isolation_rate.png\n")

# ============================================================
# Save outputs
# ============================================================
saveRDS(degree_scenario, file.path(DATA_DIR, "network_degree_scenario.rds"))
saveRDS(degree_run,      file.path(DATA_DIR, "network_degree_run.rds"))
saveRDS(mixing_df,       file.path(DATA_DIR, "network_mixing.rds"))
saveRDS(assort_df,       file.path(DATA_DIR, "network_assortativity.rds"))
saveRDS(cohort_net,      file.path(DATA_DIR, "network_cohort.rds"))

write_csv(degree_scenario, file.path(TAB_DIR, "table_network_degree.csv"))
write_csv(mixing_df,       file.path(TAB_DIR, "table_network_mixing.csv"))
write_csv(assort_df,       file.path(TAB_DIR, "table_network_assortativity.csv"))

cat("\nNetwork analysis complete.\n")
