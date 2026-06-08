# CIM v6.4 — Formal Social Network Analysis from Edge Files
# Computes: modularity Q, betweenness centrality, clustering coefficient,
#           giant component fraction, Newman assortativity, weak tie analysis
# References: Bojanowski & Corten (2014), Everett & Valente (2016),
#             Centola (2010), Granovetter (1973)

.libPaths(c(.libPaths(), "/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library"))
library(tidyverse)
library(igraph)
source("R/constants.R")

DATA_DIR <- "data"
TAB_DIR  <- "tables"
FIG_DIR  <- "figures"

build_graph <- function(edge_file) {
  edges <- tryCatch(read_csv(edge_file, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(edges) || nrow(edges) == 0) return(NULL)

  vertex_info <- bind_rows(
    edges %>% select(id = end1_id, breed = end1_breed),
    edges %>% select(id = end2_id, breed = end2_breed)
  ) %>% distinct(id, .keep_all = TRUE)

  g <- graph_from_data_frame(
    edges %>% select(end1_id, end2_id),
    directed = FALSE,
    vertices = vertex_info %>% rename(name = id)
  )
  E(g)$weight <- edges$tie_strength
  E(g)$is_cross <- as.logical(edges$is_cross_group)
  g
}

compute_metrics <- function(g) {
  if (is.null(g) || vcount(g) < 3 || ecount(g) < 2) {
    return(tibble(
      modularity_q = NA_real_, mean_betweenness = NA_real_,
      clustering_coef = NA_real_, giant_component_frac = NA_real_,
      assortativity_breed = NA_real_,
      mean_cross_strength = NA_real_, mean_within_strength = NA_real_,
      n_vertices = ifelse(is.null(g), 0L, vcount(g)),
      n_edges = ifelse(is.null(g), 0L, ecount(g))
    ))
  }

  comm <- tryCatch(cluster_louvain(g, weights = E(g)$weight), error = function(e) NULL)
  mod_q <- if (!is.null(comm)) modularity(comm) else NA_real_

  btw <- betweenness(g, normalized = TRUE)

  cc <- transitivity(g, type = "global")

  comp <- components(g)
  gc_frac <- max(comp$csize) / vcount(g)

  breed_fac <- as.integer(factor(V(g)$breed))
  assort <- tryCatch(assortativity_nominal(g, breed_fac, directed = FALSE), error = function(e) NA_real_)

  cross_mask <- E(g)$is_cross
  mean_cross <- if (any(cross_mask, na.rm = TRUE)) mean(E(g)$weight[cross_mask], na.rm = TRUE) else NA_real_
  mean_within <- if (any(!cross_mask, na.rm = TRUE)) mean(E(g)$weight[!cross_mask], na.rm = TRUE) else NA_real_

  tibble(
    modularity_q = mod_q,
    mean_betweenness = mean(btw, na.rm = TRUE),
    clustering_coef = cc,
    giant_component_frac = gc_frac,
    assortativity_breed = assort,
    mean_cross_strength = mean_cross,
    mean_within_strength = mean_within,
    n_vertices = vcount(g),
    n_edges = ecount(g)
  )
}

cat("=== SNA Graph Analysis ===\n")
cat("Processing edge files for", length(POLICY_SCENARIOS), "scenarios...\n\n")

results_list <- list()
for (sc in POLICY_SCENARIOS) {
  sc_dir <- file.path(DATA_DIR, sc)
  edge_files <- list.files(sc_dir, pattern = "CIM_edges_", full.names = TRUE)

  if (length(edge_files) == 0) {
    cat(sprintf("  %-25s: no edge files, skipping\n", sc))
    next
  }

  cat(sprintf("  %-25s: %d files... ", sc, length(edge_files)))

  sc_results <- map_dfr(seq_along(edge_files), function(i) {
    g <- build_graph(edge_files[i])
    m <- compute_metrics(g)
    m$run <- i
    m
  })
  sc_results$scenario <- sc
  results_list[[sc]] <- sc_results
  cat(sprintf("done (%.1f mean edges/run)\n", mean(sc_results$n_edges, na.rm = TRUE)))
}

sna_df <- bind_rows(results_list)
saveRDS(sna_df, file.path(DATA_DIR, "sna_graph_metrics.rds"))

sna_summary <- sna_df %>%
  group_by(scenario) %>%
  summarise(
    n_runs = n(),
    mod_q_mean = mean(modularity_q, na.rm = TRUE),
    mod_q_sd = sd(modularity_q, na.rm = TRUE),
    btw_mean = mean(mean_betweenness, na.rm = TRUE),
    btw_sd = sd(mean_betweenness, na.rm = TRUE),
    cc_mean = mean(clustering_coef, na.rm = TRUE),
    cc_sd = sd(clustering_coef, na.rm = TRUE),
    gc_frac_mean = mean(giant_component_frac, na.rm = TRUE),
    gc_frac_sd = sd(giant_component_frac, na.rm = TRUE),
    assort_mean = mean(assortativity_breed, na.rm = TRUE),
    assort_sd = sd(assortativity_breed, na.rm = TRUE),
    cross_str_mean = mean(mean_cross_strength, na.rm = TRUE),
    cross_str_sd = sd(mean_cross_strength, na.rm = TRUE),
    within_str_mean = mean(mean_within_strength, na.rm = TRUE),
    within_str_sd = sd(mean_within_strength, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(sna_summary, file.path(TAB_DIR, "table_sna_metrics.csv"))
cat("\nSaved: tables/table_sna_metrics.csv\n")
cat("Saved: data/sna_graph_metrics.rds\n")

cat("\n=== Summary Table ===\n")
print(sna_summary %>% select(scenario, n_runs, mod_q_mean, btw_mean, cc_mean, gc_frac_mean, assort_mean, cross_str_mean, within_str_mean), n = 20)

# Weak tie test: paired comparison cross vs within
cat("\n=== Weak Tie Test (Granovetter) ===\n")
tie_test <- sna_df %>%
  filter(!is.na(mean_cross_strength) & !is.na(mean_within_strength)) %>%
  group_by(scenario) %>%
  summarise(
    cross_mean = mean(mean_cross_strength, na.rm = TRUE),
    within_mean = mean(mean_within_strength, na.rm = TRUE),
    diff = cross_mean - within_mean,
    t_stat = tryCatch(t.test(mean_cross_strength, mean_within_strength, paired = TRUE)$statistic, error = function(e) NA),
    p_value = tryCatch(t.test(mean_cross_strength, mean_within_strength, paired = TRUE)$p.value, error = function(e) NA),
    .groups = "drop"
  )
print(tie_test, n = 20)
write_csv(tie_test, file.path(TAB_DIR, "table_sna_weak_ties.csv"))

# Figures
cat("\n=== Generating figures ===\n")

# Fig: Modularity by scenario
fig_mod <- sna_df %>%
  mutate(scenario = factor(scenario, levels = sna_summary$scenario[order(sna_summary$mod_q_mean)])) %>%
  ggplot(aes(x = scenario, y = modularity_q)) +
  geom_boxplot(fill = "#4C72B0", alpha = 0.85, outlier.size = 0.5, outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "Network modularity by scenario",
       subtitle = "Lower Q = more integrated network structure (Louvain community detection)",
       x = NULL, y = "Modularity Q") +
  theme_thesis + theme(legend.position = "none")
ggsave(file.path(FIG_DIR, "fig_sna_modularity.png"), fig_mod, width = 10, height = 6, dpi = 300)
cat("Saved: fig_sna_modularity.png\n")

# Fig: Clustering coefficient by scenario
fig_cc <- sna_df %>%
  mutate(scenario = factor(scenario, levels = sna_summary$scenario[order(sna_summary$cc_mean)])) %>%
  ggplot(aes(x = scenario, y = clustering_coef, fill = scenario)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5) +
  scale_fill_manual(values = SCEN_COLORS) +
  coord_flip() +
  labs(title = "Network clustering coefficient by scenario",
       subtitle = "Higher clustering = stronger triadic closure (Centola 2010 mechanism)",
       x = NULL, y = "Global clustering coefficient") +
  theme_thesis + theme(legend.position = "none")
ggsave(file.path(FIG_DIR, "fig_sna_clustering.png"), fig_cc, width = 10, height = 6, dpi = 300)
cat("Saved: fig_sna_clustering.png\n")

# Fig: Weak ties — cross vs within strength
fig_ties <- tie_test %>%
  pivot_longer(cols = c(cross_mean, within_mean), names_to = "type", values_to = "strength") %>%
  mutate(type = recode(type, cross_mean = "Cross-group", within_mean = "Within-group"),
         scenario = reorder(scenario, strength)) %>%
  ggplot(aes(x = scenario, y = strength, fill = type)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = c("Cross-group" = "#E84855", "Within-group" = "#2E86AB")) +
  coord_flip() +
  labs(title = "Cross-group ties are weaker than within-group ties in every scenario",
       subtitle = "Mean tie strength at week 52 (Granovetter 1973: weak ties bridge communities)",
       x = NULL, y = "Mean tie strength", fill = NULL) +
  theme_thesis + theme(legend.position = "bottom")
ggsave(file.path(FIG_DIR, "fig_sna_weak_ties.png"), fig_ties, width = 10, height = 6, dpi = 300)
cat("Saved: fig_sna_weak_ties.png\n")

cat("\n=== SNA Graph Analysis Complete ===\n")
