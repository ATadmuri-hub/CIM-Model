# R/25_link_prediction.R
#
# Item 11 (Phase 3): Link prediction analysis to validate network module.
#
# Method: Static link prediction with edge masking. For each run:
#   1. Load final-state edges (week 52)
#   2. Hold out 20% as test set, build graph from remaining 80%
#   3. Compute 4 predictors per candidate pair (Adamic-Adar, Jaccard, common
#      neighbors, preferential attachment) using igraph similarity functions
#   4. Sample equal number of non-edges as negative class
#   5. Compute AUC per predictor
#
# Interpretation:
#   - AUC > 0.85: strong learnable structure beyond group assignment
#   - AUC ~ 0.7:  mostly group-driven structure
#   - AUC ~ 0.5:  random tie placement
#
# Note: Edges only form within training groups (lines 1275-1280 of NetLogo).
#   Predictors will naturally score within-group dyads higher because they
#   share neighbors trivially via group co-membership. High AUC is therefore
#   the EXPECTED outcome; the magnitude tells us about dyadic predictability
#   beyond raw group structure.
#
# Scenarios tested (span different network dynamics):
#   - Baseline                   (default ~48 edges/run)
#   - Suboptimal Composition     (sparse ~19 edges/run)
#   - BuddyProgram               (denser ~55 edges/run, mandated cross-group)
#   - High SES Heterogeneity     (~58 edges/run, different mixing)
#
# Outputs:
#   tables/table_link_prediction.csv  -- per-run AUC + per-scenario summary
#   figures/fig_link_prediction.png   -- AUC distribution per scenario per predictor
#   outputs/link_prediction.rds       -- full results object
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T3.11 (Phase 3 Item 11)

suppressPackageStartupMessages({
  library(dplyr)
  library(igraph)
  library(pROC)
  library(ggplot2)
  library(tidyr)
})

DATA_DIR <- "data"
TAB_DIR  <- "tables"
FIG_DIR  <- "figures"
OUT_DIR  <- "outputs"

dir.create(TAB_DIR, showWarnings = FALSE)
dir.create(FIG_DIR, showWarnings = FALSE)
dir.create(OUT_DIR, showWarnings = FALSE)

set.seed(42)
TEST_FRAC <- 0.20

SCENARIOS <- c("Baseline", "Suboptimal Composition", "BuddyProgram",
               "High SES Heterogeneity")

# --- Per-run link prediction analysis ----------------------------------------
analyze_run <- function(scenario, run_id) {
  fp <- file.path(DATA_DIR, scenario,
                  sprintf("CIM_edges_%s_%d.csv", scenario, run_id))
  if (!file.exists(fp)) return(NULL)

  edges <- tryCatch(read.csv(fp, stringsAsFactors = FALSE),
                    error = function(e) NULL)
  if (is.null(edges) || nrow(edges) < 25) return(NULL)

  # Some runs may have repeated/duplicate-block writes; keep last block only
  if (anyDuplicated(edges)) edges <- unique(edges)

  # Build the full (undirected) graph from edges, using participant IDs
  edges$end1_id <- as.character(edges$end1_id)
  edges$end2_id <- as.character(edges$end2_id)
  g_full <- graph_from_data_frame(edges[, c("end1_id", "end2_id")],
                                  directed = FALSE)
  vs <- V(g_full)$name
  V_count <- length(vs)
  if (V_count < 6) return(NULL)

  # Train/test split: hold out 20% of edges as test positives
  n_edges <- nrow(edges)
  n_test  <- max(5, round(n_edges * TEST_FRAC))
  if (n_edges - n_test < 5) return(NULL)
  test_idx <- sample(n_edges, n_test)
  train_pairs <- edges[-test_idx, c("end1_id", "end2_id")]

  # Build train graph with all vertices from full graph (so indices align)
  g_train <- make_empty_graph(n = V_count, directed = FALSE)
  V(g_train)$name <- vs
  if (nrow(train_pairs) > 0) {
    train_idx_u <- match(train_pairs$end1_id, vs)
    train_idx_v <- match(train_pairs$end2_id, vs)
    edge_seq <- as.vector(rbind(train_idx_u, train_idx_v))
    g_train <- add_edges(g_train, edge_seq)
  }

  # Predictor matrices on g_train (V_count x V_count)
  sim_aa <- similarity(g_train, method = "invlogweighted")     # Adamic-Adar
  sim_jc <- similarity(g_train, method = "jaccard")            # Jaccard
  adj    <- as_adjacency_matrix(g_train, sparse = FALSE)       # 0/1 matrix
  cn_mat <- adj %*% adj                                        # common neighbors counts
  diag(cn_mat) <- 0
  degs   <- rowSums(adj)
  pa_mat <- outer(degs, degs)                                   # preferential attachment

  # Positive test pairs (held-out edges) -> indices in [1, V_count]
  pos_pairs <- edges[test_idx, c("end1_id", "end2_id")]
  pos_idx <- cbind(match(pos_pairs$end1_id, vs),
                   match(pos_pairs$end2_id, vs))

  # Negative samples: random non-edges in g_full (upper triangle to avoid duplicates)
  adj_full <- as_adjacency_matrix(g_full, sparse = FALSE)
  upper_mask <- upper.tri(adj_full)
  non_edge_lin <- which(adj_full == 0 & upper_mask)
  if (length(non_edge_lin) < n_test) return(NULL)
  neg_lin <- sample(non_edge_lin, n_test)
  neg_idx <- cbind(((neg_lin - 1) %% V_count) + 1,   # row
                   ((neg_lin - 1) %/% V_count) + 1)   # col

  test_idx_pairs <- rbind(pos_idx, neg_idx)
  labels <- c(rep(1L, nrow(pos_idx)), rep(0L, nrow(neg_idx)))

  scores_aa <- sim_aa[test_idx_pairs]
  scores_jc <- sim_jc[test_idx_pairs]
  scores_cn <- cn_mat[test_idx_pairs]
  scores_pa <- pa_mat[test_idx_pairs]

  compute_auc <- function(scores, labels) {
    valid <- !is.na(scores)
    if (sum(valid) < 4 || length(unique(labels[valid])) < 2) return(NA_real_)
    if (sd(scores[valid]) == 0) return(0.5)  # constant predictor → chance
    suppressWarnings(as.numeric(pROC::roc(
      labels[valid], scores[valid], quiet = TRUE, direction = "<"
    )$auc))
  }

  data.frame(
    scenario = scenario,
    run      = run_id,
    n_edges  = n_edges,
    n_vertices = V_count,
    n_test   = n_test,
    auc_aa   = compute_auc(scores_aa, labels),
    auc_cn   = compute_auc(scores_cn, labels),
    auc_jc   = compute_auc(scores_jc, labels),
    auc_pa   = compute_auc(scores_pa, labels)
  )
}

# --- Run on all scenarios + all runs -----------------------------------------
cat("=== Link prediction analysis ===\n")
all_results <- list()
for (scen in SCENARIOS) {
  cat(sprintf("\n--- %s ---\n", scen))
  files <- list.files(file.path(DATA_DIR, scen),
                      pattern = "^CIM_edges_.*\\.csv$")
  if (length(files) == 0) {
    cat("  No edge files found, skipping\n")
    next
  }
  run_ids <- as.integer(sub(".*_(\\d+)\\.csv$", "\\1", files))
  cat(sprintf("  %d edge files, processing...\n", length(run_ids)))

  scen_results <- lapply(run_ids, function(r) {
    tryCatch(analyze_run(scen, r), error = function(e) NULL)
  })
  scen_results <- scen_results[!sapply(scen_results, is.null)]
  cat(sprintf("  %d runs successfully analyzed\n", length(scen_results)))
  all_results[[scen]] <- bind_rows(scen_results)
}

results_df <- bind_rows(all_results)
cat(sprintf("\nTotal runs analyzed across all scenarios: %d\n", nrow(results_df)))

# --- Per-scenario summary ----------------------------------------------------
cat("\n=== Per-scenario AUC summary (mean +/- 95% CI) ===\n")
summary_df <- results_df %>%
  group_by(scenario) %>%
  summarise(
    n_runs   = n(),
    auc_aa_mean = mean(auc_aa, na.rm = TRUE),
    auc_aa_se   = sd(auc_aa, na.rm = TRUE) / sqrt(sum(!is.na(auc_aa))),
    auc_cn_mean = mean(auc_cn, na.rm = TRUE),
    auc_cn_se   = sd(auc_cn, na.rm = TRUE) / sqrt(sum(!is.na(auc_cn))),
    auc_jc_mean = mean(auc_jc, na.rm = TRUE),
    auc_jc_se   = sd(auc_jc, na.rm = TRUE) / sqrt(sum(!is.na(auc_jc))),
    auc_pa_mean = mean(auc_pa, na.rm = TRUE),
    auc_pa_se   = sd(auc_pa, na.rm = TRUE) / sqrt(sum(!is.na(auc_pa))),
    .groups = "drop"
  )
print(summary_df)

# --- One-sample t-test against null AUC = 0.5 -------------------------------
cat("\n=== One-sample t-tests vs null AUC = 0.5 ===\n")
t_tests <- expand.grid(
  scenario = SCENARIOS,
  predictor = c("auc_aa", "auc_cn", "auc_jc", "auc_pa"),
  stringsAsFactors = FALSE
) %>% rowwise() %>% mutate(
  d = list(results_df[[predictor]][results_df$scenario == scenario]),
  d = list(d[!is.na(d)]),
  n = length(d),
  mean_auc = mean(d),
  t_stat = (mean(d) - 0.5) / (sd(d) / sqrt(length(d))),
  p_value = if (length(d) > 1) t.test(d, mu = 0.5)$p.value else NA_real_
) %>% ungroup() %>% select(-d)

print(t_tests)

# Save tables
write.csv(results_df, file.path(TAB_DIR, "table_link_prediction_runs.csv"), row.names = FALSE)
write.csv(summary_df, file.path(TAB_DIR, "table_link_prediction.csv"), row.names = FALSE)
write.csv(t_tests, file.path(TAB_DIR, "table_link_prediction_tests.csv"), row.names = FALSE)

# --- Visualization: AUC distribution per scenario per predictor --------------
cat("\n=== Generating figure ===\n")

plot_data <- results_df %>%
  pivot_longer(cols = starts_with("auc_"),
               names_to = "predictor", values_to = "auc") %>%
  mutate(
    predictor = recode(predictor,
                       auc_aa = "Adamic-Adar",
                       auc_cn = "Common neighbors",
                       auc_jc = "Jaccard",
                       auc_pa = "Preferential attachment"),
    scenario = factor(scenario, levels = SCENARIOS)
  ) %>%
  filter(!is.na(auc))

# Sort scenarios globally by overall median AUC (across all predictors) for the x-axis
global_order <- plot_data %>%
  group_by(scenario) %>%
  summarise(med = median(auc, na.rm = TRUE), .groups = "drop") %>%
  arrange(med) %>%
  pull(scenario)
plot_data$scenario <- factor(plot_data$scenario, levels = global_order)

p <- ggplot(plot_data, aes(x = scenario, y = auc, fill = scenario)) +
  geom_boxplot(alpha = 0.85, color = "gray30",
               outlier.size = 0.4, outlier.alpha = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  facet_wrap(~ predictor, ncol = 2) +
  scale_fill_viridis_d(end = 0.85) +
  labs(
    x = NULL, y = "AUC", fill = NULL,
    title = "Link prediction AUC across scenarios and predictors",
    subtitle = "Per-run AUC by predictor; boxplots sorted by median within each panel. Dashed line = chance (0.5)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    strip.text    = element_text(face = "bold", size = 10),
    axis.text.x   = element_text(angle = 25, hjust = 1, size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIG_DIR, "fig_link_prediction.png"),
       p, width = 11, height = 7, dpi = 200, bg = "white")
cat(sprintf("Figure saved: %s\n", file.path(FIG_DIR, "fig_link_prediction.png")))

# --- Save full results -------------------------------------------------------
saveRDS(list(
  results_df = results_df,
  summary_df = summary_df,
  t_tests    = t_tests,
  metadata   = list(
    date = Sys.time(),
    scenarios = SCENARIOS,
    test_frac = TEST_FRAC,
    seed = 42
  )
), file.path(OUT_DIR, "link_prediction.rds"))

cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_link_prediction.csv")))
cat(sprintf("Saved: %s\n", file.path(TAB_DIR, "table_link_prediction_tests.csv")))
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "link_prediction.rds")))
cat("\n=== Item 11 link prediction analysis complete ===\n")
