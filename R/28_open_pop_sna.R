# R/28_open_pop_sna.R
#
# Item 12 follow-up (Phase 3 verification round): Multi-metric SNA comparison
# of Baseline (closed cohort) vs OpenPopulation (open cohort).
#
# Computes 5 graph-theoretic metrics per run for both scenarios:
#   1. Louvain modularity Q
#   2. Global clustering coefficient
#   3. Giant component fraction
#   4. Newman breed (refugee/local) assortativity
#   5. Mean tie strength (cross-group vs within-group)
#
# Then runs Welch t-tests of OpenPopulation vs Baseline on each metric and
# reports Cohen's d. Goal: address the review
# objection that the Item 12 "qualitative network structure preserves" claim
# rests on only one metric (cross-group tie ratio).
#
# Outputs:
#   tables/table_open_pop_sna.csv    -- per-metric Welch + Cohen's d
#   outputs/open_pop_sna.rds         -- full results object
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T3.12-followup (Phase 3 verification round)

suppressPackageStartupMessages({
  library(igraph)
  library(dplyr)
  library(effsize)
})

DATA_DIR <- "data"
TAB_DIR  <- "tables"
OUT_DIR  <- "outputs"

dir.create(TAB_DIR, showWarnings = FALSE)
dir.create(OUT_DIR, showWarnings = FALSE)

# --- Per-run graph metrics ---------------------------------------------------
analyze_run_graph <- function(scenario, run_id) {
  fp <- file.path(DATA_DIR, scenario,
                  sprintf("CIM_edges_%s_%d.csv", scenario, run_id))
  if (!file.exists(fp)) return(NULL)

  edges <- tryCatch(read.csv(fp, stringsAsFactors = FALSE),
                    error = function(e) NULL)
  if (is.null(edges) || nrow(edges) < 5) return(NULL)
  if (anyDuplicated(edges)) edges <- unique(edges)

  edges$end1_id <- as.character(edges$end1_id)
  edges$end2_id <- as.character(edges$end2_id)

  g <- graph_from_data_frame(edges[, c("end1_id", "end2_id")], directed = FALSE)
  E(g)$weight        <- edges$tie_strength
  E(g)$is_cross      <- edges$is_cross_group == "true"
  V(g)$breed         <- NA_character_
  for (i in seq_len(nrow(edges))) {
    V(g)[edges$end1_id[i]]$breed <- edges$end1_breed[i]
    V(g)[edges$end2_id[i]]$breed <- edges$end2_breed[i]
  }
  if (any(is.na(V(g)$breed))) return(NULL)

  # 1. Louvain modularity
  modularity_q <- tryCatch({
    cluster_louvain(g)$modularity[length(cluster_louvain(g)$modularity)]
  }, error = function(e) NA_real_)

  # 2. Global clustering coefficient
  global_clustering <- transitivity(g, type = "global")

  # 3. Giant component fraction
  components_g <- components(g)
  giant_fraction <- max(components_g$csize) / vcount(g)

  # 4. Newman breed assortativity
  V(g)$breed_id <- as.integer(factor(V(g)$breed))
  breed_assort <- assortativity_nominal(g, V(g)$breed_id)

  # 5. Mean tie strength split by cross-group status
  mean_tie_cross  <- mean(E(g)$weight[E(g)$is_cross], na.rm = TRUE)
  mean_tie_within <- mean(E(g)$weight[!E(g)$is_cross], na.rm = TRUE)

  data.frame(
    scenario           = scenario,
    run                = run_id,
    n_nodes            = vcount(g),
    n_edges            = ecount(g),
    modularity_q       = modularity_q,
    global_clustering  = global_clustering,
    giant_fraction     = giant_fraction,
    breed_assortativity = breed_assort,
    mean_tie_cross     = mean_tie_cross,
    mean_tie_within    = mean_tie_within
  )
}

cat("=== Multi-metric SNA: Baseline vs OpenPopulation ===\n")
SCENARIOS <- c("Baseline", "OpenPopulation")
all_runs <- list()
for (scen in SCENARIOS) {
  cat(sprintf("\n--- %s ---\n", scen))
  files <- list.files(file.path(DATA_DIR, scen),
                      pattern = "^CIM_edges_.*\\.csv$")
  run_ids <- as.integer(sub(".*_(\\d+)\\.csv$", "\\1", files))
  cat(sprintf("  %d edge files; processing...\n", length(run_ids)))
  scen_runs <- lapply(run_ids, function(r) {
    tryCatch(analyze_run_graph(scen, r), error = function(e) NULL)
  })
  scen_runs <- scen_runs[!sapply(scen_runs, is.null)]
  cat(sprintf("  %d runs successfully analysed\n", length(scen_runs)))
  all_runs[[scen]] <- bind_rows(scen_runs)
}

per_run <- bind_rows(all_runs)

cat("\n=== Per-scenario summary ===\n")
metrics <- c("modularity_q", "global_clustering", "giant_fraction",
             "breed_assortativity", "mean_tie_cross", "mean_tie_within")

summary_df <- per_run %>%
  group_by(scenario) %>%
  summarise(
    n           = n(),
    across(all_of(metrics),
           list(mean = ~ mean(.x, na.rm = TRUE),
                se   = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))))
  )
print(summary_df)

# --- Welch t-tests + Cohen's d (OpenPopulation vs Baseline) ------------------
cat("\n=== Welch t-tests + Cohen's d (OpenPopulation vs Baseline) ===\n")
test_results <- lapply(metrics, function(m) {
  x_open <- na.omit(all_runs[["OpenPopulation"]][[m]])
  x_base <- na.omit(all_runs[["Baseline"]][[m]])
  if (length(x_open) < 2 || length(x_base) < 2) return(NULL)
  t_res <- t.test(x_open, x_base, var.equal = FALSE)
  d_res <- cohen.d(x_open, x_base)
  data.frame(
    metric        = m,
    n_baseline    = length(x_base),
    n_openpop     = length(x_open),
    mean_baseline = mean(x_base),
    mean_openpop  = mean(x_open),
    diff          = mean(x_open) - mean(x_base),
    cohens_d      = d_res$estimate,
    d_magnitude   = as.character(d_res$magnitude),
    t_stat        = t_res$statistic,
    p_raw         = t_res$p.value
  )
})
test_df <- bind_rows(test_results)
test_df$p_adj_holm <- p.adjust(test_df$p_raw, method = "holm")
test_df$sig_holm   <- test_df$p_adj_holm < 0.05
print(test_df)

write.csv(test_df, file.path(TAB_DIR, "table_open_pop_sna.csv"), row.names = FALSE)

saveRDS(list(
  per_run    = per_run,
  summary_df = summary_df,
  test_df    = test_df,
  metadata   = list(date = Sys.time(), scenarios = SCENARIOS, metrics = metrics)
), file.path(OUT_DIR, "open_pop_sna.rds"))

cat(sprintf("\nSaved: %s\n", file.path(TAB_DIR, "table_open_pop_sna.csv")))
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "open_pop_sna.rds")))
cat("\n=== Item 12 multi-metric SNA verification complete ===\n")
