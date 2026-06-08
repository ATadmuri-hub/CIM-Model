# R/21_r_naught.R
#
# Item 3 (Phase 2 Session 3): R_0 derivation from observed network statistics.
#
# Reads per-run friendship edge files exported from NetLogo BehaviorSpace
# (data/<scenario>/CIM_edges_*.csv) and computes per-scenario means of:
#   - <k>     : mean degree of the friendship network at week 52
#   - <k^2>   : second moment of degree distribution (heterogeneity)
#   - <s>     : mean tie strength
#   - n nodes, n edges per run
#
# Then computes two interpretations of R_0 for the baseline parameters
# (beta = 0.08, gamma = 0.018) under the SIS analogy:
#
#   1. R_0_bare        = beta / gamma                        (existing thesis claim, ~4.4)
#   2. R_0_normalized  = (beta * <s>) / gamma                (accounts for model's normalization
#                                                             of peer influence by mean tie strength)
#
# The model's peer-influence formula (Eq. in 3.5 Submodel Formulas) is
#     dm_i = (mean_motivation_active - m_i) * mean_tie_strength * beta
# which means the actual restoring rate per tick is (s_bar * beta), not beta alone.
# The R_0_normalized version exposes this honestly.
#
# Output:
#   outputs/r_naught_per_scenario.rds  -- list, one entry per scenario
#   stdout summary table
#
# Author: Abdullah Tadmuri, May 2026
# Item ID: T2.3 (Phase 2 Session 3)

suppressPackageStartupMessages({
  library(igraph)
  library(dplyr)
})

DATA_DIR <- "data"
OUT_FILE <- "outputs/r_naught_per_scenario.rds"

# Model-baseline parameters (from CIM_v6_4.nlogo Globals + Parameter Table)
BETA  <- 0.08    # peer-influence coefficient
GAMMA <- 0.018   # motivation decay rate per week

# Helper: per-run network statistics from one CIM_edges_*.csv file
edge_file_stats <- function(path) {
  # Edge files may contain duplicate-appended blocks (BehaviorSpace re-runs write
  # the header + rows on each append); take the LAST complete block, matching the
  # last-block dedup logic in 01_load_data.R. Plain read.csv() on a multi-block file
  # triples the edges and ingests embedded header rows (tie_strength -> NA), which
  # corrupts <k> and <s>.
  ln <- tryCatch(readLines(path, warn = FALSE), error = function(err) NULL)
  if (is.null(ln) || length(ln) == 0) return(NULL)
  hdr   <- ln[1]
  hi    <- which(ln == hdr)
  start <- hi[length(hi)] + 1
  if (start > length(ln)) return(NULL)
  e <- tryCatch(
    read.csv(text = paste(c(hdr, ln[start:length(ln)]), collapse = "\n"),
             stringsAsFactors = FALSE),
    error = function(err) NULL)
  if (is.null(e) || nrow(e) == 0) return(NULL)
  e <- e[!is.na(e$end1_id) & !is.na(e$end2_id), ]
  if (nrow(e) == 0) return(NULL)
  e$tie_strength <- as.numeric(e$tie_strength)
  # Build undirected graph from end1_id, end2_id
  edge_list <- as.matrix(e[, c("end1_id", "end2_id")])
  g <- graph_from_edgelist(edge_list, directed = FALSE)
  deg <- degree(g)
  list(
    n_nodes  = vcount(g),
    n_edges  = ecount(g),
    mean_k   = mean(deg),
    mean_k2  = mean(deg^2),
    mean_s   = mean(e$tie_strength)
  )
}

# Helper: aggregate per-scenario stats across all runs
scenario_stats <- function(scenario_dir) {
  files <- list.files(scenario_dir, pattern = "^CIM_edges_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)

  per_run <- lapply(files, edge_file_stats)
  per_run <- per_run[!sapply(per_run, is.null)]
  if (length(per_run) == 0) return(NULL)

  list(
    scenario     = basename(scenario_dir),
    n_runs       = length(per_run),
    mean_n_nodes = mean(sapply(per_run, function(x) x$n_nodes)),
    mean_n_edges = mean(sapply(per_run, function(x) x$n_edges)),
    mean_k       = mean(sapply(per_run, function(x) x$mean_k)),
    sd_k         = sd  (sapply(per_run, function(x) x$mean_k)),
    mean_k2      = mean(sapply(per_run, function(x) x$mean_k2)),
    mean_s       = mean(sapply(per_run, function(x) x$mean_s)),
    sd_s         = sd  (sapply(per_run, function(x) x$mean_s))
  )
}

# Discover scenario folders that have edge files
scenario_dirs <- list.dirs(DATA_DIR, recursive = FALSE)
scenario_dirs <- scenario_dirs[
  sapply(scenario_dirs, function(d) length(list.files(d, "^CIM_edges_.*\\.csv$")) > 0)
]
cat(sprintf("Found %d scenario folders with edge files.\n", length(scenario_dirs)))

# Process each scenario
results <- list()
for (sd in scenario_dirs) {
  cat(sprintf("  Processing %s ... ", basename(sd)))
  s <- scenario_stats(sd)
  if (!is.null(s)) {
    results[[s$scenario]] <- s
    cat(sprintf("n_runs=%d, <k>=%.2f, <k^2>=%.2f, <s>=%.3f\n",
                s$n_runs, s$mean_k, s$mean_k2, s$mean_s))
  } else {
    cat("(skipped, no usable runs)\n")
  }
}

# Save raw results
dir.create("outputs", showWarnings = FALSE)
saveRDS(results, OUT_FILE)
cat(sprintf("\nSaved %d scenarios to %s\n", length(results), OUT_FILE))

# Compute R_0 interpretations for Baseline specifically (the headline number)
b <- results[["Baseline"]]
if (!is.null(b)) {
  R0_bare       <- BETA / GAMMA
  R0_normalized <- (BETA * b$mean_s) / GAMMA
  R0_network    <- (BETA / GAMMA) * (b$mean_k2 - b$mean_k) / b$mean_k

  cat("\n=== R_0 INTERPRETATIONS FOR BASELINE ===\n")
  cat(sprintf("Parameters: beta = %.3f, gamma = %.3f\n", BETA, GAMMA))
  cat(sprintf("Network observables: <k> = %.2f, <k^2> = %.2f, <s> = %.3f (N runs = %d)\n",
              b$mean_k, b$mean_k2, b$mean_s, b$n_runs))
  cat("\n")
  cat(sprintf("  (1) R_0_bare       = beta / gamma                 = %.3f\n", R0_bare))
  cat("      [existing thesis claim; ignores the model's normalization of peer influence]\n\n")
  cat(sprintf("  (2) R_0_normalized = (beta * <s>) / gamma         = %.3f\n", R0_normalized))
  cat("      [accounts for the model's mean-tie-strength normalization in the peer-influence formula]\n\n")
  cat(sprintf("  (3) R_0_network    = (beta/gamma) * (<k^2>-<k>)/<k> = %.3f\n", R0_network))
  cat("      [SNA spreading-lecture formula, IF beta were per-neighbour (not the case in CIM)]\n\n")
}

# Cross-scenario summary table
cat("\n=== PER-SCENARIO NETWORK STATISTICS (mean across runs) ===\n")
cat(sprintf("%-30s %6s %8s %8s %8s\n", "scenario", "n_runs", "<k>", "<k^2>", "<s>"))
cat(strrep("-", 65), "\n", sep = "")
for (nm in names(results)) {
  r <- results[[nm]]
  cat(sprintf("%-30s %6d %8.2f %8.2f %8.3f\n",
              nm, r$n_runs, r$mean_k, r$mean_k2, r$mean_s))
}
