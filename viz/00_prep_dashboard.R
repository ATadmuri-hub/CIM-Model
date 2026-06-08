#!/usr/bin/env Rscript
# viz/00_prep_dashboard.R
# Prepares compact JSON artifacts for the external replay dashboard from the
# VERIFIED pipeline outputs. Reads only exported data (no model run, no model change).
# Handles the duplicate-append artifact by taking the LAST block of each per-run CSV.

suppressMessages({library(jsonlite); library(dplyr); library(tidyr)})

if (nzchar(Sys.getenv("CIM_ROOT"))) setwd(Sys.getenv("CIM_ROOT"))  # otherwise run from the repo root
dir.create("viz/data", showWarnings = FALSE, recursive = TRUE)

# ---- last-block reader (handles duplicate-append) -------------------------
read_last_block <- function(path, header_regex) {
  ln <- readLines(path, warn = FALSE)
  h  <- grep(header_regex, ln)
  if (length(h) == 0) return(NULL)
  start <- tail(h, 1)
  blk <- ln[start:length(ln)]
  read.csv(text = paste(blk, collapse = "\n"), stringsAsFactors = FALSE,
           check.names = FALSE)
}

# ---- 1. HERO ranking (already computed + verified) ------------------------
rank <- read.csv("outputs/istanbul_ranking_precomputed.csv", check.names = FALSE)
names(rank) <- tolower(names(rank))
write_json(rank, "viz/data/ranking.json", dataframe = "rows", digits = 4, auto_unbox = TRUE)
cat("ranking.json:", nrow(rank), "scenarios\n")

# ---- 2. Weekly trajectories per scenario (mean +/- sd) --------------------
target_scenarios <- read.csv("tables/table_cost_effectiveness.csv", check.names = FALSE,
                             stringsAsFactors = FALSE)$scenario   # all 23 scenarios
MAX_RUNS <- 300                              # full thesis parity (all runs per scenario)
metrics  <- c("participation","motivation","language","integration","dropouts","cost")

traj <- list()
for (sc in target_scenarios) {
  d <- file.path("data", sc)
  if (!dir.exists(d)) { cat("  [skip] no folder:", sc, "\n"); next }
  files <- list.files(d, pattern = "^CIM_timeseries_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) next
  files <- head(files, MAX_RUNS)
  rows <- lapply(files, function(f) {
    b <- tryCatch(read_last_block(f, "^week,participation"), error = function(e) NULL)
    if (is.null(b) || !"week" %in% names(b)) return(NULL)
    b[b$week >= 0 & b$week <= 52, c("week", metrics)]
  })
  df <- bind_rows(rows)
  if (nrow(df) == 0) next
  agg <- df %>% group_by(week) %>%
    summarise(across(all_of(metrics),
                     list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}"), .groups = "drop") %>% arrange(week)
  traj[[sc]] <- agg
  cat("  traj:", sc, "->", length(files), "runs,", nrow(agg), "weeks\n")
}
write_json(traj, "viz/data/trajectories.json", dataframe = "columns",
           digits = 4, auto_unbox = TRUE)
cat("trajectories.json:", length(traj), "scenarios\n")

# ---- 3. Outcomes summary (final-week integration/cost + ranking retention)-
outc <- lapply(names(traj), function(sc) {
  a <- traj[[sc]]; last <- a[nrow(a), ]
  data.frame(scenario = sc,
             retention = rank$retention[match(sc, rank$scenario)],
             rank      = rank$rank[match(sc, rank$scenario)],
             integration_final = round(last$integration_mean, 3),
             cost_final        = round(last$cost_mean, 0),
             cefr_final        = round(last$language_mean, 2),
             dropouts_final    = round(last$dropouts_mean, 1))
}) %>% bind_rows() %>% arrange(rank)
write_json(outc, "viz/data/outcomes.json", dataframe = "rows", digits = 4, auto_unbox = TRUE)
cat("outcomes.json:", nrow(outc), "scenarios\n")

# ---- 4. Network for one Baseline run (nodes + edges with formed_week) ------
ef <- list.files("data/Baseline", pattern = "^CIM_edges_Baseline_.*\\.csv$", full.names = TRUE)
if (length(ef) > 0) {
  e <- read_last_block(ef[1], "^run,scenario,end1_id")
  e <- e[, c("end1_id","end2_id","end1_breed","end2_breed","is_cross_group","formed_week","tie_strength")]
  # node set from endpoints
  nodes <- unique(rbind(
    data.frame(id = e$end1_id, breed = e$end1_breed),
    data.frame(id = e$end2_id, breed = e$end2_breed)))
  nodes <- nodes[!duplicated(nodes$id), ]
  edges <- data.frame(source = e$end1_id, target = e$end2_id,
                      cross = tolower(as.character(e$is_cross_group)) %in% c("true","1"),
                      week  = e$formed_week, strength = round(e$tie_strength, 3))
  write_json(list(nodes = nodes, edges = edges),
             "viz/data/network_baseline.json", dataframe = "rows", auto_unbox = TRUE)
  cat("network_baseline.json:", nrow(nodes), "nodes,", nrow(edges), "edges\n")
}
cat("DONE.\n")
