#!/usr/bin/env Rscript
# viz/01_prep_replay.R — compact JSON for the Tier-C spatial replay player.
# Positions are static across weeks, so store layout once + per-week mot/state/edges.
suppressMessages(library(jsonlite))
if (nzchar(Sys.getenv("CIM_ROOT"))) setwd(Sys.getenv("CIM_ROOT"))  # otherwise run from the repo root

nd <- read.csv("data/Replay/Baseline_nodes.csv", stringsAsFactors = FALSE)
ed <- read.csv("data/Replay/Baseline_edges.csv", stringsAsFactors = FALSE)

# static layout from week 0 (agents do not move during a run)
w0 <- nd[nd$week == 0, ]
w0 <- w0[order(w0$who), ]
ids <- w0$who
state_code <- function(s) match(s, c("active","paused","dropped")) - 1  # 0/1/2

weeks <- sort(unique(nd$week))
mot <- vector("list", length(weeks))
stt <- vector("list", length(weeks))
edg <- vector("list", length(weeks))
for (k in seq_along(weeks)) {
  w <- weeks[k]
  nw <- nd[nd$week == w, ]; nw <- nw[match(ids, nw$who), ]
  mot[[k]] <- round(nw$mot, 3)
  stt[[k]] <- state_code(nw$state)
  ew <- ed[ed$week == w, ]
  edg[[k]] <- if (nrow(ew)) unname(as.matrix(ew[, c("e1","e2","cross")])) else matrix(numeric(0), 0, 3)
}

out <- list(
  world = 100,
  id    = ids,
  kind  = w0$kind,
  x     = round(w0$x, 2),
  y     = round(w0$y, 2),
  weeks = weeks,
  mot   = mot,
  state = stt,
  edges = edg
)
write_json(out, "viz/data/replay_baseline.json", auto_unbox = TRUE, digits = 3)
cat("replay_baseline.json:", length(weeks), "weeks,", length(ids), "agents,",
    nrow(ed), "edge-rows | size",
    round(file.size("viz/data/replay_baseline.json")/1024), "KB\n")
