# CIM Replay Dashboard (`viz/`)

An interactive web dashboard for the CIM v6.4 model — a static site (GitHub Pages) built from the
**verified pipeline outputs**. It reads exported data only; the published model and results are
untouched.

## Tabs
- **Policy ranking** — interactive ranked comparison of the 16 main scenarios (toggle retention /
  integration / cost / CEFR). The thesis's core "which design to choose" result.
- **Replay & network** — "play the year" scrubber animating the 6 macro metrics (mean ± SD) + a
  D3 friendship-network growth view (cross-group vs within-group ties).
- **Spatial replay** — a browser reproduction of the NetLogo world view for the canonical seed-1
  Baseline run (parks ▲, trainers ★, migrants/locals coloured by motivation, dropouts grey,
  ties orange/grey), week 0 → 52.
- **Explore** — scenario outcomes table.

## Build
```bash
export CIM_ROOT="$PWD"                 # repo root
Rscript viz/00_prep_dashboard.R        # -> viz/data/{ranking,trajectories,outcomes,network_baseline}.json
Rscript viz/01_prep_replay.R           # -> viz/data/replay_baseline.json  (needs data/Replay/*, see below)
~/quarto_bin/bin/quarto render viz/dashboard.qmd
# deploy: copy build beside the data for GitHub Pages
cp viz/dashboard.html docs/index.html
cp -R viz/dashboard_files docs/dashboard_files
cp -R viz/data docs/data
```
GitHub Pages: serve from `/docs` on the default branch. `docs/.nojekyll` is present.

## Regenerating the spatial-replay frames (Tier C)
The frames come from `CIM_v6_4_replay.nlogo` — a copy of the canonical model with a switch-gated,
RNG-isolated per-week export (`export-replay?`, default OFF → published experiments bit-identical).
```bash
JAVA=/opt/homebrew/Cellar/openjdk@21/21.0.10/libexec/openjdk.jdk/Contents/Home/bin/java
NL="$HOME/Downloads/NetLogo 7.0.3"
mkdir -p data/Replay
"$JAVA" -Dfile.encoding=UTF-8 -classpath "$NL/app/netlogo-7.0.3.jar" org.nlogo.headless.Main \
  --model CIM_v6_4_replay.nlogo --experiment Replay_Baseline --table /tmp/replay.csv --threads 1
# -> data/Replay/Baseline_nodes.csv, Baseline_edges.csv ; then run 01_prep_replay.R
```

## Reproducibility note
Every figure traces to `outputs/istanbul_ranking_precomputed.csv`, `tables/*.csv`, or per-run
`data/<scenario>/*.csv` (last-block read handles the duplicate-append artifact). The dashboard
adds no new statistics — it visualises the verified pipeline.
