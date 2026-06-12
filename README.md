# Agent-Based Modeling of Migrant Integration Through Community Sport

**Calisthenics Integration Model (CIM v6.4)**
Abdullah Tadmuri · UC3M Master's in Computational Social Science · 2026

---

## What this model does

CIM v6.4 simulates a structured calisthenics programme in urban public parks where migrants and locals train together, calibrated to conditions observed in Istanbul (2013--2022), and provides a second-domain Berlin BAMF integration-course calibration for cross-domain portability testing. Over 52 weeks (1 tick = 1 week), the model tracks:

- Attendance and dropout (4 mechanisms: motivation collapse, distance, work conflict, winter)
- Motivation dynamics (peer influence + decay)
- Language acquisition (CEFR scale, diminishing returns)
- Social tie formation (cross-group friendships, tie strength)
- Programme cost per retained participant

**22 scenario conditions + 1 contextual variable** (23 scenarios total) test programme design choices across infrastructure, support services, targeting, group structure, seasonal conditions, and Phase 3 robustness extensions (dose-response, buddy timing, open-cohort churn). The full simulation budget: 7,500 primary policy runs + 810 sensitivity + 100 auxiliary = 8,410 main-pipeline runs, plus 5,700 Tier 3 framework-generality runs (γ-bracket + Berlin second domain) = **14,110 BehaviorSpace runs** in total.

---

## Requirements

- **NetLogo 7.0.3**: [ccl.northwestern.edu/netlogo](https://ccl.northwestern.edu/netlogo/)
- **Java 21** (required by NetLogo 7)
- **R ≥ 4.3** with packages installed via `R/00_setup.R`

> **Two model files.** `CIM_v6_4.nlogo` is the canonical reproducibility artifact; all results, the 36 BehaviorSpace experiments, and the thesis reference this file. `CIM_v6_4_showcase.nlogo` is a presentation build with an upgraded interface (thematic shapes, park-catchment territories, thesis-palette plots, legend) for browsing and demos; its model logic is **bit-identical** to the canonical file (verified across the full 810-run sensitivity sweep). Use the canonical file to reproduce results.

> **Interactive dashboard.** An exploratory web dashboard (policy ranking, animated year replay, network growth, and a browser spatial replay) lives in `viz/` and builds to `docs/` for GitHub Pages. It reads exported data only; see `viz/README.md`.

---

## How to reproduce

### 1. Open the model
Open `CIM_v6_4.nlogo` in NetLogo 7.0.3. The pre-v6.4 source `CIM_v6_3.nlogo` is preserved at the repository root for reference.

### 2. Manual single run
1. Set `config-domain` chooser to `calisthenics-istanbul` (default), `language-course-berlin`, or `custom`. For `custom`, edit `config/custom.csv` first and the model takes **every** parameter from that file. Or just click the **Load Config CSV** button to import any `.csv` as the custom config in one step (see `config/schema.md` → "Adapting to a new domain")
2. Set `scenario-type` chooser to desired scenario
3. Click **Setup** → **Go**
4. Exports written automatically to `data/{scenario}/` on run end

### 3. Full experiment suite (headless, all scenarios)

Two options:

**(a) From NetLogo GUI:** Tools → BehaviorSpace → select experiment → Run.

**(b) From the command line** (recommended for batch runs):
```bash
bash run_all_tier3.sh                 # all 4 Tier 3 experiments, ~20 min on Apple Silicon
bash run_all_experiments.sh           # the original v6.4 experiment suite
```

Both scripts invoke `org.nlogo.headless.Main` directly via Homebrew's openjdk@21 + `netlogo-7.0.3.jar` rather than the native `NetLogo_Console` binary, which can hang in sandboxed environments at JVM init.

BehaviorSpace experiments in `CIM_v6_4.nlogo`:

| Group | Experiments |
|---|---|
| Main pipeline, original 16 | `Baseline_300runs`, `NoIndoor_300runs`, `MinimalSupport_300runs`, `LowParkDensity_300runs`, `LowPark_topup200runs`, `WeakPeer_300runs`, `WeakPeer_topup200runs`, `SuboptimalComp_300runs`, `HighSES_300runs`, `WomenOnly_300runs`, `WomenOnly_topup200runs`, `NoIndoorMinimal_300runs`, `Targeting50_300runs`, `Targeting70_300runs`, `Targeting90_300runs`, `BuddyProgram_300runs`, `RotatingGroups_300runs`, `Winter50_300runs`, `WomenChildcare_300runs` |
| Main pipeline, Phase 3 (7) | `Composition2_300runs`, `Composition3_300runs`, `Composition4_300runs`, `OpenPopulation_300runs`, `SuboptimalOpen_300runs`, `CentralityBuddy_300runs`, `RandomBuddy_300runs` |
| Auxiliary | `Sensitivity_3level` (810 runs), `Equifinality_ContactContagion_100runs` (100 runs) |
| Tier 3 framework-generality | `GammaBracket_Low_300runs` (900 runs), `GammaBracket_High_300runs` (900 runs), `Berlin_AllScenarios_300runs` (3,900 runs) |

Run accounting: 23 main scenarios = 7,500 policy runs + 810 sensitivity + 100 auxiliary = 8,410 main-pipeline runs; + 5,700 Tier 3 = **14,110 runs total** (`OpenPopulation`'s 300 runs are counted once, in the 7,500).

Outputs written to `data/{scenario}/` for per-scenario experiments, or `data/second_domain/{scenario}/` for the Berlin second-domain run:
- `CIM_results_{scenario}_{run}.csv`: per-run summary metrics
- `CIM_timeseries_{scenario}_{run}.csv`: weekly aggregate panel
- `CIM_agents_{scenario}_{run}.csv`: agent-level cross-section
- `CIM_panel_{scenario}_{run}.csv`: agent-week person-period panel
- `CIM_edges_{scenario}_{run}.csv`: friendship edge list

### 4. R analysis pipeline
Run from the `CIM_Model/` directory:
```r
source("R/00_setup.R")             # install packages (once)
source("R/01_load_data.R")         # load CSVs → RDS cache
source("R/02_descriptive_stats.R")
source("R/03_hypothesis_tests.R")  # Welch t-tests, Holm family split (4+11)
source("R/04_sensitivity_analysis.R")  # PRCC
source("R/05_survival_analysis.R")     # Kaplan-Meier + Cox (cluster(run_id) + cox.zph)
source("R/06_visualization.R")
source("R/07_thesis_tables.R")
source("R/08_network_analysis.R")
source("R/09_policy_outputs.R")
source("R/10_validation_table.R")
source("R/11_agent_hazard.R")
source("R/12_distributional.R")
source("R/13_surrogate.R")
source("R/14_internal_invariants.R")
source("R/15_equifinality.R")          # equifinality check (vs contact-contagion)
source("R/16_tier3_blockj_analysis.R") # open-population + gamma-bracket robustness
source("R/17_tier3_blocki_analysis.R") # Berlin second domain + hero figure + ranking invariance
source("R/18_tier3_integrate.R")       # fills Discussion template from robustness outputs
source("R/19_tier3_splice_and_ship.R") # end-to-end: analysis → splice → render → package
source("R/20_sna_graph_analysis.R")    # structural network metrics (modularity, clustering, etc.)
source("R/21_r_naught.R")              # R0 / SIS-threshold derivation
source("R/22_did_parallel_trends.R")   # difference-in-differences corroboration
source("R/23_randomization_inference.R") # randomization-inference p-values
source("R/24_dose_response.R")         # composition dose-response sweep (Kendall, AIC/LRT)
source("R/25_link_prediction.R")       # static link-prediction validation (4 predictors)
source("R/26_open_population_analysis.R") # open-cohort robustness (6 outcomes)
source("R/27_centrality_buddy_analysis.R") # 4-way buddy comparison (timing vs criterion)
source("R/28_open_pop_sna.R")          # open-population multi-metric SNA
source("R/29_suboptimal_open_test.R")  # SuboptimalOpen ranking-preservation test
```
Or run all at once:
```r
source("R/00_run_all.R")
```

### 5. Render thesis
```r
rmarkdown::render("thesis_CIM_v6.Rmd",
                  output_format = c("pdf_document", "word_document"))
```
Produces `thesis_CIM_v6.pdf` (85 pages, PDF/A-1b).

---

## File structure

```
CIM_Model/
├── CIM_v6_4.nlogo              ← NetLogo model (current version, v6.4)
├── CIM_v6_3.nlogo              ← Archival v6.3 source (preserved for reference)
├── thesis_CIM_v6.Rmd           ← Thesis R Markdown document
├── thesis_CIM_v6.pdf           ← Rendered thesis output (PDF/A-1b)
├── run_all_experiments.sh      ← Canonical headless batch runner (all experiments)
├── run_all_tier3.sh            ← Tier 3 experiment runner (γ-bracket, open-pop, Berlin)
├── README.md
├── CITATION.cff
├── LICENSE
├── apa.csl / references.bib
├── renv.lock                   ← Locked R package versions
├── R/
│   ├── 00_setup.R              ← Package installation
│   ├── 00_run_all.R            ← Run full pipeline
│   ├── 01_load_data.R          ← CSV → RDS (handles duplicate blocks)
│   ├── 02_descriptive_stats.R
│   ├── 03_hypothesis_tests.R   ← Welch t-test, Cohen's d, Holm correction
│   ├── 04_sensitivity_analysis.R  ← PRCC + tornado plot
│   ├── 05_survival_analysis.R  ← Kaplan-Meier + Cox PH
│   ├── 06_visualization.R      ← core publication figures
│   ├── 07_thesis_tables.R      ← LaTeX + CSV tables
│   ├── 08_network_analysis.R   ← Degree distribution, assortativity
│   ├── 09_policy_outputs.R     ← Cost-effectiveness, equity frontier
│   ├── 10_validation_table.R   ← Face validity + pattern validity
│   ├── 11_agent_hazard.R       ← Discrete-time dropout hazard (GLM)
│   ├── 12_distributional.R     ← Quantile ribbons, tail risk
│   ├── 13_surrogate.R          ← Scenario comparison summary table (surrogate regression removed in v6.3)
│   ├── 14_internal_invariants.R ← 10 mechanism invariant checks (11 sub-checks)
│   ├── 15_equifinality.R        ← Equifinality check vs. contact-contagion mechanism
│   ├── 16-19_tier3_*.R          ← Tier 3: open-pop, γ-bracket, Berlin, integration, packaging
│   ├── 20-23_*.R                ← SNA metrics, R0 derivation, DiD, randomization inference
│   ├── 24-29_*.R                ← Phase 3: dose-response, link-prediction, open-cohort, buddy, SNA, SuboptimalOpen
│   ├── constants.R             ← Single source of truth (targets, scenarios, palette)
│   └── extra_agent_fate.R      ← Agent survival-landscape figure
├── data/                       ← Simulation CSVs + RDS cache (gitignored)
├── figures/                    ← Generated figures (committed)
└── tables/                     ← Generated tables (committed)
```

---

## Scenarios

| Experiment | Scenario | Runs | Purpose |
|---|---|---|---|
| Baseline_300runs | Baseline | 300 | Control condition |
| NoIndoor_300runs | No Indoor Continuity | 300 | Winter barrier (H3) |
| MinimalSupport_300runs | Minimal Support | 300 | Support cuts (H4) |
| LowParkDensity_300runs + LowPark_topup200runs (500 total) | Low Park Density | 500 | Spatial access |
| WeakPeer_300runs + WeakPeer_topup200runs (500 total) | Weak Peer Influence | 500 | Peer mechanism (H1) |
| SuboptimalComp_300runs | Suboptimal Composition | 300 | Group composition (H2) |
| HighSES_300runs | High SES Heterogeneity | 300 | SES inequality |
| WomenOnly_300runs + WomenOnly_topup200runs (500 total) | Women-Only Groups | 500 | Gender equity |
| NoIndoorMinimal_300runs | NoIndoor Minimal | 300 | Combined barrier |
| Targeting50_300runs / Targeting70_300runs / Targeting90_300runs | Targeting50/70/90 | 300 each | SES targeting accuracy |
| BuddyProgram_300runs | BuddyProgram | 300 | Local buddy assignment |
| RotatingGroups_300runs | RotatingGroups | 300 | Group reshuffling |
| Winter50_300runs | Winter50 | 300 | Partial indoor coverage |
| WomenChildcare_300runs | WomenChildcare | 300 | Women + childcare |
| Composition2/3/4_300runs | Composition2/3/4 | 300 each | Group-composition dose-response (H2) |
| OpenPopulation_300runs | OpenPopulation | 300 | Open-cohort churn robustness |
| SuboptimalOpen_300runs | SuboptimalOpen | 300 | Suboptimal under churn (ranking-preservation test) |
| CentralityBuddy_300runs | CentralityBuddy | 300 | Week-8 buddy by highest local degree |
| RandomBuddy_300runs | RandomBuddy | 300 | Week-8 buddy by random selection (timing control) |

**Total: 7,500 runs across 23 scenarios** (16 original + 7 Phase 3 robustness extensions)

---

## Key parameters

| Parameter | Default | Range | Basis |
|---|---|---|---|
| Peer influence coefficient | 0.08 | 0.01–0.20 | Centola (2010) |
| Motivation decay rate | 0.018 | 0.01–0.04 | Gjestvang (2020) |
| Language gain rate | 0.019 | 0.01–0.025 | CEFR literature |
| Tie formation probability | 0.05 | 0.02–0.15 | Kossinets & Watts (2006) |
| Dropout threshold | 0.20 | 0.10–0.40 | SDT literature |

---

## Data availability

The complete raw simulation output (43,713 CSV files from 14,110 BehaviorSpace runs) is archived on Zenodo: **https://doi.org/10.5281/zenodo.20668921** (CC BY 4.0). This repository ships the analysis-ready figures, tables, and precomputed outputs; the raw per-run CSVs are regenerable from `run_all_experiments.sh` and `run_all_tier3.sh`.

## Citation

```bibtex
@software{tadmuri2026cim,
  author  = {Tadmuri, Abdullah},
  title   = {Calisthenics Integration Model ({CIM} v6.4)},
  year    = {2026},
  version = {6.4},
  url     = {https://github.com/ATadmuri-hub/CIM-Model}
}
```

---

## License

Dual-licensed, see `LICENSE`:
- **Software** (NetLogo model, `R/` pipeline, `config/`, shell runners): **MIT**.
- **Thesis manuscript** (text + figures; `thesis_CIM_v6.*`): **CC BY-NC-ND 4.0**, as declared on the thesis cover and in its PDF/A metadata.
