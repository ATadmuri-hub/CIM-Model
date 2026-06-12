# REPRODUCIBILITY: CIM v6.4

## Project: Agent-Based Modeling of Migrant Integration Through Community Sport: Testing Program Design Before Implementation

## System Requirements
- **NetLogo**: 7.0.3 (simulations only; not needed to reproduce analysis)
- **Java**: 21+ (for headless NetLogo)
- **R**: ≥ 4.3
- **renv**: managed via `renv.lock`

---

## Working Directory

Run all commands from the `CIM_Model/` directory. The project path is plain ASCII (`Masters_Thesis/CIM_Model/`) and needs no special handling.

**Path note for `run_all_experiments.sh`**: The shell script contains hardcoded macOS paths (JAVA_HOME, NetLogo JAR location, model path) that assume macOS ARM with Homebrew-installed Java 21 and NetLogo 7.0.3 in ~/Downloads. Users on other systems must update these paths via environment variables; see the script header for details.

---

## Random Seeds

Each BehaviorSpace run sets a deterministic seed, `random-seed (behaviorspace-run-number + run-start-index)` (model `setup`, ~line 357), so every reported result is reproducible **bit-for-bit** by re-running the named experiment. Interactive (GUI) runs use a fresh seed. The v6.4 pinned regeneration (May 2026) fixed these seeds, so re-running a scenario reproduces its CSVs exactly. With 300+ replications per scenario, statistical conclusions are also robust to any change of the seed base.

---

## Reproducing the Analysis

### Data Availability

The complete simulation output (43,713 CSV files from 14,110 BehaviorSpace runs) is archived on Zenodo: https://doi.org/10.5281/zenodo.20668921 (CC BY 4.0). It is excluded from the git repository via `.gitignore` due to its size, and is fully regenerable from scratch by running `bash run_all_experiments.sh` and `bash run_all_tier3.sh` (requires NetLogo 7.0.3 and Java 21).

You do NOT need to re-run the NetLogo model if you already have the data CSVs in `data/`.

### Data Completeness

After the v6.4 pinned regeneration (May 2026), all 23 scenarios export full results, panel, and edge CSVs: 300 runs each, 500 for the three precision top-up scenarios (Low Park Density, Weak Peer Influence, Women-Only Groups). Per-scenario file counts are recorded in `tables/table_experiment_accounting.csv`. (Earlier releases lacked panel/edge exports for some scenarios; that gap is now closed.)

### Step 1: Restore R environment
```r
install.packages("renv")  # if not installed
renv::restore()
```

### Step 2: Run the full analysis pipeline
```bash
Rscript R/00_run_all.R
```

Or from within R:
```r
source("R/00_run_all.R")
```

### Step 3: Render the thesis document
```bash
Rscript -e 'rmarkdown::render("thesis_CIM_v6.Rmd", output_format=c("pdf_document","word_document"))'
```

**Requires**: `pandoc` (included with RStudio) and a LaTeX distribution (TinyTeX) for the PDF. Generates `thesis_CIM_v6.pdf` (85 pages, PDF/A-1b) and `thesis_CIM_v6.docx`.

### Expected Outputs

| Directory | Contents |
|-----------|----------|
| `tables/` | 26 CSV files + LaTeX tables (policy outputs, validation, hypothesis tests, dose-response, link-prediction, etc.) |
| `figures/` | 30 PNG figures at 300 DPI |
| `tables/table_experiment_accounting.csv` | Manifest: N runs per scenario |
| `output/logs/` | Pipeline run logs |

### Expected Runtime
Approximately 10–20 minutes on a standard laptop (loading ~7,500 policy runs + sensitivity/auxiliary + analyses).

---

## Simulation Data

### 23 Scenarios (7,500 total policy runs)
| Scenario | N runs | Type |
|----------|--------|------|
| Baseline | 300 | Lever |
| No Indoor Continuity | 300 | Lever |
| Minimal Support | 300 | Lever |
| Low Park Density | 500 | Lever (precision top-up) |
| Weak Peer Influence | 500 | Lever (precision top-up) |
| Suboptimal Composition | 300 | Lever |
| High SES Heterogeneity | 300 | Context |
| Women-Only Groups | 500 | Lever (precision top-up) |
| NoIndoor Minimal | 300 | Lever |
| Targeting50 | 300 | Lever |
| Targeting70 | 300 | Lever |
| Targeting90 | 300 | Lever |
| BuddyProgram | 300 | Lever |
| RotatingGroups | 300 | Lever |
| Winter50 | 300 | Lever |
| WomenChildcare | 300 | Lever |
| Composition2 | 300 | Robustness (H2 dose) |
| Composition3 | 300 | Robustness (H2 dose) |
| Composition4 | 300 | Robustness (H2 dose) |
| OpenPopulation | 300 | Robustness (open cohort) |
| SuboptimalOpen | 300 | Robustness (ranking test) |
| CentralityBuddy | 300 | Robustness (buddy timing) |
| RandomBuddy | 300 | Robustness (buddy timing) |
| **Total** | **7,500** | 23 scenarios |

### Auxiliary and Tier 3 Experiments (separately reported)
| Experiment | N runs | Purpose |
|------------|--------|---------|
| Sensitivity (`Sensitivity_3level`) | 810 | Global sensitivity / PRCC (3⁴ × 10) |
| Alternative Mechanism Benchmark (Equifinality) | 100 | Mechanism sensitivity: contact-presence contagion vs tie-weighted peer influence |
| Tier 3 framework-generality (γ-bracket + Berlin second domain) | 5,700 | Robustness + cross-domain portability |

**Grand total**: 7,500 + 810 + 100 + 5,700 = **14,110 BehaviorSpace runs**.

---

## Re-running Simulations (optional)
To regenerate simulation data from scratch using NetLogo headless:
```bash
bash run_all_experiments.sh
```
**Warning**: This takes many hours and requires NetLogo 7.0.3 + Java 21.

---

## Data Format Notes
- Results CSVs: key-value format (`metric,value` header)
- Panel CSVs: agent-week panel (`run,scenario,agent_id,week,motivation,event,...`)
- Edge CSVs: friendship network (`run,scenario,end1_id,end2_id,...`)
- Multi-block files: the pipeline uses last-block logic to handle duplicate-appended CSVs from top-up runs

---

## License
MIT License, see `LICENSE` file.

## Citation
See `CITATION.cff` for machine-readable citation metadata.
