# 00_run_all.R — Run entire analysis pipeline
# Usage: Rscript R/00_run_all.R   (from CIM_Model/ directory)
# OR: setwd("<path>/CIM_Model"); source("R/00_run_all.R")

# Load shared constants (targets, scenario classification, winter onset)
source("R/constants.R")

cat("=== CIM v6.4 Analysis Pipeline ===
")
cat("Working directory:", getwd(), "

")

scripts <- c(
  "R/01_load_data.R",
  "R/02_descriptive_stats.R",
  "R/03_hypothesis_tests.R",
  "R/04_sensitivity_analysis.R",
  "R/05_survival_analysis.R",
  "R/06_visualization.R",
  "R/07_thesis_tables.R",
  "R/08_network_analysis.R",
  "R/09_policy_outputs.R",
  "R/10_validation_table.R",
  "R/11_agent_hazard.R",
  "R/12_distributional.R",
  "R/extra_agent_fate.R",
  "R/13_surrogate.R",
  "R/14_internal_invariants.R",
  "R/15_equifinality.R"  # Alternative Mechanism Benchmark (auxiliary experiment)
)

for (s in scripts) {
  if (file.exists(s)) {
    cat(sprintf("
>>> Running: %s
", s))
    tryCatch(
      source(s, local = FALSE, chdir = FALSE),
      error = function(e) cat(sprintf("ERROR in %s: %s
", s, e$message))
    )
    cat(sprintf("<<< Done: %s
", s))
  } else {
    cat(sprintf("SKIP (not found): %s
", s))
  }
}

cat("
=== Pipeline complete ===
")
