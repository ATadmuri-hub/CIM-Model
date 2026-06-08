# =============================================================================
# CIM v6.4 — R Environment Setup
# File: R/00_setup.R
# Run this once before any other script.
# =============================================================================

pkgs <- c(
  "tidyverse",   # data wrangling + ggplot2
  "broom",       # tidy model output
  "effsize",     # Cohen's d
  "coin",        # permutation tests
  "survival",    # Kaplan-Meier + Cox PH
  "survminer",   # KM plot formatting
  "sensitivity", # PRCC / Sobol indices
  "lhs",         # Latin Hypercube Sampling
  "diptest",     # Hartigan's dip test for bimodality
  "patchwork",   # combine ggplot panels
  "knitr",
  "kableExtra",  # LaTeX/HTML table formatting
  "scales",
  "ggtext",      # markdown in ggplot labels
  "ggrepel",     # non-overlapping text labels
  "here"         # project-relative paths
)

# Package installation handled by renv::restore()

invisible(lapply(pkgs, library, character.only = TRUE))
message("All packages loaded.")

# Shared paths (set working directory to CIM_Model/ before running)
DATA_DIR    <- "data"
RESULTS_DIR <- "results"
FIGURES_DIR <- "figures"

dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

# See R/constants.R for scenario definitions and colors

# Shared ggplot theme
theme_cim <- function(...) {
  theme_bw(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      strip.background  = element_rect(fill = "grey95"),
      legend.position   = "bottom",
      legend.key.size   = unit(0.5, "cm"),
      plot.title        = element_text(face = "bold", size = 12),
      ...
    )
}

message("Setup complete.")
