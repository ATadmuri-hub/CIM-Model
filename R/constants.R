# =============================================================================
# constants.R — Single source of truth for CIM v6.4 analysis pipeline
# Source this file from R/00_run_all.R before running any analysis scripts.
# =============================================================================

# ── Policy targets ────────────────────────────────────────────────────────────
# These are the single canonical thresholds used in all policy evaluation scripts.
# Do NOT hardcode these numbers in individual analysis scripts.
TARGET_RETENTION  <- 40    # percent [0-100]: minimum acceptable retention rate
TARGET_CROSS_TIE  <- 0.40  # ratio [0,1]: minimum cross-group tie ratio
TARGET_COST       <- 3500  # EUR per retained participant: maximum acceptable cost
TARGET_LANGUAGE   <- 1.0   # CEFR proxy units: minimum language gain threshold
                            # (explicitly a proxy; not equivalent to CEFR level)

# ── Seasonal structure ────────────────────────────────────────────────────────
# Verified from CIM_v6_4.nlogo:
#   L300: set indoor-season-start 9
#   L301: set indoor-season-end 28
# Season schedule: autumn (wk 1-8) → winter (wk 9-28) → spring/summer (wk 29-52)
# Winter = October through March (Mediterranean/temperate climate approximation)
WINTER_ONSET_WEEK <- 9   # first week of winter/indoor season
WINTER_END_WEEK   <- 28  # last week of winter/indoor season
SIM_DURATION      <- 52  # total simulation weeks

# ── Agent population (for documentation only — do NOT use as hardcoded counts) ──
# BehaviorSpace baseline experiment settings (CIM_v6_3.nlogo, Baseline_300runs):
#   num-parks = 5, refugees-per-group = 15, locals-per-group = 5
#   → 75 migrants + 25 locals + 5 trainers + 5 park entities
# These values are verified via table_experiment_accounting.csv (Phase 1 output).
# The README previously listed "24 migrants + 9 locals" — those are prototype values.
# Do not hardcode population counts in analysis scripts; compute from data instead.

# ── Scenario classification ───────────────────────────────────────────────────
# IMPORTANT: This block defines the ORIGINAL 16 policy scenarios (15 levers +
# 1 context = 5,400 runs). The Phase 3 verification round 2 added 7 more
# robustness-family scenarios (Composition2/3/4, OpenPopulation, SuboptimalOpen,
# CentralityBuddy, RandomBuddy) bringing the v6.4 main pipeline to 23 scenarios
# / 7,500 runs. POLICY_LEVERS below is intentionally LEFT AT THE ORIGINAL 15
# entries because (a) downstream scripts (R/06 visualization, R/09 policy
# outputs) hard-code length-15 layouts and (b) Phase 3 robustness tests are
# tracked separately in R/03_hypothesis_tests.R::ROBUSTNESS. To extend the
# canonical lever set, add new entries here AND update R/06 / R/09 figure code.
# 15 POLICY LEVERS (original) — programme design choices directly modifiable by implementers:
POLICY_LEVERS <- c(
  "Baseline",
  "No Indoor Continuity",
  "Minimal Support",
  "Low Park Density",
  "Weak Peer Influence",
  "Suboptimal Composition",  # lever: adjusting locals-per-group is actionable
  "Women-Only Groups",
  "NoIndoor Minimal",
  "Targeting50",
  "Targeting70",
  "Targeting90",
  "BuddyProgram",
  "RotatingGroups",
  "Winter50",
  "WomenChildcare"
)

# 1 CONTEXT variable — external/structural condition; not directly actionable:
CONTEXT_VARS <- c("High SES Heterogeneity")

# Together: 16 policy scenarios for the main policy evaluation
POLICY_SCENARIOS <- c(POLICY_LEVERS, CONTEXT_VARS)

# AUXILIARY experiments — excluded from 16-scenario policy counts:
# Reported separately in the "Alternative Mechanism Benchmark" section.
AUXILIARY_EXPERIMENTS <- c("Equifinality")
# Note: data/Sensitivity/ contains sensitivity_table.csv (output of Sensitivity_3level
# BehaviorSpace experiment). Excluded from 16-scenario policy counts; reported separately
# via 04_sensitivity_analysis.R.

# ── Display labels ────────────────────────────────────────────────────────────
# Scenario type label for plot legends and table annotations
scenario_type_label <- function(sc) {
  if (sc %in% POLICY_LEVERS) return("Lever")
  if (sc %in% CONTEXT_VARS)  return("Context")
  if (sc %in% AUXILIARY_EXPERIMENTS) return("Auxiliary")
  return("Unknown")
}


# Centralized color palette (16 scenarios)
# Canonical source: originally defined in R/06_visualization.R.
# All analysis scripts should use this palette via source("R/constants.R").
SCEN_COLORS <- c(
  # Original 8
  "Baseline"               = "#2E86AB",
  "No Indoor Continuity"   = "#E84855",
  "Minimal Support"        = "#F26419",
  "Low Park Density"       = "#A23B72",
  "Weak Peer Influence"    = "#F18F01",
  "Suboptimal Composition" = "#C73E1D",
  "High SES Heterogeneity" = "#3B6E8C",
  "Women-Only Groups"      = "#44BBA4",
  # Additional scenarios
  "NoIndoor Minimal"       = "#9B2335",
  "Targeting50"            = "#B5D99C",
  "Targeting70"            = "#6DB56B",
  "Targeting90"            = "#2D6A4F",
  "BuddyProgram"           = "#FFBC42",
  "RotatingGroups"         = "#D4A5A5",
  "Winter50"               = "#7B9EA6",
  "WomenChildcare"         = "#D62246"
)

message("constants.R loaded: targets, season parameters, scenario classification, and color palette defined.")

# ── Unified thesis theme (used by ALL figure scripts) ──────────────────────
theme_thesis <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    strip.background   = element_rect(fill = "grey95", colour = NA),
    strip.text         = element_text(face = "bold", size = 9),
    plot.title         = element_text(face = "bold", size = 13, colour = "grey10"),
    plot.subtitle      = element_text(size = 9, colour = "grey40"),
    plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0),
    axis.text          = element_text(colour = "grey30"),
    axis.title         = element_text(colour = "grey20")
  )
cat("theme_thesis defined (unified)\n")
