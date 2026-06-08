# CIM v6.4: Tier 3 integration (archival build script).
#
# One-time development script that consolidated the Block J (R/16) and Block I (R/17)
# robustness outputs into the Discussion text later merged into the thesis. Retained
# for provenance: its inputs are intermediate build artefacts not shipped in this
# repository, so it is not part of the reproducible analysis pipeline (see R/00_run_all.R).

library(tidyverse)

OUT_DIR <- "outputs"
TEMPLATE <- "_build/t3_6_discussion_template.md"
OUTPUT_MD <- "_build/t3_6_discussion_filled.md"

if (!file.exists(TEMPLATE)) stop("Template not found: ", TEMPLATE)
tpl <- readLines(TEMPLATE)

# Helper: load RDS, return NULL on failure
load_rds <- function(f) tryCatch(readRDS(file.path(OUT_DIR, f)),
                                 error = function(e) NULL)

replacements <- list()

# ----------------------------------------------------------------------------
# Block J fills
# ----------------------------------------------------------------------------
t3a <- load_rds("t3a_open_population_ranking.rds")
if (!is.null(t3a)) {
  op_row <- t3a[t3a$scenario == "OpenPopulation", ]
  bl_row <- t3a[t3a$scenario == "Baseline", ]
  if (nrow(op_row) > 0 && nrow(bl_row) > 0) {
    op_mean <- round(op_row$retention_rate_mean, 1)
    bl_mean <- round(bl_row$retention_rate_mean, 1)
    shift   <- round(op_mean - bl_mean, 1)
    replacements[["OpenPopulation retention"]] <- sprintf(
      "%.1f%% (N = 300). Shift from closed-population Baseline: %+.1f pp.", op_mean, shift
    )
  }
}

t3b <- load_rds("t3b_gamma_bracket.rds")
if (!is.null(t3b)) {
  wide <- t3b %>% pivot_wider(names_from = scenario, values_from = retention_mean, id_cols = gamma)
  low  <- wide[wide$gamma == 0.011, ]
  high <- wide[wide$gamma == 0.025, ]
  if (nrow(low) > 0 && nrow(high) > 0) {
    replacements[["γ bracket low"]] <- sprintf(
      "BuddyProgram = %.1f%%, Baseline = %.1f%%, Suboptimal = %.1f%%",
      low$BuddyProgram, low$Baseline, low$`Suboptimal Composition`
    )
    replacements[["γ bracket high"]] <- sprintf(
      "BuddyProgram = %.1f%%, Baseline = %.1f%%, Suboptimal = %.1f%%",
      high$BuddyProgram, high$Baseline, high$`Suboptimal Composition`
    )
  }
}

t3c <- load_rds("t3c_sensitivity_v62_vs_v65.rds")
if (!is.null(t3c)) {
  gamma_row <- t3c[grepl("motivation-decay|gamma", tolower(t3c$parameter)), ]
  if (nrow(gamma_row) > 0) {
    replacements[["v6.5 PRCC"]] <- sprintf("%.3f (delta = %.3f vs v6.2 -0.96)",
                                           gamma_row$prcc_v65[1], gamma_row$delta[1])
  }
}

# ----------------------------------------------------------------------------
# Block I fills
# ----------------------------------------------------------------------------
t3_6 <- load_rds("t3_6_ranking_invariance.rds")
if (!is.null(t3_6)) {
  summary_by_pair <- t3_6 %>% group_by(pair) %>%
    summarise(consistent = all(relation_holds, na.rm = TRUE), .groups = "drop")
  n_consistent <- sum(summary_by_pair$consistent, na.rm = TRUE)
  n_total <- nrow(summary_by_pair)
  replacements[["ranking invariance"]] <- sprintf(
    "%d of %d tested ranking relations hold in both domains. %s",
    n_consistent, n_total,
    if (n_consistent == n_total) "Framework-generality claim supported."
    else "Framework-generality claim partial; domain-specific narrative required."
  )
}

# ----------------------------------------------------------------------------
# Write filled template
# ----------------------------------------------------------------------------
if (length(replacements) == 0) {
  cat("No data loaded; filled template is empty of numbers.\n")
  cat("Run R/16 and R/17 first, then rerun this script.\n")
} else {
  header <- c(
    "# T3.6 Discussion, FILLED with experiment results",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    "## Filled values (drop into thesis_CIM_v6.Rmd as §5.x)",
    ""
  )
  body <- imap_chr(replacements, function(value, key) {
    sprintf("- **%s**: %s", key, value)
  })
  filled <- c(header, body, "", "## Source template with placeholders:", "", tpl)
  writeLines(filled, OUTPUT_MD)
  cat("Filled discussion written to: ", OUTPUT_MD, "\n", sep = "")
  cat(sprintf("Filled %d placeholders:\n", length(replacements)))
  for (k in names(replacements)) cat("  - ", k, "\n", sep = "")
}
