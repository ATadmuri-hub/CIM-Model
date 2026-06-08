
# 04_sensitivity_analysis.R — PRCC sensitivity analysis
library(tidyverse)
library(sensitivity)

DATA_DIR <- "data"

sens_file <- file.path(DATA_DIR, "Sensitivity", "sensitivity_table.csv")

# BehaviorSpace table format: skip first 6 lines (header metadata)
# Line 7+ has actual data with header row
raw_lines <- readLines(sens_file)
# Find the line with "[run number]" as the column header
header_line <- which(grepl("\\[run number\\]", raw_lines))
if (length(header_line) == 0) {
  cat("Could not find header line. Showing first 10 lines:\n")
  cat(paste(head(raw_lines, 10), collapse="\n"), "\n")
  stop("Header not found")
}

cat("Header at line:", header_line, "\n")
sens_raw <- read_csv(sens_file, skip = header_line - 1, show_col_types = FALSE)
cat("Sensitivity table:", nrow(sens_raw), "rows,", ncol(sens_raw), "columns\n")
cat("Column names:\n"); print(names(sens_raw))

# Clean column names
names(sens_raw) <- gsub("\\[|\\]", "", names(sens_raw))
names(sens_raw) <- gsub(" ", "_", names(sens_raw))
names(sens_raw) <- tolower(names(sens_raw))

cat("\nCleaned column names:\n"); print(names(sens_raw))

# Parameter and outcome columns — use actual names (hyphens preserved)
param_cols <- intersect(
  c("motivation-decay-rate", "peer-influence-coefficient",
    "tie-formation-probability", "dropout-threshold"),
  names(sens_raw)
)
cat("\nParameter columns found:", paste(param_cols, collapse=", "), "\n")

outcome_cols <- intersect(
  c("retention-rate-percent", "avg-motivation-level", "avg-language-proficiency",
    "cross-group-tie-ratio", "total-dropouts"),
  names(sens_raw)
)
cat("Outcome columns found:", paste(outcome_cols, collapse=", "), "\n")

if (length(param_cols) == 0 || length(outcome_cols) == 0) {
  cat("\nERROR: Column mismatch. Trying underscored versions...\n")
  # Maybe cleaning converted hyphens to underscores
  names(sens_raw) <- gsub("-", "_", names(sens_raw))
  param_cols <- intersect(
    c("motivation_decay_rate", "peer_influence_coefficient",
      "tie_formation_probability", "dropout_threshold"),
    names(sens_raw)
  )
  outcome_cols <- intersect(
    c("retention_rate_percent", "avg_motivation_level", "avg_language_proficiency",
      "cross_group_tie_ratio", "total_dropouts"),
    names(sens_raw)
  )
  cat("After underscore conversion:\n")
  cat("  params:", paste(param_cols, collapse=", "), "\n")
  cat("  outcomes:", paste(outcome_cols, collapse=", "), "\n")
}

if (length(param_cols) == 0 || length(outcome_cols) == 0) {
  cat("ERROR: Could not match columns.\n")
  stop("Column mismatch")
}

# Rename columns: hyphens → underscores (required for R formula parsing)
names(sens_raw) <- gsub("-", "_", names(sens_raw))
param_cols  <- gsub("-", "_", param_cols)
outcome_cols <- gsub("-", "_", outcome_cols)

cat("\nRenamed parameter cols:", paste(param_cols, collapse=", "), "\n")
cat("Renamed outcome cols:", paste(outcome_cols, collapse=", "), "\n")

# PRCC using sensitivity::pcc
cat("\n=== Partial Rank Correlation Coefficients (PRCC) ===\n")

prcc_results <- map_dfr(outcome_cols, function(outcome) {
  df_clean <- sens_raw %>%
    select(all_of(c(param_cols, outcome))) %>%
    drop_na()
  
  if (nrow(df_clean) < 10) {
    cat("Insufficient data for", outcome, "\n")
    return(NULL)
  }
  
  X <- df_clean[, param_cols, drop = FALSE]
  y <- df_clean[[outcome]]
  
  set.seed(42)
  pcc_res <- tryCatch(
    pcc(X, y, rank = TRUE, nboot = 1000, conf = 0.95),
    error = function(e) { cat("Error for", outcome, ":", e$message, "\n"); NULL }
  )
  
  if (is.null(pcc_res)) return(NULL)
  
  pcc_df <- as.data.frame(pcc_res$PRCC)
  pcc_df$parameter <- rownames(pcc_df)
  pcc_df$outcome <- outcome
  pcc_df
})

if (!is.null(prcc_results) && nrow(prcc_results) > 0) {
  cat("\nPRCC results (", nrow(prcc_results), "rows):\n")
  # Safe print
  prcc_safe <- as.data.frame(lapply(prcc_results, function(x) {
    if (is.factor(x)) as.character(x) else x
  }))
  for (i in seq_len(nrow(prcc_safe))) {
    cat(" ", prcc_safe$outcome[i], prcc_safe$parameter[i], 
        "PRCC=", round(as.numeric(prcc_safe[i, "original"]), 3), "\n")
  }
  saveRDS(prcc_results, file.path(DATA_DIR, "sensitivity_prcc.rds"))
  write_csv(prcc_results, file.path(DATA_DIR, "sensitivity_prcc.csv"))
  cat("Saved: sensitivity_prcc.csv\n")
} else {
  cat("No PRCC results computed.\n")
}

# Save cleaned sensitivity table
saveRDS(sens_raw, file.path(DATA_DIR, "sensitivity_clean.rds"))
cat("Saved: sensitivity_clean.rds\n")
