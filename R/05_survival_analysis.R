
# 05_survival_analysis.R — Kaplan-Meier + Cox PH
# T1.10 (plan v2): Cox model now uses cluster-robust SEs by simulation run
# and reports Schoenfeld residuals test of the proportional-hazards assumption.
library(tidyverse)
library(survival)

DATA_DIR <- "data"

agents_df <- readRDS(file.path(DATA_DIR, "agents_df.rds"))

cat("Agent data dimensions:", nrow(agents_df), "x", ncol(agents_df), "\n")
cat("Column names:\n"); print(names(agents_df))

# Check key columns exist
required <- c("dropped_out", "dropout_week", "gender", "breed", "scenario")
present  <- required %in% names(agents_df)
cat("\nRequired columns present:", paste(required[present], collapse=", "), "\n")
if (any(!present)) cat("MISSING:", paste(required[!present], collapse=", "), "\n")

# Build survival object if columns exist
if (all(c("dropped_out", "dropout_week") %in% names(agents_df))) {

  # T1.10: preserve run identifier for cluster-robust SEs in Cox model.
  # NetLogo BehaviorSpace CSV export column is 'run'; fall back to 'run_id' if renamed upstream.
  run_col <- if ("run" %in% names(agents_df)) "run" else if ("run_id" %in% names(agents_df)) "run_id" else NA_character_
  if (is.na(run_col)) stop("T1.10: neither 'run' nor 'run_id' column found in agents_df; cannot compute cluster-robust SEs.")

  surv_df <- agents_df %>%
    mutate(
      event    = as.integer(dropped_out),
      time     = ifelse(is.na(dropout_week) | dropout_week == 0 | !dropped_out, 52L,
                        as.integer(dropout_week)),
      time     = pmax(time, 1L),
      gender   = factor(gender),
      breed    = factor(breed),
      scenario = factor(scenario),
      run_id   = factor(.data[[run_col]])
    )

  cat("\nSurvival data summary:\n")
  cat("Events (dropouts):", sum(surv_df$event, na.rm=TRUE), "/", nrow(surv_df), "\n")

  # Kaplan-Meier by scenario
  km_scenario <- survfit(Surv(time, event) ~ scenario, data = surv_df)
  cat("\nKM by scenario (median survival week):\n")
  print(summary(km_scenario)$table[, c("records","events","median")])

  # KM by gender (Baseline only)
  km_gender <- survfit(Surv(time, event) ~ gender,
                       data = filter(surv_df, scenario == "Baseline", breed == "refugee"))
  cat("\nKM by gender (Baseline):\n")
  print(summary(km_gender)$table[, c("records","events","median")])

  # Cox PH
  # Final-review fix: drop "breed" — the Cox model is fit on the migrant cohort only
  # (cox_df below), within which breed is constant. (Mixed-breed fitting was the source
  # of the stored-object N mismatch flagged in the audit; prose reports migrant-only N.)
  cox_vars <- intersect(
    c("gender", "initial_motivation", "distance_to_park",
      "ses", "prior_exercise", "arrival_cohort", "scenario"),
    names(surv_df)
  )
  cat("\nCox PH covariates available:", paste(cox_vars, collapse=", "), "\n")
  cat("Note (T1.10): initial_motivation is a pre-treatment baseline covariate, assigned at agent\n")
  cat("creation before apply-scenario-configuration; it is a legitimate baseline control, not a mediator.\n")

  if (length(cox_vars) >= 2) {
    # Final-review fix: fit on the migrant cohort only AND restrict to the 16 main-pipeline
    # scenarios (the thesis scopes the survival model to the original campaign; the full
    # regeneration added Phase-3 scenarios' agent exports, which would otherwise broaden the
    # pool). This restores the reported migrant-only N (~405,000) and scenario set.
    main_scenarios <- c("Baseline","No Indoor Continuity","Minimal Support","Low Park Density",
      "Weak Peer Influence","Suboptimal Composition","High SES Heterogeneity","Women-Only Groups",
      "NoIndoor Minimal","Targeting50","Targeting70","Targeting90","BuddyProgram",
      "RotatingGroups","Winter50","WomenChildcare")
    cox_df <- droplevels(surv_df[surv_df$breed == "refugee" &
                                 surv_df$scenario %in% main_scenarios, ])
    cat(sprintf("Cox fit on migrant-only / 16-scenario frame: N = %d agent-run rows, %d events\n",
                nrow(cox_df), sum(cox_df$event)))
    # T1.10: robust SEs clustered by simulation run (addresses within-run correlation).
    formula_str <- paste("Surv(time, event) ~", paste(cox_vars, collapse=" + "), "+ cluster(run_id)")
    cox_model <- tryCatch(
      coxph(as.formula(formula_str), data = cox_df),
      error = function(e) { cat("Cox error:", e$message, "\n"); NULL }
    )

    if (!is.null(cox_model)) {
      cat("\nCox PH Model (robust SEs clustered by run):\n")
      print(summary(cox_model)$coefficients)
      saveRDS(cox_model, file.path(DATA_DIR, "cox_model.rds"))
      cat("Saved: cox_model.rds\n")

      # T1.10: Schoenfeld residuals test of the PH assumption.
      # A per-covariate p < 0.05 indicates PH is violated; consider stratification or time-varying coefficients.
      if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)
      ph_test <- tryCatch(cox.zph(cox_model), error = function(e) { cat("cox.zph error:", e$message, "\n"); NULL })
      if (!is.null(ph_test)) {
        cat("\nSchoenfeld residuals test (H0: proportional hazards):\n")
        print(ph_test)
        saveRDS(ph_test, "outputs/cox_zph.rds")
        cat("Saved: outputs/cox_zph.rds\n")
        violated <- ph_test$table[rownames(ph_test$table) != "GLOBAL", "p"] < 0.05
        if (any(violated, na.rm = TRUE)) {
          cat("\nWARNING (T1.10): PH assumption violated (p < 0.05) for:",
              paste(names(violated)[which(violated)], collapse = ", "),
              "\nConsider stratification or time-varying coefficients; surface in Limitations.\n")
        } else {
          cat("\nPH assumption holds for all covariates at alpha = 0.05.\n")
        }
      }
    }
  }

  saveRDS(surv_df, file.path(DATA_DIR, "survival_df.rds"))
  cat("Saved: survival_df.rds\n")

} else {
  cat("Skipping survival analysis — required columns not found.\n")
  cat("Available:", paste(names(agents_df), collapse=", "), "\n")
}
