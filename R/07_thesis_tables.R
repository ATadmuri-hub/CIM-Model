
# 07_thesis_tables.R — LaTeX + formatted tables for thesis
library(tidyverse)
library(kableExtra)

DATA_DIR <- "data"
TAB_DIR  <- "tables"
dir.create(TAB_DIR, showWarnings = FALSE)

results_df   <- readRDS(file.path(DATA_DIR, "results_df.rds"))
hyp_tests    <- readRDS(file.path(DATA_DIR, "hypothesis_tests.rds"))

SCENARIOS <- levels(results_df$scenario)

# ---- Table 2: Descriptive statistics ----
desc_table <- results_df %>%
  group_by(scenario) %>%
  summarise(
    N = n(),
    `Retention (%)` = sprintf("%.1f (%.1f)", mean(retention_rate,na.rm=T), sd(retention_rate,na.rm=T)),
    `Motivation`    = sprintf("%.3f (%.3f)", mean(avg_motivation,na.rm=T), sd(avg_motivation,na.rm=T)),
    `Language (CEFR)`= sprintf("%.3f (%.3f)", mean(avg_language_cefr,na.rm=T), sd(avg_language_cefr,na.rm=T)),
    `Cross-Tie`     = sprintf("%.3f (%.3f)", mean(cross_group_tie_ratio,na.rm=T), sd(cross_group_tie_ratio,na.rm=T)),
    `Female Drop%`  = sprintf("%.1f (%.1f)", mean(female_dropout_rate,na.rm=T), sd(female_dropout_rate,na.rm=T)),
    `Male Drop%`    = sprintf("%.1f (%.1f)", mean(male_dropout_rate,na.rm=T), sd(male_dropout_rate,na.rm=T)),
    .groups = "drop"
  )

kable(desc_table, format="latex", booktabs=TRUE, longtable=FALSE,
      caption="Descriptive Statistics by Scenario (Mean, SD in parentheses). $N=100$--$500$ runs per scenario (see Appendix B).") %>%
  kable_styling(font_size=9, latex_options=c("hold_position","scale_down")) %>%
  column_spec(1, bold=TRUE) %>%
  row_spec(1, bold=TRUE, background="#E8F4FD") %>%
  save_kable(file.path(TAB_DIR, "table2_descriptive.tex"))

write_csv(desc_table, file.path(TAB_DIR, "table2_descriptive.csv"))
cat("Saved: table2_descriptive.tex + .csv\n")

# ---- Table 3: Hypothesis tests ----
hyp_display <- hyp_tests %>%
  mutate(
    `Outcome`     = gsub("_", " ", outcome),
    `Baseline M`  = round(mean_base, 3),
    `Alt M`       = round(mean_alt, 3),
    `Diff (%)`    = sprintf("%+.1f%%", diff_pct),
    `95% CI`      = sprintf("[%+.2f, %+.2f]", ci_low, ci_high),
    `t`           = round(t_stat, 2),
    `p (adj)`     = case_when(p_adj_holm<0.001~"<0.001", p_adj_holm<0.01~"<0.01",
                              p_adj_holm<0.05~"<0.05", TRUE~sprintf("%.3f",p_adj_holm)),
    `d`           = round(cohens_d, 2),
    `Sig`         = ifelse(sig_holm, "***", "")
  ) %>%
  select(scenario, Outcome, `Baseline M`, `Alt M`, `Diff (%)`, `95% CI`, t, `p (adj)`, d, Sig)

kable(hyp_display, format="latex", booktabs=TRUE, longtable=TRUE,
      caption="Welch's t-test Results vs. Baseline (Holm corrected). *** $p<0.05$ (Holm-adjusted).") %>%
  kable_styling(font_size=8, latex_options=c("hold_position","repeat_header")) %>%
  save_kable(file.path(TAB_DIR, "table3_hypothesis_tests.tex"))

write_csv(hyp_display, file.path(TAB_DIR, "table3_hypothesis_tests.csv"))
cat("Saved: table3_hypothesis_tests.tex + .csv\n")

# ---- Appendix D: full 132-test table (table_full_tests_appendix.tex) ----
# Consumed by \input{} in Appendix D of the thesis. Regenerated here from
# hypothesis_tests.rds so it cannot drift stale (it previously did: a frozen
# pre-data-regeneration snapshot showed Baseline 44.455 instead of 45.067).
# Row order, family labels (C/E/R) and outcome order are fixed to match the
# thesis layout; values come straight from hyp_tests.
local({
  ht <- hyp_tests
  order_C <- c("Minimal Support", "No Indoor Continuity", "Suboptimal Composition", "Weak Peer Influence")
  order_E <- c("BuddyProgram", "High SES Heterogeneity", "Low Park Density", "NoIndoor Minimal",
               "RotatingGroups", "Targeting50", "Targeting70", "Targeting90", "Winter50",
               "Women-Only Groups", "WomenChildcare")
  order_R <- c("CentralityBuddy", "Composition2", "Composition3", "Composition4",
               "OpenPopulation", "RandomBuddy", "SuboptimalOpen")
  scen_order <- c(order_C, order_E, order_R)
  fam_of <- c(setNames(rep("C", length(order_C)), order_C),
              setNames(rep("E", length(order_E)), order_E),
              setNames(rep("R", length(order_R)), order_R))
  out_order <- c("retention_rate", "avg_motivation", "avg_language_cefr",
                 "cross_group_tie_ratio", "female_dropout_rate", "male_dropout_rate")
  out_label <- c(retention_rate = "Retention", avg_motivation = "Motivation",
                 avg_language_cefr = "Language", cross_group_tie_ratio = "Cross-tie",
                 female_dropout_rate = "F.dropout", male_dropout_rate = "M.dropout")
  fmt_p <- function(p) {
    if (is.na(p)) return("--")
    if (p < 0.001) return("<0.001***")
    s <- sprintf("%.3f", p)
    if (p < 0.01) return(paste0(s, "**"))
    if (p < 0.05) return(paste0(s, "*"))
    s
  }
  hdr <- c(
    "\\begingroup\\fontsize{6}{8}\\selectfont", "",
    "\\begin{longtable}[t]{lllllllll}",
    "\\caption{Full hypothesis test results: 132 Welch $t$-tests across 22 non-baseline scenarios and 6 outcomes. Holm correction by family (C/E/R; see prose above). $^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$.}",
    "\\label{tab:full_tests}\\\\", "\\toprule",
    "Scenario & Fam & Outcome & Base & Alt & Diff & CI(diff) & d & $p_{Holm}$\\\\", "\\midrule", "\\endfirsthead",
    "\\caption[]{Full hypothesis test results \\textit{(continued)}}\\\\", "\\toprule",
    "Scenario & Fam & Outcome & Base & Alt & Diff & CI(diff) & d & $p_{Holm}$\\\\", "\\midrule", "\\endhead", "",
    "\\endfoot", "\\bottomrule", "\\endlastfoot")
  lines <- hdr
  for (si in seq_along(scen_order)) {
    sc <- scen_order[si]; fm <- fam_of[sc]
    for (oi in seq_along(out_order)) {
      oc <- out_order[oi]
      row <- ht[ht$scenario == sc & ht$outcome == oc, ]
      if (nrow(row) != 1) stop(sprintf("Appendix D: missing/dup row %s / %s (n=%d)", sc, oc, nrow(row)))
      name_cell <- if (oi == 1) sc else " "
      lines <- c(lines, sprintf(
        "%s & %s & %s & %.3f & %.3f & %s%s & [%+.2f, %+.2f] & %+.2f & %s\\\\",
        name_cell, fm, out_label[[oc]], row$mean_base, row$mean_alt,
        sprintf("%+.1f", row$diff_pct), "\\%",
        row$ci_low, row$ci_high, row$cohens_d, fmt_p(row$p_adj_holm)))
    }
    if (si < length(scen_order)) lines <- c(lines, "\\midrule")
  }
  lines <- c(lines, "\\end{longtable}", "\\endgroup", "")
  writeLines(lines, file.path(TAB_DIR, "table_full_tests_appendix.tex"))
  cat(sprintf("Saved: table_full_tests_appendix.tex (%d data rows)\n",
              sum(grepl("& [CER] &", lines))))
})

cat("All tables saved to:", TAB_DIR, "\n")
