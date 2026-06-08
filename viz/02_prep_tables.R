#!/usr/bin/env Rscript
# viz/02_prep_tables.R -- dashboard v2 JSON from the PINNED authoritative tables.
# No model run, no new statistics: means/SD/SE/CI/significance are read verbatim from
# tables/*.csv (the verified pipeline). CIs shown = mean +/- 1.96*SE, SE from the table's
# own reported SD/n (a transparent normal-approx of the reported summary stats).
suppressMessages(library(jsonlite))
if (nzchar(Sys.getenv("CIM_ROOT"))) setwd(Sys.getenv("CIM_ROOT"))  # otherwise run from the repo root
dir.create("viz/data", showWarnings = FALSE, recursive = TRUE)

ce  <- read.csv("tables/table_cost_effectiveness.csv", check.names = FALSE, stringsAsFactors = FALSE)
eq  <- read.csv("tables/table_equity_gender.csv",      check.names = FALSE, stringsAsFactors = FALSE)
d2  <- read.csv("tables/table2_descriptive.csv",       check.names = FALSE, stringsAsFactors = FALSE)
t3  <- read.csv("tables/table3_hypothesis_tests.csv",  check.names = FALSE, stringsAsFactors = FALSE)

# parse "0.712 (0.051)" -> c(mean, sd)
pms <- function(s) {
  m <- regmatches(s, regexec("([0-9.]+)\\s*\\(([0-9.]+)\\)", s))[[1]]
  if (length(m) == 3) as.numeric(m[2:3]) else c(NA, NA)
}
mot <- t(sapply(d2$Motivation, pms));      rownames(mot) <- d2$scenario
tie2<- t(sapply(d2$`Cross-Tie`, pms));      rownames(tie2)<- d2$scenario
cef2<- t(sapply(d2$`Language (CEFR)`, pms));rownames(cef2)<- d2$scenario

confirmatory <- c("Baseline","Weak Peer Influence","Suboptimal Composition","No Indoor Continuity","Minimal Support")
robustness   <- c("Composition2","Composition3","Composition4","OpenPopulation","SuboptimalOpen","CentralityBuddy","RandomBuddy")
fam <- function(s) if (s %in% confirmatory) "confirmatory" else if (s %in% robustness) "robustness" else "exploratory"

t3r <- t3[t3$Outcome == "retention rate", ]
cilo <- function(m, se) m - 1.96*se
cihi <- function(m, se) m + 1.96*se
# per-metric Holm significance (so stars match the displayed metric, not always retention)
metricToOutcome <- c(retention="retention rate", tie="cross group tie ratio",
                     cefr="avg language cefr", motivation="avg motivation")
sigfor <- function(s, mk) {
  if (s == "Baseline") return("ref")
  r <- t3[t3$Outcome == metricToOutcome[[mk]] & t3$scenario == s, ]
  if (nrow(r)) as.character(r$Sig) else ""
}

scen <- lapply(seq_len(nrow(ce)), function(i) {
  s <- ce$scenario[i]; n <- ce$n[i]
  ret <- ce$mean_ret[i]; ret_se <- ce$se_ret[i]
  tie <- ce$mean_tie[i]; tie_se <- tie2[s,2]/sqrt(n)
  cef <- ce$mean_lang[i]; cef_se <- cef2[s,2]/sqrt(n)
  mo  <- mot[s,1]; mo_se <- mot[s,2]/sqrt(n)
  e   <- eq[eq$scenario == s, ]
  tr  <- t3r[t3r$scenario == s, ]
  list(
    scenario = s, family = fam(s), n = n,
    retention = round(ret,2), retention_lo = round(cilo(ret,ret_se),2), retention_hi = round(cihi(ret,ret_se),2),
    tie = round(tie,3), tie_lo = round(cilo(tie,tie_se),3), tie_hi = round(cihi(tie,tie_se),3),
    cefr = round(cef,3), cefr_lo = round(cilo(cef,cef_se),3), cefr_hi = round(cihi(cef,cef_se),3),
    cost = round(ce$mean_cost[i]), cost_lo = ce$ci_lo[i], cost_hi = ce$ci_hi[i],
    motivation = round(mo,3), motivation_lo = round(cilo(mo,mo_se),3), motivation_hi = round(cihi(mo,mo_se),3),
    gender_gap = if(nrow(e)) e$gender_gap else NA,
    gap_lo = if(nrow(e)) e$ci_lo else NA, gap_hi = if(nrow(e)) e$ci_hi else NA,
    sig = if (s == "Baseline") "ref" else if (nrow(tr)) tr$Sig else "",
    sigm = list(retention = sigfor(s,"retention"), tie = sigfor(s,"tie"),
                cefr = sigfor(s,"cefr"), motivation = sigfor(s,"motivation")),
    d   = if (nrow(tr)) tr$d else NA,
    p_adj = if (nrow(tr)) as.character(tr$`p (adj)`) else ""
  )
})
write_json(scen, "viz/data/scenarios.json", auto_unbox = TRUE, digits = 4, na = "null")
cat("scenarios.json:", length(scen), "scenarios | Baseline retention =",
    scen[[which(ce$scenario=="Baseline")]]$retention, "(must be ~45.1)\n")

v <- read.csv("tables/table1_validation.csv", check.names = FALSE, stringsAsFactors = FALSE)
names(v) <- c("pattern","unit","target","source","model","status")
write_json(v, "viz/data/validation.json", dataframe = "rows", auto_unbox = TRUE)
cat("validation.json:", nrow(v), "targets\n")

dr <- read.csv("tables/table_dose_response.csv", check.names = FALSE, stringsAsFactors = FALSE)
write_json(dr, "viz/data/dose.json", dataframe = "rows", auto_unbox = TRUE, digits = 4)
cat("dose.json:", nrow(dr), "doses\n")

bd <- read.csv("tables/table_centrality_buddy.csv", check.names = FALSE, stringsAsFactors = FALSE)
bd <- bd[, c("scenario_a","scenario_b","mean_a","mean_b","diff","cohens_d","p_adj_holm","sig_holm")]
write_json(bd, "viz/data/buddy.json", dataframe = "rows", auto_unbox = TRUE, digits = 4)
cat("buddy.json:", nrow(bd), "comparisons\nDONE.\n")
