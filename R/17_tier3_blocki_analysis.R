# CIM v6.5/v6.6 — Tier 3 Block I: second-domain ranking invariance
#
# Analysis:
#   T3.5 2x16 ranking matrix across Istanbul-calisthenics (v6.4 data) and Berlin
#        language-course (new data).
#   T3.6 Ranking-invariance test: is BuddyProgram > Baseline > Suboptimal
#        preserved across domains?
#
# Required data (run BehaviorSpace on both configs first):
#   data/                          ← v6.4 Istanbul calisthenics (existing)
#   data/second_domain/            ← v6.5 Berlin language-course (NEW, from user runs)
#
# The Berlin run is T3.4: 12 of 16 scenarios (NoIndoor, NoIndoorMinimal, Winter50 flagged N/A).

library(tidyverse)
library(ggplot2)
source("R/constants.R")

DATA_DIR <- "data"
OUT_DIR  <- "outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, showWarnings = FALSE)

# ============================================================================
# Load both domains' data
# ============================================================================
istanbul <- tryCatch(
  readRDS(file.path(DATA_DIR, "results_df.rds")) %>%
    select(scenario, retention_rate, avg_motivation, cross_group_tie_ratio) %>%
    mutate(domain = "Istanbul-calisthenics"),
  error = function(e) { cat("ERROR: Istanbul data not loaded\n"); NULL }
)

berlin_dir <- file.path(DATA_DIR, "second_domain")
berlin <- if (dir.exists(berlin_dir)) {
  files <- list.files(berlin_dir, pattern = "CIM_results_.*\\.csv",
                      full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    cat("No Berlin data yet; run Block I experiments first.\n")
    NULL
  } else {
    map_dfr(files, function(f) {
      d <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c"))
      tibble(
        scenario = d$value[d$metric == "scenario"],
        retention_rate = as.numeric(d$value[d$metric == "retention_rate"]),
        avg_motivation = as.numeric(d$value[d$metric == "avg_motivation"]),
        cross_group_tie_ratio = as.numeric(d$value[d$metric == "cross_group_tie_ratio"])
      )
    }) %>% mutate(domain = "Berlin-language-course")
  }
} else {
  cat("Berlin config not yet run; skipping Block I analysis.\n"); NULL
}

if (is.null(istanbul) || is.null(berlin)) {
  cat("\nSkipping Block I. Ensure both domains' data are loaded before rerunning.\n")
  quit(save = "no")
}

combined <- bind_rows(istanbul, berlin)

# ============================================================================
# T3.5 2x16 ranking matrix hero figure
# ============================================================================
na_scenarios <- c("No Indoor Continuity", "NoIndoor Minimal", "Winter50")

ranking <- combined %>%
  group_by(domain, scenario) %>%
  summarise(retention = mean(retention_rate, na.rm = TRUE), .groups = "drop") %>%
  group_by(domain) %>%
  mutate(rank = rank(-retention, ties.method = "min"),
         na_flag = scenario %in% na_scenarios & domain == "Berlin-language-course") %>%
  ungroup()

p <- ggplot(ranking, aes(x = reorder(scenario, -rank, FUN = mean),
                         y = domain,
                         fill = retention,
                         label = ifelse(na_flag, "N/A", sprintf("%.1f", retention)))) +
  geom_tile(aes(alpha = ifelse(na_flag, 0.3, 1.0))) +
  geom_text(size = 3) +
  scale_fill_viridis_c(name = "Retention %", na.value = "grey70") +
  scale_alpha_identity() +
  labs(
    title = "Ranking invariance across two community-integration domains",
    subtitle = "Each cell shows mean retention rate; N/A = scenario not applicable in that domain",
    x = NULL, y = NULL,
    caption = NULL
  ) +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

ggsave(file.path(OUT_DIR, "hero_figure_2x16_ranking.pdf"), p, width = 14, height = 4.5)
ggsave(file.path(OUT_DIR, "hero_figure_2x16_ranking.png"), p, width = 14, height = 4.5, dpi = 300)
cat("Hero figure saved: outputs/hero_figure_2x16_ranking.{pdf,png}\n")

# ============================================================================
# T3.6 Ranking-invariance test
# ============================================================================
test_pairs <- tribble(
  ~weaker, ~stronger,
  "Suboptimal Composition", "Baseline",
  "Suboptimal Composition", "BuddyProgram",
  "Minimal Support", "Baseline",
  "Baseline", "BuddyProgram",
  "NoIndoor Minimal", "Baseline"
)

invariance <- map_dfr(c("Istanbul-calisthenics", "Berlin-language-course"), function(d) {
  sub <- ranking %>% filter(domain == d)
  pmap_dfr(test_pairs, function(weaker, stronger) {
    w <- sub$retention[sub$scenario == weaker]
    s <- sub$retention[sub$scenario == stronger]
    if (length(w) == 0 || length(s) == 0) return(NULL)
    tibble(domain = d, pair = paste(weaker, "<", stronger),
           weaker_retention = w, stronger_retention = s,
           relation_holds = s > w)
  })
})
print(invariance)
saveRDS(invariance, file.path(OUT_DIR, "t3_6_ranking_invariance.rds"))

summary_by_pair <- invariance %>%
  group_by(pair) %>%
  summarise(domains_tested = n(),
            both_domains = domains_tested == 2,
            consistent_across_domains = if (domains_tested == 2) all(relation_holds) else NA,
            .groups = "drop")
cat("\nRanking-invariance summary:\n")
print(summary_by_pair)

n_tested_both <- sum(summary_by_pair$both_domains)
n_consistent <- sum(summary_by_pair$consistent_across_domains, na.rm = TRUE)

cat(sprintf("\nFully-testable pairs (present in BOTH domains): %d\n", n_tested_both))
cat(sprintf("Consistent across both domains: %d of %d\n", n_consistent, n_tested_both))

if (n_consistent == n_tested_both && n_tested_both > 0) {
  cat(sprintf("\nFINDING: Framework-generality claim SUPPORTED for the %d fully-testable ranking relations.\n", n_tested_both))
} else {
  cat(sprintf("\nFINDING: Framework-generality claim WEAKENED. %d of %d fully-testable ranking relations hold across both domains.\n", n_consistent, n_tested_both))
  cat("Inconsistent pairs require Discussion narrative on domain-specific deviations.\n")
}

cat("\n=== Tier 3 Block I analysis complete ===\n")
