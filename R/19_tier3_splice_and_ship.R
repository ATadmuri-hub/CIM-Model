# CIM v6.4: Tier 3 assembly (archival build script).
#
# One-time development pipeline that ran the Tier 3 analyses (R/16, R/17, R/18), merged
# the resulting framework-generality section into the thesis, re-rendered the PDF and
# docx, and packaged the release bundle. Retained for provenance: its inputs are
# intermediate build artefacts not shipped in this repository, so it is not part of the
# reproducible analysis pipeline (see R/00_run_all.R).
#
# Usage: Rscript R/19_tier3_splice_and_ship.R

library(tidyverse)

OUT_DIR <- "outputs"
PLANNING <- "_build"
DELIVERY <- file.path(PLANNING, "release_bundle")

dir.create(DELIVERY, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------------
# Step 1: Run analysis pipeline
# ----------------------------------------------------------------------------
cat("Step 1: Running analysis pipeline...\n")
for (script in c("R/16_tier3_blockj_analysis.R",
                 "R/17_tier3_blocki_analysis.R",
                 "R/18_tier3_integrate.R")) {
  cat("  ", script, "\n", sep = "")
  tryCatch(source(script, echo = FALSE),
           error = function(e) cat("    ERROR:", conditionMessage(e), "\n"))
}

# ----------------------------------------------------------------------------
# Step 2: Read filled template
# ----------------------------------------------------------------------------
filled_path <- file.path(PLANNING, "t3_6_discussion_filled.md")
if (!file.exists(filled_path)) {
  stop("Filled template not found. R/18 must succeed before splicing.")
}
filled_md <- readLines(filled_path)

# ----------------------------------------------------------------------------
# Step 3: Splice into thesis Rmd
# ----------------------------------------------------------------------------
rmd_path <- "thesis_CIM_v6.Rmd"
rmd <- readLines(rmd_path)

# Insert point: before "# Mechanism Robustness Check"
anchor <- grep("^# Mechanism Robustness Check", rmd)[1]
if (is.na(anchor)) stop("Mechanism Robustness anchor not found in Rmd")

marker <- "<!-- TIER 3 INSERT START -->"
if (any(grepl(marker, rmd))) {
  cat("  Rmd already contains Tier 3 insert; skipping splice.\n")
} else {
  # Extract just the "Filled values" block from the filled template (skip re-insert of template)
  block_start <- grep("^## Filled values", filled_md)[1]
  block_end <- grep("^## Source template with placeholders", filled_md)[1] - 1
  if (!is.na(block_start) && !is.na(block_end)) {
    insert_lines <- c(
      "",
      marker,
      "",
      "# Framework generality and robustness findings (Tier 3)",
      "",
      filled_md[block_start:block_end],
      "",
      "<!-- TIER 3 INSERT END -->",
      ""
    )
    rmd <- c(rmd[1:(anchor-1)], insert_lines, rmd[anchor:length(rmd)])
    writeLines(rmd, rmd_path)
    cat("  Thesis Rmd spliced at line ", anchor, "\n", sep = "")
  }
}

# ----------------------------------------------------------------------------
# Step 4: Render PDF + docx
# ----------------------------------------------------------------------------
cat("Step 4: Rendering PDF + docx (~3-5 min)...\n")
tryCatch({
  rmarkdown::render(rmd_path, output_format = "pdf_document",
                    output_file = "thesis_CIM_v6.pdf", quiet = TRUE)
  rmarkdown::render(rmd_path, output_format = "word_document",
                    output_file = "thesis_CIM_v6.docx", quiet = TRUE)
  cat("  PDF + docx rendered\n")
}, error = function(e) cat("  ERROR in render:", conditionMessage(e), "\n"))

# ----------------------------------------------------------------------------
# Step 5: Generate CHANGELOG.md
# ----------------------------------------------------------------------------
cat("Step 5: Writing CHANGELOG...\n")
tpl_path <- file.path(PLANNING, "CHANGELOG_template.md")
if (file.exists(tpl_path)) {
  changelog <- readLines(tpl_path)
  # For now, just write the template as-is (user can fine-tune numbers manually)
  writeLines(changelog, "CHANGELOG.md")
  cat("  CHANGELOG.md written (bracketed placeholders may need manual fills)\n")
}

# ----------------------------------------------------------------------------
# Step 6: Package ship bundle
# ----------------------------------------------------------------------------
cat("Step 6: Packaging ship bundle at ", DELIVERY, "...\n", sep = "")
for (src in c("thesis_CIM_v6.Rmd", "thesis_CIM_v6.pdf", "thesis_CIM_v6.docx",
              "references.bib", "CIM_v6_4.nlogo",
              "CHANGELOG.md")) {
  if (file.exists(src)) file.copy(src, DELIVERY, overwrite = TRUE)
}
for (d in c("config", "outputs")) {
  if (dir.exists(d)) file.copy(d, DELIVERY, recursive = TRUE)
}
cat("Done. Ship bundle ready at ", DELIVERY, "\n", sep = "")
