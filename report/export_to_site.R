#!/usr/bin/env Rscript
# Copy the rendered report into the Quarto website repo.
#   Rscript report/export_to_site.R <path-to-website-repo>
# Writes <site>/_dnam-report-results.md (included by dnam-pipeline-report.qmd)
# and <site>/images/dnam-report/*.png. The site pages are not otherwise touched.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !dir.exists(args[1])) {
  stop("Usage: Rscript report/export_to_site.R <path-to-website-repo>", call. = FALSE)
}
site <- normalizePath(args[1])
md <- "report/dnam_report.md"
if (!file.exists(md)) stop("Render report/dnam_report.qmd first (see its header).", call. = FALSE)

img_dir <- file.path(site, "images", "dnam-report")
dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)
pngs <- list.files("report/figures", pattern = "\\.png$", full.names = TRUE)
file.copy(pngs, img_dir, overwrite = TRUE)

txt <- readLines(md, warn = FALSE)
txt <- gsub("(\\]\\()(report/)?figures/", "\\1images/dnam-report/", txt)
# Quarto's gfm output may start with a YAML block; drop it
if (length(txt) && txt[1] == "---") {
  end <- which(txt == "---")[2]
  txt <- txt[-seq_len(end)]
}
writeLines(txt, file.path(site, "_dnam-report-results.md"))
message("Wrote ", length(pngs), " figures and _dnam-report-results.md to ", site)
