files <- list.files("A:/casact/pcpa-2026", recursive = TRUE, full.names = TRUE)
r_files <- grep("\\.(R|Rmd|qmd)$", files, value = TRUE, ignore.case = TRUE)
cat("Found", length(r_files), "R/Rmd files\n")
for (f in r_files) {
  lines <- readLines(f, warn = FALSE)
  idx <- grep("scaled\\.dev|anova", lines, ignore.case = TRUE)
  if (length(idx) > 0) {
    cat("\nFile:", f, "\n")
    for (i in idx) {
      cat(sprintf("  Line %4d: %s\n", i, lines[i]))
    }
  }
}
