lines <- readLines("5.diagnostics.Rmd", warn = FALSE)
idx <- grep("qq|q-q|normal|claimant|resid", lines, ignore.case = TRUE)
cat("Found", length(idx), "matches\n")
for (i in head(idx, 20)) {
  cat(sprintf("Line %3d: %s\n", i, lines[i]))
}
