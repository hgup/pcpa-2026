pkgs <- c("broom", "car", "statmod", "tweedie", "stats", "gtsummary", "modelsummary", "parameters", "performance")
for (p in pkgs) {
  p_dir <- find.package(p, quiet = TRUE)
  if (length(p_dir) > 0) {
    files <- list.files(p_dir, pattern = "\\.R$|\\.r$", recursive = TRUE, full.names = TRUE)
    for (f in files) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
      if (is.null(lines)) next
      idx <- grep("scaled\\.dev", lines, ignore.case = TRUE)
      if (length(idx) > 0) {
        cat("Package:", p, "- File:", basename(f), "\n")
        for (i in idx) cat(" ", lines[i], "\n")
      }
    }
  }
}
cat("Completed targeted search.\n")
