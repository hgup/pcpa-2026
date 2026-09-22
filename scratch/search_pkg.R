lib_paths <- .libPaths()
for (lib in lib_paths) {
  files <- list.files(lib, pattern = "\\.R$|\\.r$", recursive = TRUE, full.names = TRUE)
  for (f in files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) next
    idx <- grep("scaled\\.dev", lines, ignore.case = TRUE)
    if (length(idx) > 0) {
      cat("Found in package file:", f, "\n")
      for (i in idx) cat(lines[i], "\n")
    }
  }
}
cat("Done searching libraries.\n")
