files <- list.files("A:/casact/pcpa-2026", recursive = TRUE, full.names = TRUE)
for (f in files) {
  if (grepl("\\.(png|jpg|csv|RData)$", f)) next
  lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) next
  idx <- grep("policy-data|removed|ranges reflects", lines, ignore.case = TRUE)
  if (length(idx) > 0) {
    cat("File:", f, "\n")
    for (i in head(idx, 5)) cat("  Line", i, ":", lines[i], "\n")
  }
}
