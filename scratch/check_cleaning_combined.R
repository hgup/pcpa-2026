files <- c("2.cleaning-combined.Rmd", "submission/finalreport.Rmd", "report.Rmd", "submission/final_code.R", "Code.R")
for (f in files) {
  if (!file.exists(f)) next
  lines <- readLines(f, warn = FALSE)
  idx <- grep("policy|721|72865|72144|remove|dedup", lines, ignore.case = TRUE)
  cat("\n=== File:", f, "===\n")
  for (i in head(idx, 15)) {
    cat(sprintf("  Line %4d: %s\n", i, lines[i]))
  }
}
