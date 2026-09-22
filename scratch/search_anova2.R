files <- c("4.model.Rmd", "5.diagnostics.Rmd", "F23_VariableSelection.Rmd", "F26_IterativeRefinement.Rmd", "SriCode.R", "Code.R", "submission/finalreport.Rmd")
for (f in files) {
  if (!file.exists(f)) next
  lines <- readLines(f, warn = FALSE)
  idx <- grep("Anova|anova|drop1|scaled", lines)
  if (length(idx) > 0) {
    cat("\n=== File:", f, "===\n")
    for (i in idx) {
      cat(sprintf("  Line %4d: %s\n", i, lines[i]))
    }
  }
}
