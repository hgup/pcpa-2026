suppressPackageStartupMessages(library(tidyverse))
lines <- readLines("work.ipynb", warn = FALSE)
idx <- grep("policy.*(remove|drop|filter|dupe|expos)", lines, ignore.case = TRUE)
for (i in head(idx, 15)) {
  cat("Line", i, ":", lines[i], "\n")
}
