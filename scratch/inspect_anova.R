suppressPackageStartupMessages({library(statmod); library(tweedie)})
lines <- deparse(stats:::anova.glmlist)
idx <- grep("scaled|Deviance|dispersion", lines, ignore.case = TRUE)
for (i in idx) cat(lines[i], "\n")
