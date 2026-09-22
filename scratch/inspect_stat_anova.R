lines <- deparse(stats:::stat.anova)
idx <- grep("scaled|Deviance|col|names", lines, ignore.case = TRUE)
for (i in idx) cat(lines[i], "\n")
