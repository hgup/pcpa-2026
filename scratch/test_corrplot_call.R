suppressPackageStartupMessages({library(corrplot)})
m <- matrix(c(1, 0.25, 0.25, 1), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
png("scratch/test_corrplot.png")
corrplot(m, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.8,
         is.corr = FALSE, col.lim = c(0, 1),
         title = "Cramer's V - Categorical Predictors", mar = c(0, 0, 2, 0))
dev.off()
cat("Corrplot rendered successfully without warnings.\n")
