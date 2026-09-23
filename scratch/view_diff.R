d <- system("git diff --no-index Code.R scratch/recovered_Code_76e74948.R", intern = TRUE)
cat(head(d, 120), sep = "\n")
