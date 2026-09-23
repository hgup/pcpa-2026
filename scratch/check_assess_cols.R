a <- read.csv("data/assessment-data.csv", nrows = 5)
print(names(a))
cat("Has policy.Exposure:", "policy.Exposure" %in% names(a), "\n")
