suppressPackageStartupMessages({
  library(tidyverse)
  library(rcompanion)
  library(corrplot)
})

train <- read.csv("data/train.csv")

# Select true categorical rating variables (exclude IDs, dates, high cardinality)
cat_vars <- train |>
  select(any_of(c(
    "policy.NBRB", "pet.Type", "pet.Group", "pet.Gender",
    "pet.Offspring", "pet.Microchip", "pet.Origin",
    "owner.Gender", "owner.Location", "owner.Channel"
  ))) |>
  mutate(across(everything(), as.factor))

cat("Categorical variables:\n")
print(names(cat_vars))

# Compute pairwise Cramer's V matrix
calc_cramer_matrix <- function(df) {
  cols <- names(df)
  n <- length(cols)
  mat <- matrix(1, nrow = n, ncol = n, dimnames = list(cols, cols))
  
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      val <- rcompanion::cramerV(df[[i]], df[[j]], bias.correct = TRUE)
      mat[i, j] <- val
      mat[j, i] <- val
    }
  }
  mat
}

cramer_mat <- calc_cramer_matrix(cat_vars)
cat("\nCramer's V Matrix (rounded):\n")
print(round(cramer_mat, 2))
