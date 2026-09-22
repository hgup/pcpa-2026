suppressPackageStartupMessages({library(tidyverse); library(glue)})

train <- read.csv("data/train.csv")

# 1. Benchmark comparison & top 24 outliers overall
breed_outliers <- train |>
  mutate(group_pp = sum(total_losses) / sum(policy.Exposure), .by = pet.Group) |>
  summarize(
    breed_pp   = sum(total_losses) / sum(policy.Exposure),
    group_pp   = group_pp[1],
    diff       = breed_pp - group_pp,
    pct_diff   = diff / group_pp,
    .by        = c(pet.Group, pet.Breed)
  ) |>
  slice_max(abs(pct_diff), n = 24)

# 2. Extract lists of surcharge (adverse) and discount (favorable) breeds
surcharge_breeds <- breed_outliers |>
  filter(pct_diff > 0) |>
  arrange(desc(pct_diff)) |>
  pull(pet.Breed)

favorable_breeds <- breed_outliers |>
  filter(pct_diff < 0) |>
  arrange(pct_diff) |> # most favorable (-75%) first
  pull(pet.Breed)

cat("Surcharge Breeds (", length(surcharge_breeds), "):\n", sep = "")
print(surcharge_breeds)

cat("\nFavorable Breeds (", length(favorable_breeds), "):\n", sep = "")
print(favorable_breeds)
