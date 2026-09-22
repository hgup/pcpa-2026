suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(scales)
})

train <- read.csv("data/train.csv")

# 1. Benchmark comparison & top 24 outliers
breed_outliers <- train |>
  mutate(group_pp = sum(total_losses) / sum(policy.Exposure), .by = pet.Group) |>
  summarize(
    breed_pp   = sum(total_losses) / sum(policy.Exposure),
    group_pp   = group_pp[1],
    diff       = breed_pp - group_pp,
    pct_diff   = diff / group_pp,
    .by        = c(pet.Group, pet.Breed)
  ) |>
  slice_max(abs(pct_diff), n = 24) |>
  # Order: Favorable at bottom, Adverse at top ordered descending by abs(pct_diff) from top to bottom
  arrange(diff > 0, abs(pct_diff)) |>
  mutate(
    type        = if_else(diff > 0, "Adverse (+ Surcharge)", "Favorable (- Discount)"),
    breed_label = fct_inorder(glue("{pet.Breed} ({pet.Group})")),
    label_text  = sprintf("%+0.0f%%", pct_diff * 100)
  )

print(levels(breed_outliers$breed_label))
