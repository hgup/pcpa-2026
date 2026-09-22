suppressPackageStartupMessages({library(tidyverse); library(glue)})
train <- read.csv("data/train.csv")
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
  mutate(
    type = if_else(diff > 0, "Adverse (+ Surcharge)", "Favorable (- Discount)"),
    label = glue("{pet.Breed} ({pet.Group})")
  )
print(count(breed_outliers, type))
print(breed_outliers |> select(pet.Breed, type, diff, pct_diff) |> arrange(desc(pct_diff)))
