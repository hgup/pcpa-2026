suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

train <- read.csv("data/train.csv")

# Pure premium by height bands
height_curve <- train |>
  mutate(h_band = cut(pet.Height, breaks = seq(0, 35, by = 3), include.lowest = TRUE)) |>
  summarize(
    n_policies = n(),
    exposure   = round(sum(policy.Exposure), 1),
    losses     = sum(total_losses),
    observed_pp = round(sum(total_losses) / sum(policy.Exposure), 2),
    .by = h_band
  ) |>
  filter(n_policies > 50) |>
  arrange(h_band)

print(as.data.frame(height_curve))
