suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

train <- read.csv("data/train.csv")

# Create 4 bands for Height and 4 bands for Weight
grid_data <- train |>
  mutate(
    h_bin = cut(pet.Height, breaks = quantile(pet.Height, probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = c("Q1 (Short)", "Q2 (Med-Low)", "Q3 (Med-High)", "Q4 (Tall)")),
    w_bin = cut(pet.Weight, breaks = quantile(pet.Weight, probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = c("Q1 (Light)", "Q2 (Med-Light)", "Q3 (Med-Heavy)", "Q4 (Heavy)"))
  ) |>
  group_by(h_bin, w_bin) |>
  summarize(
    n_policies = n(),
    total_exposure = sum(policy.Exposure),
    observed_pp = round(sum(total_losses) / sum(policy.Exposure), 2),
    .groups = "drop"
  )

cat("=== Empirical Pure Premium Matrix (Height Bins x Weight Bins) ===\n")
grid_matrix <- grid_data |>
  select(h_bin, w_bin, observed_pp) |>
  pivot_wider(names_from = w_bin, values_from = observed_pp)
print(as.data.frame(grid_matrix))

# Calculate the slope / difference across weight for Short vs Tall
cat("\nWeight Effect (Q4 Heavy vs Q1 Light) by Height Band:\n")
diff_tbl <- grid_matrix |>
  mutate(Heavy_minus_Light = `Q4 (Heavy)` - `Q1 (Light)`) |>
  select(h_bin, `Q1 (Light)`, `Q4 (Heavy)`, Heavy_minus_Light)
print(as.data.frame(diff_tbl))
