suppressPackageStartupMessages({library(tidyverse); library(scales)})

train <- read.csv("data/train.csv")

# 2D Quartile Grid: Testing for Non-Parallelism (Interaction)
hw_diag <- train |>
  mutate(
    h_q = factor(ntile(pet.Height, 4), labels = paste0("Height_Q", 1:4)),
    w_q = factor(ntile(pet.Weight, 4), labels = paste0("Weight_Q", 1:4))
  ) |>
  summarize(pp = round(sum(total_losses) / sum(policy.Exposure), 1), .by = c(h_q, w_q))

# 1. Empirical Pure Premium Matrix
hw_diag |> pivot_wider(names_from = w_q, values_from = pp) |> print()

# 2. Interaction Curves
p <- ggplot(hw_diag, aes(x = w_q, y = pp, color = h_q, group = h_q)) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Empirical Interaction: pet.Height \u00d7 pet.Weight (Quartiles)",
       subtitle = "Non-parallel / crossing slopes prove the interaction exists in the raw data",
       x = "Weight Quartile", y = "Exposure-Weighted Pure Premium ($)", color = "Height Quartile") +
  theme_minimal()

print(p)
