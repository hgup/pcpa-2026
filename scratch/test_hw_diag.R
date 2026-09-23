suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(glue)
})

train <- read.csv("data/train.csv")

# 1. Diagnostic function for continuous x continuous interaction
check_int_height_weight <- function(df = train) {
  # Discretize into 3 meaningful stature and weight tiers
  binned_df <- df |>
    mutate(
      height_band = cut(
        pet.Height,
        breaks         = c(0, 12, 22, 40),
        labels         = c("Short (<=12 in)", "Medium (12-22 in)", "Tall (>22 in)"),
        include.lowest = TRUE
      ),
      weight_band = cut(
        pet.Weight,
        breaks         = c(0, 20, 50, 250),
        labels         = c("Light (<=20 lbs)", "Medium (20-50 lbs)", "Heavy (>50 lbs)"),
        include.lowest = TRUE
      )
    )

  # Empirical cross-tabulation (Exposure-weighted Pure Premium)
  grid_summary <- binned_df |>
    summarize(
      n_policies  = n(),
      exposure    = round(sum(policy.Exposure), 1),
      pure_prem   = round(sum(total_losses) / sum(policy.Exposure), 2),
      .by         = c(height_band, weight_band)
    ) |>
    drop_na()

  # Print matrix view
  cat("--- Empirical Exposure-Weighted Pure Premium Matrix ($) ---\n")
  grid_matrix <- grid_summary |>
    select(height_band, weight_band, pure_prem) |>
    pivot_wider(names_from = weight_band, values_from = pure_prem) |>
    arrange(height_band)
  print(as.data.frame(grid_matrix))

  # Interaction plot: Testing for non-parallelism (fanning / crossing slopes)
  p <- ggplot(grid_summary, aes(x = weight_band, y = pure_prem, color = height_band, group = height_band)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    scale_y_continuous(labels = dollar_format()) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title    = "Empirical Diagnostic: pet.Height \u00d7 pet.Weight Interaction",
      subtitle = "Diverging / non-parallel slopes confirm that weight risk scales differently by animal stature",
      x        = "Weight Tier",
      y        = "Exposure-Weighted Pure Premium ($)",
      color    = "Height Tier"
    ) +
    theme_minimal() +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))

  print(p)
  invisible(grid_matrix)
}

check_int_height_weight(train)
