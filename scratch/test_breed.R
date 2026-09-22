suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(scales)
})

train <- read.csv("data/train.csv")

# 1. Benchmark comparison & top 24 outliers by absolute % deviation
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
    type        = if_else(diff > 0, "Adverse (+ Surcharge)", "Favorable (- Discount)"),
    breed_label = reorder(glue("{pet.Breed} ({pet.Group})"), diff),
    label_text  = sprintf("%+0.0f%%", pct_diff * 100)
  )

# 2. Dumbbell deviation plot
pal <- c("Group Benchmark" = "#5B9BD5", "Adverse (+ Surcharge)" = "#C00000", "Favorable (- Discount)" = "#385723")

p <- ggplot(breed_outliers, aes(y = breed_label)) +
  geom_segment(aes(x = group_pp, xend = breed_pp, yend = breed_label), color = "grey65", linewidth = 1.2) +
  geom_point(aes(x = group_pp, fill = "Group Benchmark"), shape = 21, size = 3.5, stroke = 1) +
  geom_point(aes(x = breed_pp, fill = type), shape = 21, size = 3.8, stroke = 1) +
  geom_text(aes(x = pmax(group_pp, breed_pp) + 25, label = label_text, color = type),
            size = 3.5, fontface = "bold", hjust = 0) +
  scale_fill_manual(name = "Premium Level", values = pal) +
  scale_color_manual(values = pal, guide = "none") +
  scale_x_continuous(labels = dollar_format(), expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Top Outlier Breeds vs. Parent Group Benchmark Pure Premium",
    subtitle = "Blue = Expected Group PP | Colored = Actual Breed PP (with % deviation)",
    x        = "Pure Premium ($)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey80"),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 10)
  )

png("scratch/test_breed_plot.png", width = 900, height = 700)
print(p)
dev.off()
cat("Success\n")
