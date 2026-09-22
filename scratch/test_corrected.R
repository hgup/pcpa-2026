suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(scales)
})

train <- read.csv("data/train.csv")

# 1. Compute group benchmarks & extract top 24 outlier breeds overall
breed_signal <- train |>
  mutate(group_pp = sum(total_losses) / sum(policy.Exposure), .by = pet.Group) |>
  summarize(
    n_policies = n(),
    breed_pp   = sum(total_losses) / sum(policy.Exposure),
    group_pp   = group_pp[1],
    diff       = breed_pp - group_pp,
    pct_diff   = diff / group_pp,
    .by        = c(pet.Group, pet.Breed)
  ) |>
  slice_max(abs(pct_diff), n = 24)

# 2. Extract lists of surcharge (adverse) and discount (favorable) breeds
surcharge_breeds <- breed_signal |>
  filter(diff > 0) |>
  arrange(desc(abs(pct_diff))) |>
  pull(pet.Breed)

discount_breeds <- breed_signal |>
  filter(diff < 0) |>
  arrange(desc(abs(pct_diff))) |>
  pull(pet.Breed)

print(surcharge_breeds)
print(discount_breeds)

# 3. Stacked Dumbbell Deviation Chart
# Top: Adverse (+ Surcharge) descending by abs(pct_diff)
# Bottom: Favorable (- Discount) stacked below, descending by abs(pct_diff)
pal <- c(
  "Group Benchmark"        = "#5B9BD5",
  "Adverse (+ Surcharge)"  = "#C00000",
  "Favorable (- Discount)" = "#385723"
)

p <- breed_signal |>
  arrange(diff > 0, abs(pct_diff)) |>
  mutate(
    type        = if_else(diff > 0, "Adverse (+ Surcharge)", "Favorable (- Discount)"),
    breed_label = fct_inorder(glue("{pet.Breed} ({pet.Group})")),
    label_text  = sprintf("%+0.0f%%", pct_diff * 100)
  ) |>
  ggplot(aes(y = breed_label)) +
  geom_segment(aes(x = group_pp, xend = breed_pp, yend = breed_label), color = "grey65", linewidth = 1.2) +
  geom_point(aes(x = group_pp, fill = "Group Benchmark"), shape = 21, size = 3.5, stroke = 1) +
  geom_point(aes(x = breed_pp, fill = type), shape = 21, size = 3.8, stroke = 1) +
  geom_text(
    aes(x = pmax(group_pp, breed_pp) + 25, label = label_text, color = type),
    size = 3.5, fontface = "bold", hjust = 0
  ) +
  scale_fill_manual(name = "Premium Level", values = pal) +
  scale_color_manual(values = pal, guide = "none") +
  scale_x_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Top Outlier Breeds vs. Parent Group Benchmark Pure Premium",
    subtitle = "Top: Adverse Surcharges (descending % magnitude) | Bottom: Favorable Discounts (descending % magnitude)",
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

png("scratch/test_corrected_plot.png", width = 900, height = 750)
print(p)
dev.off()
cat("Success\n")
