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
  slice_max(abs(pct_diff), n = 24)

# We want:
# Viewed from TOP to BOTTOM on the y-axis:
# 1. Adverse breeds: largest abs(pct_diff) -> smallest abs(pct_diff)
# 2. Favorable breeds: largest abs(pct_diff) -> smallest abs(pct_diff)
#
# Because ggplot puts level 1 at the BOTTOM and level N at the TOP:
# The bottom-to-top order must be:
# - Favorable: smallest abs(pct_diff) -> largest abs(pct_diff)
# - Adverse: smallest abs(pct_diff) -> largest abs(pct_diff)

ordered_df <- breed_outliers |>
  mutate(is_adverse = diff > 0) |>
  arrange(is_adverse, abs(pct_diff)) |>
  mutate(
    type        = if_else(is_adverse, "Adverse (+ Surcharge)", "Favorable (- Discount)"),
    breed_label = fct_inorder(glue("{pet.Breed} ({pet.Group})")),
    label_text  = sprintf("%+0.0f%%", pct_diff * 100)
  )

cat("Order as plotted from TOP to BOTTOM:\n")
rev_levels <- rev(levels(ordered_df$breed_label))
pct_labels <- ordered_df |> select(breed_label, label_text) |> deframe()
for (i in seq_along(rev_levels)) {
  lbl <- rev_levels[i]
  cat(sprintf("%2d. %-45s %s\n", i, lbl, pct_labels[lbl]))
}
