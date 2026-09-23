suppressPackageStartupMessages({library(tidyverse); library(janitor)})
raw_policy <- read.csv("data/policy-data.csv")
cat("Total rows in raw policy:", nrow(raw_policy), "\n")
cat("Unique policy.IDs:", n_distinct(raw_policy$policy.ID), "\n")

dupes <- raw_policy |> get_dupes(policy.ID)
cat("Dupes via get_dupes(policy.ID):", nrow(dupes), "\n")

# Check entire duplicate rows across all columns
dupe_rows_all <- raw_policy |> get_dupes()
cat("Dupes across pick(everything()):", nrow(dupe_rows_all), "\n")

# Let's see what dupes look like
if (nrow(dupes) > 0) {
  print(head(dupes, 10))
}
