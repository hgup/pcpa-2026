suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
})

raw_policy <- read.csv("data/policy-data.csv")

# ================= 2.1 Policy Data Cleaning & Deduplication ===================
policy <- raw_policy |>
  # 1. Deduplicate by policy.ID (resolving conflicts by taking first non-NA entry)
  summarize(
    across(everything(), ~ na.omit(.x)[1]),
    .by = policy.ID
  ) |>
  # 2. Filter valid exposures (safety check)
  filter(policy.Exposure > 0, !is.na(policy.Exposure)) |>
  # 3. Standardize dates from decimal years & clean categorical miscodings
  mutate(
    policy.Start = date_decimal(policy.Start),
    policy.End   = date_decimal(policy.End),
    policy.NBRB  = factor(
      case_match(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB),
      levels = c("NB", "RB")
    )
  )

# Validation check
cat("Raw records        :", nrow(raw_policy), "\n")
cat("Cleaned records    :", nrow(policy), "\n")
cat("Records removed    :", nrow(raw_policy) - nrow(policy), "(721 duplicates)\n")
cat("Unique policy IDs  :", n_distinct(policy$policy.ID), "\n")
stopifnot(n_distinct(policy$policy.ID) == nrow(policy))
