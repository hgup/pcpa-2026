raw_policy <- read.csv("data/policy-data.csv")
cat("Rows in raw policy-data.csv:", nrow(raw_policy), "\n")

# Check zero exposure
n_zero_exp <- sum(raw_policy$policy.Exposure <= 0, na.rm = TRUE)
cat("Rows with policy.Exposure <= 0:", n_zero_exp, "\n")

# Check NA exposure
n_na_exp <- sum(is.na(raw_policy$policy.Exposure))
cat("Rows with NA policy.Exposure:", n_na_exp, "\n")

# Check duplicates
n_dupe_id <- sum(duplicated(raw_policy$policy.ID))
cat("Duplicate policy.ID count:", n_dupe_id, "\n")

# Check cleaning step in Code.R
# Code.R:
# policy <- raw_policy |>
#   filter(policy.Exposure > 0)
clean_policy <- raw_policy[raw_policy$policy.Exposure > 0 & !is.na(raw_policy$policy.Exposure), ]
n_removed <- nrow(raw_policy) - nrow(clean_policy)
cat("Total rows removed from policy-data:", n_removed, "\n")
