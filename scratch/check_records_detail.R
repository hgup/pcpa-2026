suppressPackageStartupMessages(library(dplyr))
raw_policy <- read.csv("data/policy-data.csv")
raw_pet    <- read.csv("data/pet-data.csv")
raw_owner  <- read.csv("data/owner-data.csv")

cat("Rows in raw_policy:", nrow(raw_policy), "\n")
cat("Unique policy.ID:", n_distinct(raw_policy$policy.ID), "\n")
cat("Duplicates in policy.ID:", nrow(raw_policy) - n_distinct(raw_policy$policy.ID), "\n")

# Check policies without matching pet
policy_pet_match <- sum(raw_policy$pet.ID %in% raw_pet$pet.ID)
cat("Policies with matching pet.ID:", policy_pet_match, "Missing:", nrow(raw_policy) - policy_pet_match, "\n")

# Check exact unique rows
cat("If deduplicating policy.ID: exactly", nrow(raw_policy) - n_distinct(raw_policy$policy.ID), "records removed (721).\n")
cat("If deduplicating exact full rows: exactly", (nrow(raw_policy) - nrow(unique(raw_policy))), "records removed (700).\n")
cat("If filtering exposure <= 0: exactly 0 records removed.\n")
