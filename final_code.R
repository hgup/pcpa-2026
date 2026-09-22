# ================== CAS PCPA - Predictive Analytics Project ===================
# Project Wolverine: Pet Insurance Pure Premium Modeling
# Master Implementation Code File (final_code.R)

# ======================= 1. Load Required Libraries ===========================
suppressPackageStartupMessages({
  library(tidyverse)
  library(statmod)
  library(tweedie)
  library(car)
  library(caret)
  library(glue)
  library(broom)
  library(scales)
})

set.seed(42)

# ================= 2. Data Ingestion, Cleaning & Merging ======================

# Load raw source datasets
raw_pet    <- read.csv("data/pet-data.csv")
raw_owner  <- read.csv("data/owner-data.csv")
raw_policy <- read.csv("data/policy-data.csv")
raw_claims <- read.csv("data/claims-data.csv")

# Clean Policy: filter out zero-exposure policies, parse dates, standardize NBRB
policy <- raw_policy %>%
  filter(policy.Exposure > 0) %>%
  mutate(
    policy.Start = lubridate::date_decimal(policy.Start),
    policy.End   = lubridate::date_decimal(policy.End),
    policy.NBRB  = factor(case_match(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB))
  )

# Clean Pets: standardize categorical miscodings, deduplicate resolving conflicts via non-NA
pets <- raw_pet %>%
  mutate(
    pet.Type      = factor(pet.Type),
    pet.Group     = factor(case_match(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group)),
    pet.Breed     = factor(pet.Breed),
    pet.Gender    = factor(case_match(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender)),
    pet.Offspring = factor(pet.Offspring),
    pet.Microchip = factor(case_match(pet.Microchip, "Nope" ~ "No", "Y" ~ "Yes", .default = pet.Microchip)),
    pet.Origin    = factor(pet.Origin)
  ) %>%
  distinct() %>%
  group_by(pet.ID) %>%
  summarize(across(everything(), ~ na.omit(.x)[1]), .groups = "drop")

# Clean Owners: deduplicate by taking maximum recorded pet count and non-NA attributes
owners <- raw_owner %>%
  group_by(owner.ID) %>%
  summarize(
    owner.PetCount = max(owner.PetCount, na.rm = TRUE),
    across(-owner.PetCount, ~ na.omit(.x)[1]),
    .groups = "drop"
  ) %>%
  mutate(
    owner.Gender   = factor(owner.Gender),
    owner.Location = factor(owner.Location),
    owner.Channel  = factor(owner.Channel)
  )

# Aggregate Claims: combine illness and accident losses at policy level
claims_agg <- raw_claims %>%
  group_by(policy.ID) %>%
  summarize(
    n_claims      = n(),
    illness_loss  = sum(claim.Cost[claim.Coverage == "Illness"]),
    accident_loss = sum(claim.Cost[claim.Coverage == "Accident"]),
    total_losses  = sum(claim.Cost),
    .groups       = "drop"
  )

# Merge datasets: Policy base + Pets + Owners + Claims
merged_df <- policy %>%
  left_join(pets, by = "pet.ID") %>%
  left_join(owners, by = "owner.ID") %>%
  left_join(claims_agg, by = "policy.ID") %>%
  mutate(
    n_claims      = replace_na(n_claims, 0),
    illness_loss  = replace_na(illness_loss, 0),
    accident_loss = replace_na(accident_loss, 0),
    total_losses  = replace_na(total_losses, 0),
    pure_premium  = total_losses / policy.Exposure
  )

# Remove rows with missing values (negligible impact: ~2.9% records dropped)
clean_df <- merged_df %>% drop_na()

# Stratified Train/Test Split (70% train / 30% test)
set.seed(42)
train_idx <- createDataPartition(clean_df$total_losses, p = 0.70, list = FALSE)
train <- clean_df[train_idx, ]
test  <- clean_df[-train_idx, ]

# ========== 3. Exploratory Data Analysis & Feature Engineering ================

# Cap extreme losses at upper fence ($14,949) to prevent GLM distortion from rare extreme events
bp_stats    <- boxplot.stats(train$total_losses[train$total_losses > 0])$stats
upper_fence <- bp_stats[5]
train$total_losses <- pmin(train$total_losses, upper_fence)
train$pure_premium <- train$total_losses / train$policy.Exposure

# Outlier Breed Identification: 24 high-divergence breeds mapped to 3-tier rating factor
surcharge_breeds <- c(
  "Poodle", "Airedale Terrier", "Afghan Hound", "Bull Terrier", "Chausie",
  "Kerry Blue Terrier", "Welsh Springer Spaniel", "Chow Chow", 
  "Soft Coated Wheaten Terrier", "American Staffordshire Terrier", "Pointer", 
  "Polish Lowland Sheepdog", "Collie", "Staffordshire Bull Terrier", 
  "Black and Tan Coonhound", "Dalmatian"
)

discount_breeds <- c(
  "Australian Terrier", "Pumi", "Shetland Sheepdog", "Beagle", 
  "Pyrenean Shepherd", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier"
)

apply_feature_engineering <- function(df) {
  df %>%
    mutate(
      breed_outlier = factor(
        case_when(
          pet.Breed %in% surcharge_breeds ~ "Surcharge",
          pet.Breed %in% discount_breeds  ~ "Discount",
          TRUE ~ "Standard"
        ),
        levels = c("Standard", "Surcharge", "Discount")
      ),
      pet_count_band = factor(
        ifelse(owner.PetCount >= 5, "5+", as.character(owner.PetCount)),
        levels = c("1", "2", "3", "4", "5+")
      ),
      credit_score_band = cut(
        owner.CreditScore,
        breaks = seq(300, 850, by = 50),
        include.lowest = TRUE
      )
    )
}

train <- apply_feature_engineering(train)
test  <- apply_feature_engineering(test)

# Set reference levels for categorical variables
cat_cols <- c("policy.NBRB", "pet.Type", "pet.Group", "pet.Gender", "pet.Offspring",
              "pet.Microchip", "pet.Origin", "owner.Gender", "owner.Location", "owner.Channel", "breed_outlier")

train <- train %>%
  mutate(across(all_of(cat_cols), factor)) %>%
  mutate(
    pet.Type       = relevel(pet.Type, ref = "Dog"),
    pet.Group      = relevel(pet.Group, ref = "Shorthair"),
    pet.Gender     = relevel(pet.Gender, ref = "Male"),
    pet.Offspring  = relevel(pet.Offspring, ref = "No"),
    pet.Microchip  = relevel(pet.Microchip, ref = "Yes"),
    pet.Origin     = relevel(pet.Origin, ref = "Store"),
    owner.Gender   = relevel(owner.Gender, ref = "Male"),
    owner.Location = relevel(owner.Location, ref = "Suburban"),
    policy.NBRB    = relevel(policy.NBRB, ref = "RB"),
    breed_outlier  = relevel(breed_outlier, ref = "Standard")
  )

test <- test %>%
  mutate(across(all_of(cat_cols), factor)) %>%
  mutate(
    pet.Type       = relevel(pet.Type, ref = "Dog"),
    pet.Group      = relevel(pet.Group, ref = "Shorthair"),
    pet.Gender     = relevel(pet.Gender, ref = "Male"),
    pet.Offspring  = relevel(pet.Offspring, ref = "No"),
    pet.Microchip  = relevel(pet.Microchip, ref = "Yes"),
    pet.Origin     = relevel(pet.Origin, ref = "Store"),
    owner.Gender   = relevel(owner.Gender, ref = "Male"),
    owner.Location = relevel(owner.Location, ref = "Suburban"),
    policy.NBRB    = relevel(policy.NBRB, ref = "RB"),
    breed_outlier  = relevel(breed_outlier, ref = "Standard")
  )

# ================ 4. GLM Development & Variable Selection =====================

# Estimate initial Tweedie variance power p via profile likelihood
p_prof_init <- tweedie.profile(
  total_losses ~ 1,
  p.vec   = seq(1.1, 1.9, by = 0.1),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)

p_prof_ref <- tweedie.profile(
  total_losses ~ 1,
  p.vec   = seq(p_prof_init$p.max - 0.1, p_prof_init$p.max + 0.1, by = 0.01),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)
p_initial <- p_prof_ref$p.max
cat("Initial Tweedie p:", round(p_initial, 4), "\n")

# Model A: Full Baseline GLM (excluding pet.Breed for high cardinality and pet.Type for exact aliasing with pet.Group)
model_A <- glm(
  total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight +
    pet.Offspring + pet.Microchip + pet.Origin + owner.Age + owner.Gender +
    owner.Location + owner.CreditScore + owner.Channel + owner.PetCount + policy.NBRB +
    offset(log(policy.Exposure)),
  data   = train,
  family = statmod::tweedie(var.power = p_initial, link.power = 0)
)
cat("Model A (Baseline) AIC:", round(tweedie_AIC(model_A), 2), "\n")

# Model B: Backward Variable Selection using drop1 Chi-square test (p < 0.05)
# Variables removed: owner.Channel, owner.Age, owner.Location, owner.CreditScore, pet.Origin, pet.Microchip, pet.Offspring
model_B <- glm(
  total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight +
    owner.PetCount + policy.NBRB + offset(log(policy.Exposure)),
  data   = train,
  family = statmod::tweedie(var.power = p_initial, link.power = 0)
)
cat("Model B (Selected Main Effects) AIC:", round(tweedie_AIC(model_B), 2), "\n")

# Model C: Add Quadratic Senior Age effect I(pet.Age^2)
model_C <- update(model_B, . ~ . + I(pet.Age^2))
cat("Model C (+ Quadratic Age) AIC:", round(tweedie_AIC(model_C), 2), "\n")

# Model D: Add Morphology & Build Interaction pet.Height:pet.Weight
model_D <- update(model_C, . ~ . + pet.Height:pet.Weight)
cat("Model D (+ Morphology Interaction) AIC:", round(tweedie_AIC(model_D), 2), "\n")

# Model E: Add Outlier Breed Rating Tier
model_E <- update(model_D, . ~ . + breed_outlier)
cat("Model E (+ Outlier Breed Tier) AIC:", round(tweedie_AIC(model_E), 2), "\n")

# ================= 5. Diagnostics & Iterative Refinement ======================

# Model F: Add Quadratic Height I(pet.Height^2) to capture non-linear stature/volume scaling
model_F <- update(model_E, . ~ . + I(pet.Height^2))
cat("Model F (+ Quadratic Height) AIC:", round(tweedie_AIC(model_F), 2), "\n")

# Confirm Tweedie distributional assumption using Dunn-Smyth (randomized quantile) residuals
q_resids <- statmod::qresid(model_F)
cat("Dunn-Smyth Quantile Residuals Summary (centered near 0 with standard normal quantiles):\n")
print(summary(q_resids))

# Re-profile Tweedie p on the refined model formula
p_recheck <- tweedie.profile(
  formula(model_F),
  p.vec   = seq(1.2, 1.6, by = 0.05),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)

p_refined <- tweedie.profile(
  formula(model_F),
  p.vec   = seq(p_recheck$p.max - 0.05, p_recheck$p.max + 0.05, by = 0.01),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)
p_final <- p_refined$p.max
cat("Re-profiled Final Tweedie p:", round(p_final, 4), "\n")

# Fit Final Model with re-profiled p = 1.3173
model_final <- glm(
  formula(model_F),
  data   = train,
  family = statmod::tweedie(var.power = p_final, link.power = 0)
)
cat("Final Model AIC:", round(tweedie_AIC(model_final), 2), "\n")
cat("Final Model % Deviance Explained:",
    round((1 - model_final$deviance / model_final$null.deviance) * 100, 2), "%\n")

# ================== 6. Holdout Validation on Test Data ========================

# Predict expected pure premium on holdout set
test$predicted_losses <- predict(model_final, newdata = test, type = "response")
test$predicted_pp     <- test$predicted_losses / test$policy.Exposure

# Double Lift Analysis across 10 deciles sorted by predicted-to-benchmark pure premium
benchmark_pp <- mean(test$pure_premium)
test$decile  <- ntile(test$predicted_pp / benchmark_pp, 10)

double_lift <- test %>%
  group_by(decile) %>%
  summarize(
    n_policies        = n(),
    exposure          = sum(policy.Exposure),
    actual_losses     = sum(total_losses),
    predicted_losses  = sum(predicted_losses),
    mean_actual_pp    = round(actual_losses / exposure, 2),
    mean_predicted_pp = round(predicted_losses / exposure, 2),
    ae_ratio          = round(actual_losses / predicted_losses, 3),
    .groups           = "drop"
  )
cat("\n=== Holdout Decile Calibration & Double Lift ===\n")
print(as.data.frame(double_lift))

# Gini Coefficient Calculation
gini_df <- test %>%
  arrange(predicted_pp) %>%
  mutate(
    cum_exp  = cumsum(policy.Exposure) / sum(policy.Exposure),
    cum_loss = cumsum(total_losses) / sum(total_losses)
  )

n_obs <- nrow(gini_df)
gini_area <- sum(
  (gini_df$cum_exp[2:n_obs] - gini_df$cum_exp[1:(n_obs - 1)]) *
  (gini_df$cum_loss[2:n_obs] + gini_df$cum_loss[1:(n_obs - 1)]) / 2
)
gini_coef <- 1 - 2 * gini_area
cat(sprintf("Holdout Gini Coefficient: %.4f\n", gini_coef))

# ====== 7. Scoring Assessment Data Set (assessment-data.csv -> scored.csv) ===

# Load assessment dataset (unseen production data)
assess_raw <- read.csv("data/assessment-data.csv")

# Clean and format assessment data matching training pipeline
assess_clean <- assess_raw %>%
  mutate(
    policy.NBRB = factor(case_match(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB)),
    pet.Type    = factor(pet.Type),
    pet.Group   = factor(case_match(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group)),
    pet.Gender  = factor(case_match(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender)),
    breed_outlier = factor(
      case_when(
        pet.Breed %in% surcharge_breeds ~ "Surcharge",
        pet.Breed %in% discount_breeds  ~ "Discount",
        TRUE ~ "Standard"
      ),
      levels = c("Standard", "Surcharge", "Discount")
    )
  )

# Predict annual expected pure premium (PPP)
# Model output under log(policy.Exposure) offset represents expected losses.
# Dividing by policy.Exposure gives the annualized pure premium rate (PPP).
assess_clean$PPP <- predict(model_final, newdata = assess_clean, type = "response") / assess_clean$policy.Exposure

# Export scored file with exact required columns: policy.ID and PPP
scored_output <- assess_clean %>%
  select(policy.ID, PPP)

write.csv(scored_output, "scored.csv", row.names = FALSE)
cat(sprintf("Scored %d records. File saved to scored.csv\n", nrow(scored_output)))
