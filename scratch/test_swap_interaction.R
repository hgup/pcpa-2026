suppressPackageStartupMessages({
  library(tidyverse)
  library(statmod)
  library(tweedie)
})

# Load engineered train data
train <- read.csv("data/train_engineered.csv")
if (!"age_sq" %in% names(train)) train$age_sq <- train$pet.Age^2

# Tweedie AIC helper
calc_aic <- function(m) {
  p   <- m$family$var.power
  y   <- m$y
  mu  <- m$fitted.values
  w   <- m$prior.weights
  phi <- summary(m)$dispersion
  ll  <- sum(tweedie::dtweedie(y = y, power = p, mu = mu, phi = phi / w, log = TRUE))
  -2 * ll + 2 * length(m$coefficients)
}

# 1. Baseline: Model F (has Height:Weight, no Age:Weight)
f_model_f <- pure_premium ~ pet.Age + age_sq + pet.Height + pet.Weight + 
  pet.Height:pet.Weight + breed_tier + owner.Location + owner.PetCount_band + 
  channel_clean + is_new_business

m_f <- glm(f_model_f, data = train, family = tweedie(var.power = 1.3173, link.power = 0), offset = log(policy.Exposure))
aic_f <- calc_aic(m_f)

# 2. Swap Candidate: Remove Height:Weight, Add Age:Weight
f_swap <- pure_premium ~ pet.Age + age_sq + pet.Height + pet.Weight + 
  pet.Age:pet.Weight + breed_tier + owner.Location + owner.PetCount_band + 
  channel_clean + is_new_business

m_swap <- glm(f_swap, data = train, family = tweedie(var.power = 1.3173, link.power = 0), offset = log(policy.Exposure))
aic_swap <- calc_aic(m_swap)

# 3. Model G: Keep Both Height:Weight AND Age:Weight
f_g <- pure_premium ~ pet.Age + age_sq + pet.Height + pet.Weight + 
  pet.Height:pet.Weight + pet.Age:pet.Weight + breed_tier + owner.Location + 
  owner.PetCount_band + channel_clean + is_new_business

m_g <- glm(f_g, data = train, family = tweedie(var.power = 1.3173, link.power = 0), offset = log(policy.Exposure))
aic_g <- calc_aic(m_g)

# Summary table
cat("=== Comparison of Models ===\n")
cat(sprintf("Model F (Baseline with Height:Weight)       : AIC = %.2f\n", aic_f))
cat(sprintf("Swap (Remove Height:Weight, Add Age:Weight) : AIC = %.2f (Delta vs F: %+.2f)\n", aic_swap, aic_swap - aic_f))
cat(sprintf("Model G (Keep BOTH Height:Weight & Age:Weight): AIC = %.2f (Delta vs F: %+.2f)\n", aic_g, aic_g - a_f <- aic_g - aic_f))

cat("\n--- Summary of Swap Model Coefficients ---\n")
s_swap <- summary(m_swap)$coefficients
print(round(s_swap[grep("Age|Height|Weight", rownames(s_swap)), ], 6))

cat("\n--- Summary of Model G Coefficients (Both Interactions) ---\n")
s_g <- summary(m_g)$coefficients
print(round(s_g[grep("Age|Height|Weight", rownames(s_g)), ], 6))

cat("\n--- Drop1 Test on Model G ---\n")
print(drop1(m_g, test = "Chisq"))
