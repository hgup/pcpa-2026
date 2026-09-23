suppressPackageStartupMessages({library(tidyverse); library(statmod); library(tweedie)})
train_raw <- read.csv("data/train.csv")
train <- train_raw |> mutate(across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor))

p <- 1.3173

# Model A: Full Baseline (all variables linear)
m_a <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             pet.Offspring + pet.Microchip + pet.Origin + owner.Age + owner.Gender + 
             owner.Location + owner.CreditScore + owner.Channel + owner.PetCount + policy.NBRB + 
             offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("Model A (Full Baseline) pet.Age beta:\n")
print(coef(m_a)["pet.Age"])

# Model B: Selected main effects (linear pet.Age)
m_b <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             owner.Gender + owner.PetCount + policy.NBRB + 
             offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("\nModel B (Backward Selection) pet.Age beta:\n")
print(coef(m_b)["pet.Age"])

# What about exponentiated beta (relativity)? exp(0.222) = 1.248!
cat("\nExponentiated beta exp(beta) [Relativity/Multiplicative effect per year of age]:\n")
cat("exp(beta) Model A:", exp(coef(m_a)["pet.Age"]), "\n")
cat("exp(beta) Model B:", exp(coef(m_b)["pet.Age"]), "\n")
