suppressPackageStartupMessages({library(tidyverse); library(statmod); library(tweedie)})
train_raw <- read.csv("data/train.csv")
train <- train_raw |> mutate(across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor))

m_a <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             pet.Offspring + pet.Microchip + pet.Origin + owner.Age + owner.Gender + 
             owner.Location + owner.CreditScore + owner.Channel + owner.PetCount + policy.NBRB + 
             offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = 1.3173, link.power = 0))

cat("--- Model A (Linear Baseline) Coefficients ---\n")
print(coef(m_a)[c("owner.Age", "pet.Age", "owner.PetCount")])
