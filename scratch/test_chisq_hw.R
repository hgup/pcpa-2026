suppressPackageStartupMessages({
  library(tidyverse)
  library(statmod)
  library(tweedie)
})

train <- read.csv("data/train.csv")
p <- 1.3173

# 1. Direct Likelihood Ratio Chi-Square Test (ANOVA between Additive vs Interaction GLM)
m_add <- glm(total_losses ~ pet.Height + pet.Weight + offset(log(policy.Exposure)),
             data = train, family = statmod::tweedie(var.power = p, link.power = 0))

m_int <- glm(total_losses ~ pet.Height * pet.Weight + offset(log(policy.Exposure)),
             data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("--- Likelihood Ratio Chi-Square Test (Additive vs Interaction) ---\n")
print(anova(m_add, m_int, test = "Chisq"))

# 2. Chi-Square Test using drop1 on the interaction model
cat("\n--- Single Term Deletion Drop1 Test ---\n")
print(drop1(m_int, test = "Chisq"))
