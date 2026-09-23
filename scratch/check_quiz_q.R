suppressPackageStartupMessages({
  library(tidyverse)
  library(statmod)
  library(tweedie)
})

train_raw  <- read.csv("data/train.csv")
assess_raw <- read.csv("data/assessment-data.csv")

surcharge_breeds <- c("Poodle", "Airedale Terrier", "Bull Terrier", "Kerry Blue Terrier", "Afghan Hound", "Soft Coated Wheaten Terrier", "Chausie", "Chow Chow", "Welsh Springer Spaniel", "Weimaraner", "Dalmatian", "Collie", "Staffordshire Bull Terrier", "Polish Lowland Sheepdog", "Siberian")
discount_breeds  <- c("Australian Terrier", "Pumi", "Shetland Sheepdog", "Pyrenean Shepherd", "Beagle", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier", "Nova Scotia Duck Tolling Retriever")

train <- train_raw |> mutate(
  breed_outlier = factor(case_when(pet.Breed %in% surcharge_breeds ~ "Surcharge", pet.Breed %in% discount_breeds ~ "Discount", TRUE ~ "Standard"), levels = c("Standard", "Surcharge", "Discount")),
  across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor)
)

# Fit final model
m_final <- glm(
  total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
    owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + I(pet.Height^2) + 
    pet.Height:pet.Weight + breed_outlier + offset(log(policy.Exposure)),
  data = train, family = statmod::tweedie(var.power = 1.3173, link.power = 0)
)

cat("--- Coefficients of Candidates ---\n")
cf <- coef(m_final)
print(cf[grep("Age|PetCount", names(cf))])

cat("\n--- Mix Comparison: Train vs Assessment ---\n")
cat("Train Mean Pet Age      :", mean(train$pet.Age), "\n")
cat("Assess Mean Pet Age     :", mean(assess_raw$pet.Age), "\n")
cat("Train Pet Age >= 10 Pct :", scales::percent(mean(train$pet.Age >= 10)), "\n")
cat("Assess Pet Age >= 10 Pct:", scales::percent(mean(assess_raw$pet.Age >= 10)), "\n")

# Predicted PP
train_pred <- predict(m_final, type = "response") / train$policy.Exposure

assess_prep <- assess_raw |> mutate(
  policy.NBRB = factor(case_match(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB), levels = c("NB", "RB")),
  pet.Group   = factor(case_match(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group), levels = levels(train$pet.Group)),
  pet.Gender  = factor(case_match(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender), levels = levels(train$pet.Gender)),
  owner.Gender = factor(owner.Gender, levels = levels(train$owner.Gender)),
  breed_outlier = factor(case_when(pet.Breed %in% surcharge_breeds ~ "Surcharge", pet.Breed %in% discount_breeds ~ "Discount", TRUE ~ "Standard"), levels = c("Standard", "Surcharge", "Discount")),
  policy.Exposure = 1
)

assess_pred <- predict(m_final, newdata = assess_prep, type = "response")

cat("\nMean Observed Pure Premium in Train :", mean(train$pure_premium), "\n")
cat("Mean Predicted Pure Premium in Train:", mean(train_pred), "\n")
cat("Mean Predicted Pure Premium in Assess:", mean(assess_pred), "\n")
