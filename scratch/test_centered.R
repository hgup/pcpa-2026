suppressPackageStartupMessages({
  library(tidyverse)
  library(car)
  library(statmod)
  library(tweedie)
})

train_raw <- read.csv("data/train.csv")
surcharge_breeds <- c("Poodle", "Airedale Terrier", "Bull Terrier", "Kerry Blue Terrier", "Afghan Hound", "Soft Coated Wheaten Terrier", "Chausie", "Chow Chow", "Welsh Springer Spaniel", "Weimaraner", "Dalmatian", "Collie", "Staffordshire Bull Terrier", "Polish Lowland Sheepdog", "Siberian")
discount_breeds  <- c("Australian Terrier", "Pumi", "Shetland Sheepdog", "Pyrenean Shepherd", "Beagle", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier", "Nova Scotia Duck Tolling Retriever")

train <- train_raw |> mutate(
  breed_outlier = factor(case_when(pet.Breed %in% surcharge_breeds ~ "Surcharge", pet.Breed %in% discount_breeds ~ "Discount", TRUE ~ "Standard"), levels = c("Standard", "Surcharge", "Discount")),
  across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor),
  h_c = pet.Height - mean(pet.Height),
  w_c = pet.Weight - mean(pet.Weight)
)

p <- 1.3173

# Centered model
m_c <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + h_c + w_c + 
             owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + I(h_c^2) + 
             h_c:w_c + breed_outlier + offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("--- VIF for Centered Model ---\n")
print(car::vif(m_c))

cat("\n--- Coefficients for Centered Model ---\n")
s_c <- summary(m_c)$coefficients
print(s_c[grep("h_c|w_c", rownames(s_c)), ])

# Compare AIC and fitted values
m_raw <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
               owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + I(pet.Height^2) + 
               pet.Height:pet.Weight + breed_outlier + offset(log(policy.Exposure)),
             data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("\nAIC Raw     :", AICtweedie(m_raw), "\n")
cat("AIC Centered:", AICtweedie(m_c), "\n")
cat("Max absolute difference in fitted values:", max(abs(predict(m_raw, type="response") - predict(m_c, type="response"))), "\n")
