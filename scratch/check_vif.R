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
  across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor)
)

# 1. Pearson Correlation
cor_hw <- cor(train$pet.Height, train$pet.Weight)
cat(sprintf("Pearson Correlation between Height and Weight: %.4f\n", cor_hw))

# 2. VIF on Linear Model (without interaction)
m_linear <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
                  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + breed_outlier +
                  offset(log(policy.Exposure)),
                data = train, family = statmod::tweedie(var.power = 1.3173, link.power = 0))

cat("\n--- GVIF for Additive Model (Main Effects Only) ---\n")
print(car::vif(m_linear))

# 3. Model F (with Height:Weight and Height^2)
m_f <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + I(pet.Height^2) + 
             pet.Height:pet.Weight + breed_outlier + offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = 1.3173, link.power = 0))

cat("\n--- GVIF for Model F (with Interactions/Polynomials) ---\n")
print(car::vif(m_f))

# 4. Check standard errors of coefficients
cat("\n--- Coefficients & Standard Errors for Height and Weight in Model F ---\n")
s_f <- summary(m_f)$coefficients
print(s_f[grep("Height|Weight", rownames(s_f)), ])
