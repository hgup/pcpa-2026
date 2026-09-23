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

p <- 1.3173

m_f <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + I(pet.Height^2) + 
             pet.Height:pet.Weight + breed_outlier + offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("--- Predictor-Level VIF (car::vif type='predictor') ---\n")
print(car::vif(m_f, type = "predictor"))
