suppressPackageStartupMessages({
  library(tidyverse)
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

# Model without Height^2 (Model E)
m_e <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + breed_outlier +
             offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model with Height^2 (Model F)
m_f <- update(m_e, . ~ . + I(pet.Height^2))

cat("--- Model E (Linear Height + Height:Weight) Coefficients ---\n")
print(summary(m_e)$coefficients[c("pet.Height", "pet.Weight", "pet.Height:pet.Weight"), ])

cat("\n--- Model F (Linear + Quadratic Height + Height:Weight) Coefficients ---\n")
print(summary(m_f)$coefficients[c("pet.Height", "I(pet.Height^2)", "pet.Weight", "pet.Height:pet.Weight"), ])

# Compare predictions for extreme heights at median weight (say weight = 60)
new_d <- data.frame(
  pet.Group = factor("Sporting", levels = levels(train$pet.Group)),
  pet.Age = 5, pet.Gender = factor("Male", levels = levels(train$pet.Gender)),
  pet.Height = c(10, 15, 20, 25, 30, 35),
  pet.Weight = 60,
  owner.Gender = factor("Male", levels = levels(train$owner.Gender)),
  owner.PetCount = 1, policy.NBRB = factor("NB", levels = levels(train$policy.NBRB)),
  breed_outlier = factor("Standard", levels = levels(train$breed_outlier)),
  policy.Exposure = 1
)

pred_e <- predict(m_e, newdata = new_d, type = "response")
pred_f <- predict(m_f, newdata = new_d, type = "response")

cat("\nPredicted Pure Premium across Height (at Weight = 60 lbs):\n")
res_pred <- data.frame(Height = new_d$pet.Height, Pred_Model_E_Linear = round(pred_e, 1), Pred_Model_F_Quadratic = round(pred_f, 1), Diff = round(pred_f - pred_e, 1))
print(res_pred)
