suppressPackageStartupMessages({
  library(tidyverse)
  library(statmod)
  library(tweedie)
})

train_raw <- read.csv("data/train.csv")

surcharge_breeds <- c(
  "Poodle", "Airedale Terrier", "Bull Terrier", "Kerry Blue Terrier",
  "Afghan Hound", "Soft Coated Wheaten Terrier", "Chausie", "Chow Chow",
  "Welsh Springer Spaniel", "Weimaraner", "Dalmatian", "Collie",
  "Staffordshire Bull Terrier", "Polish Lowland Sheepdog", "Siberian"
)

discount_breeds <- c(
  "Australian Terrier", "Pumi", "Shetland Sheepdog", "Pyrenean Shepherd",
  "Beagle", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier",
  "Nova Scotia Duck Tolling Retriever"
)

train <- train_raw |>
  mutate(
    breed_outlier = factor(
      case_when(
        pet.Breed %in% surcharge_breeds ~ "Surcharge",
        pet.Breed %in% discount_breeds  ~ "Discount",
        TRUE ~ "Standard"
      ),
      levels = c("Standard", "Surcharge", "Discount")
    ),
    across(c(policy.NBRB, pet.Type, pet.Group, pet.Gender, owner.Gender), as.factor)
  )

p <- 1.3173

# Model F (current baseline)
f_modelf <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + I(pet.Height^2) + 
  pet.Height:pet.Weight + breed_outlier + offset(log(policy.Exposure))

m_f <- glm(f_modelf, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model with pet.Type:pet.Weight
f_type_wt <- update(f_modelf, . ~ . + pet.Type:pet.Weight)
m_type_wt <- glm(f_type_wt, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model with pet.Group:pet.Weight
f_group_wt <- update(f_modelf, . ~ . + pet.Group:pet.Weight)
m_group_wt <- glm(f_group_wt, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("AIC Model F (Baseline)            :", round(tweedie_AIC(m_f), 2), "\n")
cat("AIC Model F + pet.Type:pet.Weight :", round(tweedie_AIC(m_type_wt), 2), 
    "| Delta AIC:", round(tweedie_AIC(m_type_wt) - tweedie_AIC(m_f), 2), "\n")
cat("AIC Model F + pet.Group:pet.Weight:", round(tweedie_AIC(m_group_wt), 2), 
    "| Delta AIC:", round(tweedie_AIC(m_group_wt) - tweedie_AIC(m_f), 2), "\n")

cat("\n--- Summary of pet.Type:pet.Weight Term ---\n")
s_tw <- summary(m_type_wt)$coefficients
print(s_tw[grep("Type|Weight", rownames(s_tw)), ])

cat("\n--- Drop1 Test for pet.Type:pet.Weight ---\n")
print(drop1(m_type_wt, test = "Chisq"))
