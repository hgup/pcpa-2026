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
    across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor)
  )

p <- 1.3173

# Current model
f_current <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + breed_outlier +
  offset(log(policy.Exposure))

m_current <- glm(f_current, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model with Height^2
f_h2 <- update(f_current, . ~ . + I(pet.Height^2))
m_h2 <- glm(f_h2, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model with Weight^2
f_w2 <- update(f_current, . ~ . + I(pet.Weight^2))
m_w2 <- glm(f_w2, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("AIC Current (linear Height)  :", AICtweedie(m_current), "\n")
cat("AIC with I(pet.Height^2)     :", AICtweedie(m_h2), "\n")
cat("AIC with I(pet.Weight^2)     :", AICtweedie(m_w2), "\n")

cat("\nSummary of I(pet.Height^2) term:\n")
s_h2 <- summary(m_h2)$coefficients
print(s_h2[grep("Height", rownames(s_h2)), ])
