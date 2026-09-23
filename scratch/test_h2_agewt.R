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

f_base <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + breed_outlier +
  offset(log(policy.Exposure))

f_h2 <- update(f_base, . ~ . + I(pet.Height^2))
m_h2 <- glm(f_h2, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

f_h2_agewt <- update(f_base, . ~ . + I(pet.Height^2) + pet.Age:pet.Weight)
m_h2_agewt <- glm(f_h2_agewt, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("AIC Base              :", AICtweedie(glm(f_base, data = train, family = statmod::tweedie(var.power = p, link.power = 0))), "\n")
cat("AIC + Height^2        :", AICtweedie(m_h2), "\n")
cat("AIC + Height^2 + Age:Wt:", AICtweedie(m_h2_agewt), "\n")
cat("\nDrop1 on + Height^2 + Age:Wt:\n")
print(drop1(m_h2_agewt, test = "Chisq"))
