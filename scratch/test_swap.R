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

# Model 1: Current Best (with Height:Weight, WITHOUT Age:Weight)
f_current <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + breed_outlier +
  offset(log(policy.Exposure))

m_current <- glm(f_current, data = train, family = statmod::tweedie(var.power = p, link.power = 0))
aic_current <- AICtweedie(m_current)

# Model 2: Swap (REMOVE Height:Weight, ADD Age:Weight)
f_swap <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Age:pet.Weight + breed_outlier +
  offset(log(policy.Exposure))

m_swap <- glm(f_swap, data = train, family = statmod::tweedie(var.power = p, link.power = 0))
aic_swap <- AICtweedie(m_swap)

# Model 3: Keep BOTH (Height:Weight AND Age:Weight)
f_both <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + pet.Age:pet.Weight + breed_outlier +
  offset(log(policy.Exposure))

m_both <- glm(f_both, data = train, family = statmod::tweedie(var.power = p, link.power = 0))
aic_both <- AICtweedie(m_both)

# Model 4: NEITHER
f_neither <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + breed_outlier +
  offset(log(policy.Exposure))

m_neither <- glm(f_neither, data = train, family = statmod::tweedie(var.power = p, link.power = 0))
aic_neither <- AICtweedie(m_neither)

cat("=========================================================================\n")
cat("                       INTERACTION COMPARISON TABLE                     \n")
cat("=========================================================================\n")
res_tbl <- tibble(
  Model = c(
    "1. Neither Interaction",
    "2. Current Model (Only Height:Weight)",
    "3. Swap Model (Only Age:Weight)",
    "4. Model G (BOTH Height:Weight & Age:Weight)"
  ),
  AIC = round(c(aic_neither, aic_current, aic_swap, aic_both), 2),
  Delta_vs_Current = round(c(aic_neither - aic_current, 0, aic_swap - aic_current, aic_both - aic_current), 2),
  Deviance = round(c(m_neither$deviance, m_current$deviance, m_swap$deviance, m_both$deviance), 1),
  Pct_Dev_Explained = paste0(round(c(
    (1 - m_neither$deviance / m_neither$null.deviance) * 100,
    (1 - m_current$deviance / m_current$null.deviance) * 100,
    (1 - m_swap$deviance / m_swap$null.deviance) * 100,
    (1 - m_both$deviance / m_both$null.deviance) * 100
  ), 3), "%")
)
print(as.data.frame(res_tbl), row.names = FALSE)

cat("\n--- Summary of Swap Model Coefficients ---\n")
s_swap <- summary(m_swap)$coefficients
print(round(s_swap[grep("Age|Height|Weight", rownames(s_swap)), ], 6))

cat("\n--- Summary of Model G Coefficients (Both Interactions) ---\n")
s_both <- summary(m_both)$coefficients
print(round(s_both[grep("Age|Height|Weight", rownames(s_both)), ], 6))

cat("\n--- Drop1 on Model G (Both) ---\n")
print(drop1(m_both, test = "Chisq"))
