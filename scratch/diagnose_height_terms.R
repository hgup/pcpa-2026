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

# Model without Height:Weight and without Height^2
f_base <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + breed_outlier +
  offset(log(policy.Exposure))
m_base <- glm(f_base, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model with Height:Weight (Model E)
f_e <- update(f_base, . ~ . + pet.Height:pet.Weight)
m_e <- glm(f_e, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model F (adds Height^2)
f_f <- update(f_e, . ~ . + I(pet.Height^2))
m_f <- glm(f_f, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

cat("Base (no interaction, linear height) AIC:", tweedie_AIC(m_base), "\n")
cat("Model E (+ Height:Weight interaction) AIC:", tweedie_AIC(m_e), "\n")
cat("Model F (+ Height^2)                  AIC:", tweedie_AIC(m_f), "\n")

# Check residual curvature vs Height in Model E
r_e <- residuals(m_e, type = "deviance")
fit_loess <- loess(r_e ~ train$pet.Height, span = 0.5)
pred_loess <- predict(fit_loess, newdata = data.frame(pet.Height = c(5, 10, 15, 20, 25, 30)))
cat("\nLOESS curve of Deviance Residuals vs Height (Model E):\n")
print(data.frame(Height = c(5, 10, 15, 20, 25, 30), LOESS_Resid = round(pred_loess, 4)))

# Check residual curvature vs Height in Model F
r_f <- residuals(m_f, type = "deviance")
fit_loess_f <- loess(r_f ~ train$pet.Height, span = 0.5)
pred_loess_f <- predict(fit_loess_f, newdata = data.frame(pet.Height = c(5, 10, 15, 20, 25, 30)))
cat("\nLOESS curve of Deviance Residuals vs Height (Model F):\n")
print(data.frame(Height = c(5, 10, 15, 20, 25, 30), LOESS_Resid = round(pred_loess_f, 4)))
