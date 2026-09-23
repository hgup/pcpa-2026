suppressPackageStartupMessages({library(tidyverse); library(statmod); library(tweedie)})
train_raw <- read.csv("data/train.csv")

surcharge_breeds <- c("Poodle", "Airedale Terrier", "Bull Terrier", "Kerry Blue Terrier", "Afghan Hound", "Soft Coated Wheaten Terrier", "Chausie", "Chow Chow", "Welsh Springer Spaniel", "Weimaraner", "Dalmatian", "Collie", "Staffordshire Bull Terrier", "Polish Lowland Sheepdog", "Siberian")
discount_breeds <- c("Australian Terrier", "Pumi", "Shetland Sheepdog", "Pyrenean Shepherd", "Beagle", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier", "Nova Scotia Duck Tolling Retriever")

train <- train_raw |>
  mutate(
    breed_outlier = factor(case_when(pet.Breed %in% surcharge_breeds ~ "Surcharge", pet.Breed %in% discount_breeds ~ "Discount", TRUE ~ "Standard"), levels = c("Standard", "Surcharge", "Discount")),
    across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor)
  )

p <- 1.3173

# Model E (linear height + height:weight)
m_e <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + breed_outlier + offset(log(policy.Exposure)), data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model F (adds Height^2)
m_f <- update(m_e, . ~ . + I(pet.Height^2))

train$res_e <- residuals(m_e, type = "deviance")
train$res_f <- residuals(m_f, type = "deviance")

# Height bands residual means
height_bands <- train |>
  mutate(h_band = cut(pet.Height, breaks = seq(0, 35, by = 5), include.lowest = TRUE)) |>
  group_by(h_band) |>
  summarize(
    n = n(),
    mean_res_Model_E = round(mean(res_e), 4),
    mean_res_Model_F = round(mean(res_f), 4),
    diff = round(mean(res_f) - mean(res_e), 4)
  )

print(height_bands)
