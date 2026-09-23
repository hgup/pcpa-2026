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

# Model E (Linear Height + Height:Weight)
m_e <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + breed_outlier +
             offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = p, link.power = 0))

train$pred_losses_e <- predict(m_e, type = "response")
train$dev_res_e     <- residuals(m_e, type = "deviance")

# Diagnostic: Actual vs Expected Losses and Mean Deviance Residual by Height Band
height_diag <- train |>
  mutate(h_band = cut(pet.Height, breaks = c(0, 8, 14, 20, 26, 35), labels = c("<8 (Toy/Mini)", "8-14 (Small)", "14-20 (Medium)", "20-26 (Large)", ">26 (Giant)"), include.lowest = TRUE)) |>
  group_by(h_band) |>
  summarize(
    n_policies   = n(),
    exposure     = round(sum(policy.Exposure), 1),
    actual_pp    = round(sum(total_losses) / sum(policy.Exposure), 2),
    expected_pp  = round(sum(pred_losses_e) / sum(policy.Exposure), 2),
    actual_over_expected = round(sum(total_losses) / sum(pred_losses_e), 4),
    mean_dev_res = round(mean(dev_res_e), 4)
  )

print(as.data.frame(height_diag))
