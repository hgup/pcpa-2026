suppressPackageStartupMessages({
  library(tidyverse)
  library(statmod)
  library(tweedie)
})

train_raw <- read.csv("data/train.csv")
test_raw  <- read.csv("data/test.csv")

surcharge_breeds <- c("Poodle", "Airedale Terrier", "Bull Terrier", "Kerry Blue Terrier", "Afghan Hound", "Soft Coated Wheaten Terrier", "Chausie", "Chow Chow", "Welsh Springer Spaniel", "Weimaraner", "Dalmatian", "Collie", "Staffordshire Bull Terrier", "Polish Lowland Sheepdog", "Siberian")
discount_breeds  <- c("Australian Terrier", "Pumi", "Shetland Sheepdog", "Pyrenean Shepherd", "Beagle", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier", "Nova Scotia Duck Tolling Retriever")

prep <- function(df) {
  df |> mutate(
    breed_outlier = factor(case_when(pet.Breed %in% surcharge_breeds ~ "Surcharge", pet.Breed %in% discount_breeds ~ "Discount", TRUE ~ "Standard"), levels = c("Standard", "Surcharge", "Discount")),
    across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor)
  )
}

train <- prep(train_raw)
test  <- prep(test_raw)

p <- 1.3173

# Model WITHOUT Height:Weight
f_no_int <- total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
  owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + I(pet.Height^2) + breed_outlier +
  offset(log(policy.Exposure))
m_no_int <- glm(f_no_int, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Model WITH Height:Weight
f_with_int <- update(f_no_int, . ~ . + pet.Height:pet.Weight)
m_with_int <- glm(f_with_int, data = train, family = statmod::tweedie(var.power = p, link.power = 0))

# Predict on TEST set
pred_no   <- predict(m_no_int, newdata = test, type = "response")
pred_with <- predict(m_with_int, newdata = test, type = "response")

actual <- test$pure_premium
pred_pp_no   <- pred_no / test$policy.Exposure
pred_pp_with <- pred_with / test$policy.Exposure

rmse_no   <- sqrt(mean((actual - pred_pp_no)^2))
rmse_with <- sqrt(mean((actual - pred_pp_with)^2))

mae_no   <- mean(abs(actual - pred_pp_no))
mae_with <- mean(abs(actual - pred_pp_with))

calc_gini <- function(actual, pred, exposure) {
  df <- data.frame(actual = actual, pred = pred, exposure = exposure) |>
    arrange(pred) |>
    mutate(
      cum_exp   = cumsum(exposure) / sum(exposure),
      cum_loss  = cumsum(actual * exposure) / sum(actual * exposure)
    )
  # Trapezoidal rule for Area Under Lorenz Curve
  n <- nrow(df)
  lorenz_area <- sum((df$cum_loss[-1] + df$cum_loss[-n]) * diff(df$cum_exp)) / 2
  gini <- 1 - 2 * lorenz_area
  gini
}

gini_no   <- calc_gini(test$pure_premium, pred_pp_no, test$policy.Exposure)
gini_with <- calc_gini(test$pure_premium, pred_pp_with, test$policy.Exposure)

cat("=== Holdout (Test Set) Performance Comparison ===\n")
cat(sprintf("WITHOUT Height:Weight -> RMSE: %.4f | MAE: %.4f | Gini: %.4f\n", rmse_no, mae_no, gini_no))
cat(sprintf("WITH    Height:Weight -> RMSE: %.4f | MAE: %.4f | Gini: %.4f\n", rmse_with, mae_with, gini_with))
cat(sprintf("Delta (Improvement)   -> RMSE: %+.4f | MAE: %+.4f | Gini: %+.4f\n", rmse_with - rmse_no, mae_with - mae_no, gini_with - gini_no))
