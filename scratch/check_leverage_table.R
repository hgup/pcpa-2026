suppressPackageStartupMessages({library(tidyverse); library(statmod); library(tweedie)})
train_raw <- read.csv("data/train.csv")

surcharge_breeds <- c("Poodle", "Airedale Terrier", "Bull Terrier", "Kerry Blue Terrier", "Afghan Hound", "Soft Coated Wheaten Terrier", "Chausie", "Chow Chow", "Welsh Springer Spaniel", "Weimaraner", "Dalmatian", "Collie", "Staffordshire Bull Terrier", "Polish Lowland Sheepdog", "Siberian")
discount_breeds  <- c("Australian Terrier", "Pumi", "Shetland Sheepdog", "Pyrenean Shepherd", "Beagle", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier", "Nova Scotia Duck Tolling Retriever")

train <- train_raw |> mutate(
  breed_outlier = factor(case_when(pet.Breed %in% surcharge_breeds ~ "Surcharge", pet.Breed %in% discount_breeds ~ "Discount", TRUE ~ "Standard"), levels = c("Standard", "Surcharge", "Discount")),
  across(c(policy.NBRB, pet.Group, pet.Gender, owner.Gender), as.factor)
)

p <- 1.3173

m_e <- glm(total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight + 
             owner.Gender + owner.PetCount + policy.NBRB + I(pet.Age^2) + pet.Height:pet.Weight + breed_outlier +
             offset(log(policy.Exposure)),
           data = train, family = statmod::tweedie(var.power = p, link.power = 0))

train_res <- train |>
  mutate(
    fitted_values = predict(m_e, type = "response"),
    leverage = hatvalues(m_e)
  )

n <- nrow(train_res)
lev_thresh <- 2 * length(coef(m_e)) / n

top_lev <- train_res |>
  filter(leverage > lev_thresh) |>
  arrange(desc(leverage)) |>
  select(pet.Breed, pet.Group, pet.Height, pet.Weight, leverage) |>
  head(20)

print(as.data.frame(top_lev))

cat("\nSummary of pet.Height across full data:\n")
print(summary(train$pet.Height))

cat("\nSummary of pet.Weight across full data:\n")
print(summary(train$pet.Weight))
