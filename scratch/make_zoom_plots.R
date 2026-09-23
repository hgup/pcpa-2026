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

train$dev_res <- residuals(m_e, type = "deviance")

# 1. Zoomed via coord_cartesian
p_zoom <- ggplot(train, aes(x = pet.Height, y = dev_res)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess", color = "darkgreen", linewidth = 1.2, se = FALSE) +
  coord_cartesian(ylim = c(-12, 10)) +
  labs(title = "Zoomed Deviance Residuals vs Pet Height", subtitle = "Zoomed to [-12, +10] via coord_cartesian (LOESS calculated on all data)", x = "Pet Height (in)", y = "Deviance Residuals") +
  theme_minimal()

# 2. Binned Residual Plot (Actuarial Standard)
binned_diag <- train |>
  mutate(height_bin = ntile(pet.Height, 15)) |>
  group_by(height_bin) |>
  summarize(
    mean_height = mean(pet.Height),
    mean_resid  = mean(dev_res),
    se_resid    = sd(dev_res) / sqrt(n())
  )

p_binned <- ggplot(binned_diag, aes(x = mean_height, y = mean_resid)) +
  geom_point(color = "navy", size = 3) +
  geom_line(color = "navy", linewidth = 1) +
  geom_errorbar(aes(ymin = mean_resid - 1.96 * se_resid, ymax = mean_resid + 1.96 * se_resid), width = 0.5, color = "grey50") +
  geom_hline(yintercept = mean(train$dev_res), color = "red", linetype = "dashed") +
  labs(title = "Binned Mean Deviance Residuals vs Pet Height (15 Bins)", subtitle = "Dashed red line = portfolio mean residual. Clear dip in middle heights.", x = "Mean Height (in)", y = "Mean Deviance Residual") +
  theme_minimal()

png("scratch/zoom_height.png", width = 800, height = 500)
print(p_zoom)
dev.off()

png("scratch/binned_height.png", width = 800, height = 500)
print(p_binned)
dev.off()

cat("Plots generated successfully.\n")
