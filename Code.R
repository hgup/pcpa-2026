# ================== CAS PCPA - Predictive Analytics Project ===================
# Project Wolverine: Pet Insurance Pure Premium Modeling
# Master Implementation Code File

# ======================= 1. Load Required Libraries ===========================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, scales, 
  janitor, skimr, naniar, glue, broom,
  statmod, tweedie, car, caret
)

set.seed(42)

# ================= 2. Data Ingestion, Cleaning & Merging ======================

## Data & Appendix Loading ====

# String of names copied from the appendix (data dictionary)
appendix_breed_cats <- "Abyssinian, American Bobtail, American Curl, American Shorthair, American Wirehair, Asian, Asian Semi Longhair, Australian Mist, Balinese, Bambino, Bengal, Birman, Bombay, Brazilian Shorthair, British Shorthair, Burmese, Chantilly Tiffany, Chartreux, Chausie, Cornish Rex, Cymric, Cyprus, Devon Rex, Donskoy, Egyptian Mau, European Burmese, European Shorthair, Exotic Shorthair, German Rex, Havana, Havana Brown, Highlander, Himalayan, Japanese Bobtail, Javanese, Korat, Kurilian Bobtail, LaPerm, Lykoi, Maine Coon, Manx, Minskin, Munchkin, Napoleon, Nebelung, Ocicat, Ojos Azules, Oriental, Oriental Bicolor, Oriental Longhair, Persian, Peterbald, Pixiebob, Ragamuffin, Ragdoll, Russian Blue, Scottish Fold, Selkirk Rex, Seychellois, Siamese, Siberian, Siberian Forest, Singapura, Snowshoe, Sokoke, Somali, Sphynx, Thai, Tiffany, Tonkinese, Toybob, Toyger, Turkish Angora, Turkish Van, Ukrainian Levkoy, Ussuri, York Chocolate"
appendix_breed_dogs <- " Afghan Hound, Airedale Terrier, Akita, Alaskan Malamute, American Eskimo Dog, American Foxhound, American Staffordshire Terrier, Anatolian Shepherd Dog, Australian Cattle Dog, Australian Shepherd, Australian Terrier, Basenji, Basset Hound, Beagle, Bearded Collie, Beauceron, Bedlington Terrier, Belgian Malinois, Belgian Sheepdog, Belgian Tervuren, Bernese Mountain Dog, Bichon Frise, Black and Tan Coonhound, Bloodhound, Border Collie, Border Terrier, Borzoi, Boston Terrier, Boxer, Boykin Spaniel, Briard, Brittany, Brussels Griffon, Bull Terrier, Bulldog, Cairn Terrier, Canaan Dog, Cardigan Welsh Corgi, Cavalier King Charles Spaniel, Chesapeake Bay Retriever, Chihuahua, Chinese Crested, Chow Chow, Clumber Spaniel, Cocker Spaniel, Collie, Coton de Tulear, Dachshund, Dalmatian, Dandie Dinmont Terrier, Doberman Pinscher, Dogo Argentino, English Cocker Spaniel, English Foxhound, English Setter, English Springer Spaniel, English Toy Spaniel, Entlebucher Mountain Dog, Field Spaniel, Finnish Lapphund, Finnish Spitz, French Bulldog, German Pinscher, German Shepherd Dog, German Shorthaired Pointer, German Wirehaired Pointer, Giant Schnauzer, Glen of Imaal Terrier, Golden Retriever, Gordon Setter, Great Dane, Great Pyrenees, Greater Swiss Mountain Dog, Greyhound, Harrier, Havanese, Irish Red and White Setter, Irish Setter, Irish Terrier, Irish Water Spaniel, Irish Wolfhound, Italian Greyhound, Japanese Chin, Keeshond, Kerry Blue Terrier, Komondor, Kuvasz, Labrador Retriever, Lagotto Romagnolo, Lakeland Terrier, Leonberger, Lhasa Rapso, Maltese, Manchester Terrier, Mastiff, Miniature Bull Terrier, Miniature Pinscher, Miniature Schnauzer, Newfoundland, Norfolk Terrier, Norwegian Buhund, Norwegian Elkhound, Norwegian Lundehund, Norwich Terrier, Nova Scotia Duck Tolling Retriever, Old English Sheepdog, Otterhound, Parson Russell Terrier, Pekingese, Pembroke Welsh Corgi, Pharaoh Hound, Pointer, Polish Lowland Sheepdog, Pomeranian, Poodle, Portuguese Water Dog, Pug, Puli, Pumi, Pyrenean Shepherd, Redbone Coonhound, Rhodesian Ridgeback, Rottweiler, Saint Bernard, Saluki, Samoyed, Schipperke, Scottish Deerhound, Scottish Terrier, Sealyham Terrier, Shetland Sheepdog, Shiba Inu, Shih Tzu, Siberian Husky, Silky Terrier, Skye Terrier, Sloughi, Soft Coated Wheaten Terrier, Spanish Water Dog, Standard Schnauzer, Sussex Spaniel, Swedish Vallhund, Tibetan Mastiff, Tibetan Spaniel, Tibetan Terrier, Toy Fox Terrier, Treeing Walker Coonhound, Vizsla, Weimaraner, Welsh Springer Spaniel, Welsh Terrier, West Highland White Terrier, Whippet, Wire Fox Terrier, Wirehaired Pointing Griffon, Wirehaired Vizsla, Xoloitzcuintli, Yorkshire Terrier. "

# Load raw source datasets
raw_pet    <- read.csv("data/pet-data.csv")
raw_owner  <- read.csv("data/owner-data.csv")
raw_policy <- read.csv("data/policy-data.csv")
raw_claims <- read.csv("data/claims-data.csv")

## Helper Functions ====
print_header <- function(heading, total_width = 70) {
  pad_len <- max(1, floor((total_width - (nchar(heading) + 2)) / 2))
  cat("\n\n", strrep("=", pad_len), " ", heading, " ", strrep("=", pad_len), "\n\n", sep = "")
}

# --- Comprehensive Data Overview ---
show_data_overview <- function(df, name, mcar_t=TRUE) {
  print_header(glue("{name}: Structure & Skim"))
  glimpse(df)
  print(skim(df))
  
  print_header(glue("{name}: Missingness Summary"))
  
  # naniar::miss_var_summary computes n_missing and pct_missing automatically
  miss_summary <- naniar::miss_var_summary(df) |>
    filter(n_miss > 0)
  print(as.data.frame(miss_summary), row.names = FALSE)
  print_header(glue("{name}: MCAR Test"))
  if(mcar_t){
    print(naniar::mcar_test(df))
  }
}

# --- Categorical Variable Profiling ---
profile_categorical_variables <- function(df, factor_cols, name) {
  print_header(glue("{name}: Profiling Categorical Variables"))
  
  # tabyl handles multiple columns cleanly when piped through purrr::map
  factor_cols |>
    set_names() |>
    map(\(col) df |>
          tabyl({{ col }}) |>
          adorn_pct_formatting(digits = 2))
}

# --- Duplicate & Key Integrity Checks ---
check_for_duplicates <- function(df, id_col, name) {
  id_name <- rlang::as_name(ensym(id_col))
  
  print_header(glue("{name}: Duplicate Summary for `{id_name}`"))
  dupe_rows <- df |> get_dupes({{ id_col }})
  n_dupes <- nrow(dupe_rows)
  cat(glue("Found {n_dupes} duplicate record(s) involving key: `{id_name}`.\n\n"))
  
  print_header(glue("{name}: Rows with Conflicting Attributes"))
  conflicts <- df |>
    filter(n_distinct(pick(everything())) > 1, .by = {{ id_col }}) |>
    arrange({{ id_col }})
  print(conflicts)
  
  invisible(list(dupes = dupe_rows, conflicts = conflicts))
}

# --- Claim Frequency Distribution ---
analyze_claims_distribution <- function(claims_df, name = "Claims") {
  print_header(glue("{name}: Claim Frequency per Policy"))
  
  dist_tbl <- claims_df |>
    count(policy.ID, name = "n_claims") |>
    tabyl(n_claims) |>
    adorn_pct_formatting(digits = 2)
  
  print(dist_tbl)
  invisible(dist_tbl)
}

# --- Outlier Analysis (Tukey IQR + Faceted Visualizations) ---
check_for_outliers <- function(df, cols, name) {
  # 1. Summary table using boxplot.stats
  summary_tbl <- map_dfr(cols, \(col) {
    x   <- na.omit(df[[col]])
    bp  <- boxplot.stats(x)
    iqr <- diff(bp$stats[c(2, 4)])
    tibble(
      Variable     = col,
      Q1           = bp$stats[2],
      Q3           = bp$stats[4],
      IQR          = iqr,
      Lower_Cutoff = bp$stats[1],
      Upper_Cutoff = bp$stats[5],
      N_Outliers   = length(bp$out),
      Pct_Outliers = scales::percent(length(bp$out) / length(x), accuracy = 0.01)
    )
  })
  print(summary_tbl)
  
  # 2. Reshape once for faceted plots
  long_df <- df |>
    select(all_of(cols)) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    drop_na()
  
  # 3. Boxplot & Histogram
  p_box <- ggplot(long_df, aes(y = value)) +
    geom_boxplot(fill = "lightblue", outlier.color = "firebrick") +
    facet_wrap(~ variable, scales = "free") +
    labs(title = glue("{name}: Boxplots"), x = NULL, y = NULL) +
    theme_minimal() + theme(axis.text.x = element_blank())
  
  p_hist <- ggplot(long_df, aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ variable, scales = "free") +
    labs(title = glue("{name}: Histograms"), x = NULL, y = "Count") +
    theme_minimal()
  
  print(p_box)
  print(p_hist)
}


## ========= Policy Data ===========

show_data_overview(raw_policy, "Policy") 
profile_categorical_variables(raw_policy, c("policy.NBRB"), "Policy")
check_for_duplicates(raw_policy, policy.ID, "Policy")
check_for_outliers( raw_policy, c("policy.Exposure"), "Policy" )

# Zero exposure records present
# Dates are in decimal years, needs parsing
# MCAR test suggests missingness is completely at random
# Miscoded policy.NBRB categories: `NewBus` -> `NB`, `RedBus` -> `RB`
# No duplicates. Good

# Clean Policy: filter out zero-exposure policies, parse dates, standardize NBRB
policy <- raw_policy |>
  filter(policy.Exposure > 0) |>
  mutate(
    policy.Start = lubridate::date_decimal(policy.Start),
    policy.End = lubridate::date_decimal(policy.End),
    policy.NBRB = factor(case_match(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB))
  )

## ========== Pets Data ============

show_data_overview(raw_pet, "Pets")
profile_categorical_variables(raw_pet, c("pet.Type", "pet.Group", "pet.Gender", "pet.Offspring", "pet.Microchip", "pet.Origin"), "Pets")
check_for_duplicates(raw_pet, pet.ID, "Pets")
check_for_outliers(raw_pet, c("pet.Age", "pet.Height", "pet.Weight"), "Pets")

# No anomalies in pet.Age, pet.Height and pet.Weight numerical values
# MCAR test suggests missingness is completely at random
# Miscodings found in: pet.Gender, pet.Microchip, pet.Group

### An interesting finding: Extra Dog breeds ====

# Checking if all breed names are present in the appendix
check_breeds <- function(breed = "Dog",
                         appendix = appendix_breed_dogs,
                         df = raw_pet) {
  breeds_expected <- appendix |>
    str_replace_all("[\r\n.]+", " ") |>
    str_remove("\\.+$") |>
    str_squish() |>
    str_split_1("\\s*,\\s*")
  
  print_header(glue("Check discrepancies in `{breed}` breed"))
  breeds_actual <- as.list(
    df |>
      filter(pet.Type == breed & !is.na(pet.Breed)) |>
      select(pet.Breed) |>
      unnest(pet.Breed) |>
      unique() |>
      pull()
  )
  
  print(glue("{breed} breeds present in the data, but missing from appendix list:"))
  extra_breeds <- setdiff(breeds_actual, breeds_expected)
  cat(paste0(seq_along(extra_breeds), ". ", extra_breeds, "\n"), sep = "")
  
  return(extra_breeds)
}

extra_dog_breeds <- check_breeds("Dog", appendix_breed_dogs)
extra_cat_breeds <- check_breeds("Cat", appendix_breed_cats)
raw_pet |> filter(pet.Breed %in% extra_dog_breeds) |> count(pet.Breed, sort = TRUE)

# All breed names have been validated and found correct but
# we found four extra dog breeds in our dataset, not present in the appendix
# They seem represent a good number in the population, thus, we retain them.


# Clean Pets: standardize categorical miscodings, deduplicate resolving conflicts via non-NA
pets <- raw_pet |> 
  mutate(
    pet.Type = factor(pet.Type),
    pet.Group = factor(case_match(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group)),
    pet.Breed = factor(pet.Breed),
    pet.Gender = factor(case_match(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender)),
    pet.Offspring = factor(pet.Offspring),
    pet.Microchip = factor(case_match(pet.Microchip, "Nope" ~ "No", "Y" ~ "Yes", .default = pet.Microchip)),
    pet.Origin = factor(pet.Origin)
  ) |> 
  distinct() |>
  group_by(pet.ID) |>
  summarize(across(everything(), ~ na.omit(.x)[1]), .groups = "drop")


## ========= Owners Data ===========

show_data_overview(raw_owner, "Owner")
profile_categorical_variables(raw_owner, c("owner.Gender", "owner.Location", "owner.Channel", "owner.PetCount"), "Owners")
check_for_duplicates(raw_owner, owner.ID, "Owners")
check_for_outliers(raw_owner, c("owner.Age", "owner.CreditScore", "owner.PetCount"), "Owners")

# No anomalies in owner.Age, owner.CreditScore, owner.PetCount (valid ranges)
# MCAR test suggests missingness is completely at random
# No miscodings found
# 219 duplicate owner.ID records found, with four owners having conflicting records
# Number of pets for the owner in the pet dataset

# Clean Owners: dedupe by taking maximum recorded pet count and non-NA attributes
owners <- raw_owner |>
  group_by(owner.ID) |>
  summarize(
    owner.PetCount = max(owner.PetCount, na.rm = TRUE),
    across(-owner.PetCount, ~ na.omit(.x)[1]),
    .groups = "drop"
  ) |>
  mutate(
    owner.Gender   = factor(owner.Gender),
    owner.Location = factor(owner.Location),
    owner.Channel  = factor(owner.Channel)
  )

## ========= Claims Data ===========
show_overview(raw_claims, "Claims")
profile_categorical_variables(raw_claims, c("claim.Coverage"), "Claims")
check_for_duplicates(raw_claims, claim.ID, "Claims")
check_for_outliers(raw_claims, c("claim.Cost"), "Claims")
analyze_claims_distribution(raw_claims, "Claims")

# Aggregate Claims: combine illness and accident losses at policy level
claims_agg <- raw_claims |>
  group_by(policy.ID) |>
  summarize(
    n_claims      = n(),
    illness_loss  = sum(claim.Cost[claim.Coverage == "Illness"]),
    accident_loss = sum(claim.Cost[claim.Coverage == "Accident"]),
    total_losses  = sum(claim.Cost),
    .groups       = "drop"
  )

## ==== Merge datasets: Policy base + Pets + Owners + Claims =====

merged_df <- policy |>
  left_join(pets, by = "pet.ID") |>
  left_join(owners, by = "owner.ID") |>
  left_join(claims_agg, by = "policy.ID") |>
  mutate(
    n_claims      = replace_na(n_claims, 0),
    illness_loss  = replace_na(illness_loss, 0),
    accident_loss = replace_na(accident_loss, 0),
    total_losses  = replace_na(total_losses, 0),
    pure_premium  = total_losses / policy.Exposure
  )

# Remove rows with missing values (negligible impact: ~2.9% records dropped)
clean_df <- merged_df |> drop_na()

# Stratified Train/Test Split (70% train / 30% test)
set.seed(42)
train_idx <- createDataPartition(clean_df$total_losses, p = 0.70, list = FALSE)
train <- clean_df[train_idx, ]
test  <- clean_df[-train_idx, ]

# ========== 3. Exploratory Data Analysis & Feature Engineering ================

# Cap extreme losses at upper fence ($14,949) to prevent GLM distortion from rare extreme events
bp_stats    <- boxplot.stats(train$total_losses[train$total_losses > 0])$stats
upper_fence <- bp_stats[5]
train$total_losses <- pmin(train$total_losses, upper_fence)
train$pure_premium <- train$total_losses / train$policy.Exposure

# Outlier Breed Identification: 24 high-divergence breeds mapped to 3-tier rating factor
surcharge_breeds <- c(
  "Poodle", "Airedale Terrier", "Afghan Hound", "Bull Terrier", "Chausie",
  "Kerry Blue Terrier", "Welsh Springer Spaniel", "Chow Chow", 
  "Soft Coated Wheaten Terrier", "American Staffordshire Terrier", "Pointer", 
  "Polish Lowland Sheepdog", "Collie", "Staffordshire Bull Terrier", 
  "Black and Tan Coonhound", "Dalmatian"
)

discount_breeds <- c(
  "Australian Terrier", "Pumi", "Shetland Sheepdog", "Beagle", 
  "Pyrenean Shepherd", "Pembroke Welsh Corgi", "Rat Terrier", "Norfolk Terrier"
)

apply_feature_engineering <- function(df) {
  df |>
    mutate(
      breed_outlier = factor(
        case_when(
          pet.Breed %in% surcharge_breeds ~ "Surcharge",
          pet.Breed %in% discount_breeds  ~ "Discount",
          TRUE ~ "Standard"
        ),
        levels = c("Standard", "Surcharge", "Discount")
      ),
      pet_count_band = factor(
        ifelse(owner.PetCount >= 5, "5+", as.character(owner.PetCount)),
        levels = c("1", "2", "3", "4", "5+")
      ),
      credit_score_band = cut(
        owner.CreditScore,
        breaks = seq(300, 850, by = 50),
        include.lowest = TRUE
      )
    )
}

train <- apply_feature_engineering(train)
test  <- apply_feature_engineering(test)

# Set reference levels for categorical variables
cat_cols <- c("policy.NBRB", "pet.Type", "pet.Group", "pet.Gender", "pet.Offspring",
              "pet.Microchip", "pet.Origin", "owner.Gender", "owner.Location", "owner.Channel", "breed_outlier")

train <- train |>
  mutate(across(all_of(cat_cols), factor)) |>
  mutate(
    pet.Type       = relevel(pet.Type, ref = "Dog"),
    pet.Group      = relevel(pet.Group, ref = "Shorthair"),
    pet.Gender     = relevel(pet.Gender, ref = "Male"),
    pet.Offspring  = relevel(pet.Offspring, ref = "No"),
    pet.Microchip  = relevel(pet.Microchip, ref = "Yes"),
    pet.Origin     = relevel(pet.Origin, ref = "Store"),
    owner.Gender   = relevel(owner.Gender, ref = "Male"),
    owner.Location = relevel(owner.Location, ref = "Suburban"),
    policy.NBRB    = relevel(policy.NBRB, ref = "RB"),
    breed_outlier  = relevel(breed_outlier, ref = "Standard")
  )

test <- test |>
  mutate(across(all_of(cat_cols), factor)) |>
  mutate(
    pet.Type       = relevel(pet.Type, ref = "Dog"),
    pet.Group      = relevel(pet.Group, ref = "Shorthair"),
    pet.Gender     = relevel(pet.Gender, ref = "Male"),
    pet.Offspring  = relevel(pet.Offspring, ref = "No"),
    pet.Microchip  = relevel(pet.Microchip, ref = "Yes"),
    pet.Origin     = relevel(pet.Origin, ref = "Store"),
    owner.Gender   = relevel(owner.Gender, ref = "Male"),
    owner.Location = relevel(owner.Location, ref = "Suburban"),
    policy.NBRB    = relevel(policy.NBRB, ref = "RB"),
    breed_outlier  = relevel(breed_outlier, ref = "Standard")
  )

# ================ 4. GLM Development & Variable Selection =====================

# Estimate initial Tweedie variance power p via profile likelihood
p_prof_init <- tweedie.profile(
  total_losses ~ 1,
  p.vec   = seq(1.1, 1.9, by = 0.1),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)

p_prof_ref <- tweedie.profile(
  total_losses ~ 1,
  p.vec   = seq(p_prof_init$p.max - 0.1, p_prof_init$p.max + 0.1, by = 0.01),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)
p_initial <- p_prof_ref$p.max
cat("Initial Tweedie p:", round(p_initial, 4), "\n")

# Model A: Full Baseline GLM (excluding pet.Breed for high cardinality and pet.Type for exact aliasing with pet.Group)
model_A <- glm(
  total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight +
    pet.Offspring + pet.Microchip + pet.Origin + owner.Age + owner.Gender +
    owner.Location + owner.CreditScore + owner.Channel + owner.PetCount + policy.NBRB +
    offset(log(policy.Exposure)),
  data   = train,
  family = statmod::tweedie(var.power = p_initial, link.power = 0)
)
cat("Model A (Baseline) AIC:", round(tweedie_AIC(model_A), 2), "\n")

# Model B: Backward Variable Selection using drop1 Chi-square test (p < 0.05)
# Variables removed: owner.Channel, owner.Age, owner.Location, owner.CreditScore, pet.Origin, pet.Microchip, pet.Offspring
model_B <- glm(
  total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight +
    owner.PetCount + policy.NBRB + offset(log(policy.Exposure)),
  data   = train,
  family = statmod::tweedie(var.power = p_initial, link.power = 0)
)
cat("Model B (Selected Main Effects) AIC:", round(tweedie_AIC(model_B), 2), "\n")

# Model C: Add Quadratic Senior Age effect I(pet.Age^2)
model_C <- update(model_B, . ~ . + I(pet.Age^2))
cat("Model C (+ Quadratic Age) AIC:", round(tweedie_AIC(model_C), 2), "\n")

# Model D: Add Morphology & Build Interaction pet.Height:pet.Weight
model_D <- update(model_C, . ~ . + pet.Height:pet.Weight)
cat("Model D (+ Morphology Interaction) AIC:", round(tweedie_AIC(model_D), 2), "\n")

# Model E: Add Outlier Breed Rating Tier
model_E <- update(model_D, . ~ . + breed_outlier)
cat("Model E (+ Outlier Breed Tier) AIC:", round(tweedie_AIC(model_E), 2), "\n")

# ================= 5. Diagnostics & Iterative Refinement ======================

# Model F: Add Quadratic Height I(pet.Height^2) to capture non-linear stature/volume scaling
model_F <- update(model_E, . ~ . + I(pet.Height^2))
cat("Model F (+ Quadratic Height) AIC:", round(tweedie_AIC(model_F), 2), "\n")

# Confirm Tweedie distributional assumption using Dunn-Smyth (randomized quantile) residuals
q_resids <- statmod::qresid(model_F)
cat("Dunn-Smyth Quantile Residuals Summary (centered near 0 with standard normal quantiles):\n")
print(summary(q_resids))

# Re-profile Tweedie p on the refined model formula
p_recheck <- tweedie.profile(
  formula(model_F),
  p.vec   = seq(1.2, 1.6, by = 0.05),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)

p_refined <- tweedie.profile(
  formula(model_F),
  p.vec   = seq(p_recheck$p.max - 0.05, p_recheck$p.max + 0.05, by = 0.01),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)
p_final <- p_refined$p.max
cat("Re-profiled Final Tweedie p:", round(p_final, 4), "\n")

# Fit Final Model with re-profiled p = 1.3173
model_final <- glm(
  formula(model_F),
  data   = train,
  family = statmod::tweedie(var.power = p_final, link.power = 0)
)
cat("Final Model AIC:", round(tweedie_AIC(model_final), 2), "\n")
cat("Final Model % Deviance Explained:",
    round((1 - model_final$deviance / model_final$null.deviance) * 100, 2), "%\n")

# ================== 6. Holdout Validation on Test Data ========================

# Predict expected pure premium on holdout set
test$predicted_losses <- predict(model_final, newdata = test, type = "response")
test$predicted_pp     <- test$predicted_losses / test$policy.Exposure

# Double Lift Analysis across 10 deciles sorted by predicted-to-benchmark pure premium
benchmark_pp <- mean(test$pure_premium)
test$decile  <- ntile(test$predicted_pp / benchmark_pp, 10)

double_lift <- test |>
  group_by(decile) |>
  summarize(
    n_policies        = n(),
    exposure          = sum(policy.Exposure),
    actual_losses     = sum(total_losses),
    predicted_losses  = sum(predicted_losses),
    mean_actual_pp    = round(actual_losses / exposure, 2),
    mean_predicted_pp = round(predicted_losses / exposure, 2),
    ae_ratio          = round(actual_losses / predicted_losses, 3),
    .groups           = "drop"
  )
cat("\n=== Holdout Decile Calibration & Double Lift ===\n")
print(as.data.frame(double_lift))

# Gini Coefficient Calculation
gini_df <- test |>
  arrange(predicted_pp) |>
  mutate(
    cum_exp  = cumsum(policy.Exposure) / sum(policy.Exposure),
    cum_loss = cumsum(total_losses) / sum(total_losses)
  )

n_obs <- nrow(gini_df)
gini_area <- sum(
  (gini_df$cum_exp[2:n_obs] - gini_df$cum_exp[1:(n_obs - 1)]) *
  (gini_df$cum_loss[2:n_obs] + gini_df$cum_loss[1:(n_obs - 1)]) / 2
)
gini_coef <- 1 - 2 * gini_area
cat(sprintf("Holdout Gini Coefficient: %.4f\n", gini_coef))

# ====== 7. Scoring Assessment Data Set (assessment-data.csv -> scored.csv) ===

# Load assessment dataset (unseen production data)
assess_raw <- read.csv("data/assessment-data.csv")

# Clean and format assessment data matching training pipeline
assess_clean <- assess_raw |>
  mutate(
    policy.NBRB = factor(case_match(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB)),
    pet.Type    = factor(pet.Type),
    pet.Group   = factor(case_match(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group)),
    pet.Gender  = factor(case_match(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender)),
    breed_outlier = factor(
      case_when(
        pet.Breed %in% surcharge_breeds ~ "Surcharge",
        pet.Breed %in% discount_breeds  ~ "Discount",
        TRUE ~ "Standard"
      ),
      levels = c("Standard", "Surcharge", "Discount")
    )
  )

# Predict annual expected pure premium (PPP)
# Model output under log(policy.Exposure) offset represents expected losses.
# Dividing by policy.Exposure gives the annualized pure premium rate (PPP).
assess_clean$PPP <- predict(model_final, newdata = assess_clean, type = "response") / assess_clean$policy.Exposure

# Export scored file with exact required columns: policy.ID and PPP
scored_output <- assess_clean |>
  select(policy.ID, PPP)

write.csv(scored_output, "scored.csv", row.names = FALSE)
cat(sprintf("Scored %d records. File saved to scored.csv\n", nrow(scored_output)))
