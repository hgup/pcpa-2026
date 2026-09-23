# ================== CAS PCPA - Predictive Analytics Project ===================
# Project Wolverine: Pet Insurance Pure Premium Modeling
# Master Implementation Code File

# ======================= 1. Load Required Libraries ===========================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, scales, corrplot, rcompanion,
  janitor, skimr, naniar, glue, broom,
  statmod, tweedie, car, caret
)

set.seed(42)

# ================= 2. Data Ingestion, Cleaning & Merging ======================

## 2.0 Data Loading, Appendix & Functions ======================================

# String of names copied from the appendix (data dictionary)
appendix_breed_cats <- "Abyssinian, American Bobtail, American Curl, American Shorthair, American Wirehair, Asian, Asian Semi Longhair, Australian Mist, Balinese, Bambino, Bengal, Birman, Bombay, Brazilian Shorthair, British Shorthair, Burmese, Chantilly Tiffany, Chartreux, Chausie, Cornish Rex, Cymric, Cyprus, Devon Rex, Donskoy, Egyptian Mau, European Burmese, European Shorthair, Exotic Shorthair, German Rex, Havana, Havana Brown, Highlander, Himalayan, Japanese Bobtail, Javanese, Korat, Kurilian Bobtail, LaPerm, Lykoi, Maine Coon, Manx, Minskin, Munchkin, Napoleon, Nebelung, Ocicat, Ojos Azules, Oriental, Oriental Bicolor, Oriental Longhair, Persian, Peterbald, Pixiebob, Ragamuffin, Ragdoll, Russian Blue, Scottish Fold, Selkirk Rex, Seychellois, Siamese, Siberian, Siberian Forest, Singapura, Snowshoe, Sokoke, Somali, Sphynx, Thai, Tiffany, Tonkinese, Toybob, Toyger, Turkish Angora, Turkish Van, Ukrainian Levkoy, Ussuri, York Chocolate"
appendix_breed_dogs <- " Afghan Hound, Airedale Terrier, Akita, Alaskan Malamute, American Eskimo Dog, American Foxhound, American Staffordshire Terrier, Anatolian Shepherd Dog, Australian Cattle Dog, Australian Shepherd, Australian Terrier, Basenji, Basset Hound, Beagle, Bearded Collie, Beauceron, Bedlington Terrier, Belgian Malinois, Belgian Sheepdog, Belgian Tervuren, Bernese Mountain Dog, Bichon Frise, Black and Tan Coonhound, Bloodhound, Border Collie, Border Terrier, Borzoi, Boston Terrier, Boxer, Boykin Spaniel, Briard, Brittany, Brussels Griffon, Bull Terrier, Bulldog, Cairn Terrier, Canaan Dog, Cardigan Welsh Corgi, Cavalier King Charles Spaniel, Chesapeake Bay Retriever, Chihuahua, Chinese Crested, Chow Chow, Clumber Spaniel, Cocker Spaniel, Collie, Coton de Tulear, Dachshund, Dalmatian, Dandie Dinmont Terrier, Doberman Pinscher, Dogo Argentino, English Cocker Spaniel, English Foxhound, English Setter, English Springer Spaniel, English Toy Spaniel, Entlebucher Mountain Dog, Field Spaniel, Finnish Lapphund, Finnish Spitz, French Bulldog, German Pinscher, German Shepherd Dog, German Shorthaired Pointer, German Wirehaired Pointer, Giant Schnauzer, Glen of Imaal Terrier, Golden Retriever, Gordon Setter, Great Dane, Great Pyrenees, Greater Swiss Mountain Dog, Greyhound, Harrier, Havanese, Irish Red and White Setter, Irish Setter, Irish Terrier, Irish Water Spaniel, Irish Wolfhound, Italian Greyhound, Japanese Chin, Keeshond, Kerry Blue Terrier, Komondor, Kuvasz, Labrador Retriever, Lagotto Romagnolo, Lakeland Terrier, Leonberger, Lhasa Rapso, Maltese, Manchester Terrier, Mastiff, Miniature Bull Terrier, Miniature Pinscher, Miniature Schnauzer, Newfoundland, Norfolk Terrier, Norwegian Buhund, Norwegian Elkhound, Norwegian Lundehund, Norwich Terrier, Nova Scotia Duck Tolling Retriever, Old English Sheepdog, Otterhound, Parson Russell Terrier, Pekingese, Pembroke Welsh Corgi, Pharaoh Hound, Pointer, Polish Lowland Sheepdog, Pomeranian, Poodle, Portuguese Water Dog, Pug, Puli, Pumi, Pyrenean Shepherd, Redbone Coonhound, Rhodesian Ridgeback, Rottweiler, Saint Bernard, Saluki, Samoyed, Schipperke, Scottish Deerhound, Scottish Terrier, Sealyham Terrier, Shetland Sheepdog, Shiba Inu, Shih Tzu, Siberian Husky, Silky Terrier, Skye Terrier, Sloughi, Soft Coated Wheaten Terrier, Spanish Water Dog, Standard Schnauzer, Sussex Spaniel, Swedish Vallhund, Tibetan Mastiff, Tibetan Spaniel, Tibetan Terrier, Toy Fox Terrier, Treeing Walker Coonhound, Vizsla, Weimaraner, Welsh Springer Spaniel, Welsh Terrier, West Highland White Terrier, Whippet, Wire Fox Terrier, Wirehaired Pointing Griffon, Wirehaired Vizsla, Xoloitzcuintli, Yorkshire Terrier. "
dog_groups <- c("Herding", "Hound", "Non-Sporting", "Sporting", "Terrier", "Toy", "Working")
cat_groups <- c("Domestic", "Pedigree")

# Load raw source datasets
raw_pet    <- read.csv("data/pet-data.csv")
raw_owner  <- read.csv("data/owner-data.csv")
raw_policy <- read.csv("data/policy-data.csv")
raw_claims <- read.csv("data/claims-data.csv")

# --- Clean Headers ---
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


## 2.1 Policy Data =============================================================

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
    policy.NBRB = recode_values(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB)
  )

## 2.2 Pets Data ===============================================================

show_data_overview(raw_pet, "Pets")
profile_categorical_variables(raw_pet, c("pet.Type", "pet.Group", "pet.Gender", "pet.Offspring", "pet.Microchip", "pet.Origin"), "Pets")
check_for_duplicates(raw_pet, pet.ID, "Pets")
check_for_outliers(raw_pet, c("pet.Age", "pet.Height", "pet.Weight"), "Pets")

# No anomalies in pet.Age, pet.Height and pet.Weight numerical values
# MCAR test suggests missingness is completely at random
# Miscodings found in: pet.Gender, pet.Microchip, pet.Group

### 2.2.a An interesting finding: Extra Dog breeds ====

# Checking if all breed names are present in the appendix
get_breed_list <- function(appendix) appendix |> str_replace_all("[\r\n.]+", " ") |>
    str_remove("\\.+$") |> str_squish() |> str_split_1("\\s*,\\s*")

dog_breeds <- get_breed_list(appendix_breed_dogs)  
cat_breeds <- get_breed_list(appendix_breed_cats)  

check_breeds <- function(breed = "Dog", breed_list = dog_breeds, df = raw_pet) {
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
  extra_breeds <- setdiff(breeds_actual, breed_list)
  cat(paste0(seq_along(extra_breeds), ". ", extra_breeds, "\n"), sep = "")
  return(extra_breeds)
}

extra_dog_breeds <- check_breeds("Dog", dog_breeds)
extra_cat_breeds <- check_breeds("Cat", cat_breeds)

raw_pet |> filter(pet.Breed %in% extra_dog_breeds) |> count(pet.Breed, sort = TRUE)

# All breed names have been validated and found correct but
# we found four extra dog breeds in our dataset, not present in the appendix
# They seem represent a good number in the population, thus, we retain them.


# Clean Pets: standardize categorical miscodings, deduplicate resolving conflicts via non-NA
pets <- raw_pet |> 
  mutate(
    # The missing pet.Type can be inferred if pet.Group, 
    pet.Type = case_when(
      !is.na(pet.Type) & pet.Type != "" ~ pet.Type,
      pet.Group %in% dog_groups | pet.Breed %in% dog_breeds ~ "Dog",
      pet.Group %in% cat_groups | pet.Breed %in% cat_breeds ~ "Cat",
      TRUE                              ~ pet.Type
    ),
    pet.Group = recode_values(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group),
    pet.Gender = recode_values(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender),
    pet.Microchip = recode_values(pet.Microchip, "Nope" ~ "No", "Y" ~ "Yes", .default = pet.Microchip)
  ) |> 
  distinct() |>
  group_by(pet.ID) |> 
  summarize(across(everything(), ~ na.omit(.x)[1]),.groups = "drop")


## 2.3 Owners Data =============================================================

show_data_overview(raw_owner, "Owner")
profile_categorical_variables(raw_owner, c("owner.Gender", "owner.Location", "owner.Channel", "owner.PetCount"), "Owners")
check_for_duplicates(raw_owner, owner.ID, "Owners")
check_for_outliers(raw_owner, c("owner.Age", "owner.CreditScore", "owner.PetCount"), "Owners")

# No anomalies in owner.Age, owner.CreditScore, owner.PetCount (valid ranges)
# MCAR test suggests missingness is completely at random
# No miscodings found
# 219 duplicate owner.ID records found, with four owners having conflicting records

# Clean Owners: dedupe by taking maximum recorded pet count and non-NA attributes
owners <- raw_owner |>
  group_by(owner.ID) |> 
  summarize(
    owner.PetCount = max(owner.PetCount, na.rm = TRUE),
    across(-owner.PetCount, ~ na.omit(.x)[1]),
    .groups = "drop"
  )

## 2.4 Claims Data =============================================================

show_data_overview(raw_claims, "Claims")
profile_categorical_variables(raw_claims, c("claim.Coverage"), "Claims")
check_for_duplicates(raw_claims, claim.ID, "Claims")
check_for_outliers(raw_claims, c("claim.Cost"), "Claims")
analyze_claims_distribution(raw_claims, "Claims")

# No missing values found. No duplicate claim.ID records found.
# Claims distribution has higher illness claims
# 84% of policies have one claim, 12.8% have two, remaining (<2.3%)
# claim.Cost exhibits a right-skewed distribution with severe high-cost outliers

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

## 2.5 Merge datasets w/ Policy (base) =========================================


merged_df <- policy |>
  left_join(pets, by = "pet.ID") |>
  left_join(owners, by = "owner.ID") |>
  left_join(claims_agg, by = "policy.ID") |>
  mutate(
    n_claims = replace_na(n_claims, 0),
    illness_loss = replace_na(illness_loss, 0),
    accident_loss = replace_na(accident_loss, 0),
    total_losses = replace_na(total_losses, 0),
    pure_premium = total_losses / policy.Exposure
  )

## 2.6 Handle Missing Values and Impact Analysis ===============================

# As per project instructions, imputation is outside the scope of the study
# so we will just drop the variables and note their impact on our analysis.
clean_df <- drop_na(merged_df)

calc_metrics <- function(df) {
    c(Rows = nrow(df),
    Exposure = sum(df$policy.Exposure),
    Claims = sum(df$n_claims),
    Losses = sum(df$total_losses))
  }

impact_summary <- tibble(
  Metric = c("Policy Count (Rows)", "Earned Exposure (Years)", "Claim Count (N)", "Total Incurred Loss ($)"),
  Before = calc_metrics(merged_df),
  After = calc_metrics(clean_df),
  Change = After - Before,
  Pct_Retained = scales::percent(After / Before, accuracy = 0.01)
  ) |> mutate(across(c(Before, After, Change), \(x) comma(round(x,1))))

impact_summary
# There is negligible Data loss, eliminating roughly 3% of the portfolio.


## 2.7 Train/Test Split (70% train / 30% test) =================================

set.seed(42)
train_idx <- createDataPartition(clean_df$total_losses, p = 0.70, list = FALSE)
train <- clean_df[train_idx, ]
test  <- clean_df[-train_idx, ]

# ========== 3. Exploratory Data Analysis & Feature Engineering ================

plot_hist <- function(.data , var, x_lab="Total Losses ($)") {
  .data |> 
    filter(total_losses > 0) |> 
    ggplot(aes(x={{var}})) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title = glue("Distribution of {x_lab} (Claimants Only)"),
      x     = "Total Losses ($)",
      y     = "Count"
    ) +
    theme_minimal()
}

plot_boxplot <- function(.data, col){
  col_name <- rlang::as_name(ensym(col))
  .data |> ggplot(aes(y = {{col}})) +
    geom_boxplot(fill = "steelblue") +
    labs(title = glue("Boxplot of {col_name}"), y =col_name) +
    theme_minimal()
}

train |> plot_hist(pure_premium, x_lab = "Pure Premium ($)")

## 3.1 Reviewing Total Losses ==================================================

# The total losses are heavily right skewed and have potential outliers
train |> plot_hist(total_losses, x_lab = "Total Losses ($)")

# The log transformation adjusts the right skewness and ensures positive values
train |> 
  mutate(log_total_loss = log(total_losses)) |>
  plot_hist(log_total_loss, x_lab = "Log Total Losses ($)")

# Cap extreme losses at upper fence ($3,981) to prevent model distortion due to rare events
bp_stats <- boxplot.stats(train$total_losses[train$total_losses > 0])$stats
upper_fence <- bp_stats[5]
tibble(Stat = c("Lower Fence","Q1", "Median","Q3","Upper fence"), Value = bp_stats)
train$total_losses <- pmin(train$total_losses, upper_fence)
train$pure_premium <- train$total_losses / train$policy.Exposure

# Accumulation of total losses near $4000 due to the cap
train |> plot_hist(total_losses, x_lab = "Total Losses ($)")


## 3.2 Reviewing Pure Premium ==================================================

train |> plot_hist(pure_premium, x_lab = "Pure Premium ($)")
train |> filter(total_losses > 0) |>  plot_boxplot(pure_premium)

# Log transformation normalizes severe right skew into a symmetric bell curve,
# supporting the choice of a log link function for the GLM.
train |>  mutate(log_pp = log(pure_premium)) |> 
  plot_hist(log_pp, x_lab = "Log Pure Premium ($)")

## 3.3 Reviewing Frequency & Severity ==========================================

# We see right skewness in both frequency and severity

train <- train |>  mutate(frequency = n_claims/policy.Exposure)
summary(train$frequency)
train |> plot_hist(frequency, x_lab = "Claim Frequency")

train <- train |>  mutate(severity = total_losses/n_claims)
summary(train$severity)
train |> plot_hist(severity, x_lab = "Severity")


## 3.4 Relationship between Frequency & Severity ===============================

# We found no significant correlation (0.0255) between frequency and severity,
# indicating that it isn't a case of adverse selection. Thus we need not model
# them separately.
train |>  filter(n_claims > 0) |> 
  summarize(correlation = round(cor(frequency, severity), 4))

## 3.4 Univariate Analysis =====================================================

# One-way summary & dual-axis plot (count and pure premium)
univar_analysis <- function(df, group_col, x_label = NULL, title = NULL, reorder_by_pp = FALSE) {
  col_name <- rlang::as_label(rlang::enquo(group_col))
  x_label  <- x_label %||% col_name
  title    <- title %||% glue("One-Way Analysis: {col_name}")
  
  summary_df <- df |>
    summarize(
      n_policies = n(),
      mean_pp    = round(mean(pure_premium, na.rm = TRUE), 2),
      median_pp  = round(median(pure_premium, na.rm = TRUE), 2),
      .by = {{ group_col }}
    )
  
  if (reorder_by_pp) {
    summary_df <- summary_df |>
      arrange(mean_pp) |>
      mutate({{ group_col }} := factor({{ group_col }}, levels = unique({{ group_col }})))
  } else {
    summary_df <- summary_df |> arrange({{ group_col }})
  }
  
  max_count    <- max(summary_df$n_policies, na.rm = TRUE)
  max_pp       <- max(summary_df$mean_pp, na.rm = TRUE)
  scale_factor <- if (is.finite(max_pp) && max_pp > 0) max_count / max_pp else 1
  
  print(
    ggplot(summary_df, aes(x = {{ group_col }})) +
      geom_col(aes(y = n_policies / scale_factor), fill = "steelblue", alpha = 0.35) +
      geom_line(aes(y = mean_pp, group = 1), color = "navy", linewidth = 1) +
      geom_point(aes(y = mean_pp), color = "navy", size = 2) +
      scale_y_continuous(
        name     = "Mean Pure Premium",
        labels   = scales::comma,
        sec.axis = sec_axis(~ . * scale_factor, name = "Policy Count", labels = scales::comma),
        expand   = expansion(mult = c(0, 0.05))
      ) +
      labs(title = title, x = x_label) +
      theme_minimal()
  )
  
  summary_df
}

# Continuous univariate wrapper (binned)
univar_analysis_continuous <- function(df = train, col, bands = seq(10, 100, by = 10), include.lowest = TRUE) {
  col_name <- rlang::as_label(rlang::enquo(col))
  df |>
    mutate(band = cut({{ col }}, breaks = bands, include.lowest = include.lowest)) |>
    univar_analysis(
      group_col     = band,
      x_label       = glue("{col_name} Band"),
      title         = glue("One-Way Analysis: {col_name} ({length(bands) - 1} Bins)"),
      reorder_by_pp = FALSE
    ) |>
    rename("{col_name}_band" := band)
}

# Categorical univariate wrapper (factor)
univar_analysis_categorical <- function(df = train, col, reorder = TRUE) {
  col_name <- rlang::as_label(rlang::enquo(col))
  univar_analysis(
    df            = df,
    group_col     = {{ col }},
    x_label       = col_name,
    title         = glue("One-Way Analysis: {col_name} (Factor)"),
    reorder_by_pp = reorder
  )
}

### 3.4.1 Policy Variables =====================================================
train |> univar_analysis_categorical(policy.NBRB)
train |> 
  mutate(year = year(policy.Start)) |> 
  arrange(year) |> 
  univar_analysis_categorical(year)

### 3.4.2 Pet Variables ========================================================
train |> univar_analysis_categorical(pet.Group)
train |> univar_analysis_categorical(pet.Type)
train |> univar_analysis_continuous(pet.Age, seq(1,17, by=2))
train |> univar_analysis_categorical(pet.Gender)
train |> univar_analysis_continuous(pet.Height, seq(4,35, by=5))
train |> univar_analysis_continuous(pet.Weight, seq(0,240, by=30))
train |> univar_analysis_continuous(pet.Weight, c(0,30,150,210,240))
train |> univar_analysis_categorical(pet.Offspring)
train |> univar_analysis_categorical(pet.Origin)
train |> univar_analysis_categorical(pet.Microchip)

### 3.4.3 Owners Variables =====================================================
train |> univar_analysis_continuous(owner.Age, seq(17,78, by=10))
train |> univar_analysis_categorical(owner.Gender)
train |> univar_analysis_categorical(owner.Location)
train |> univar_analysis_continuous(owner.CreditScore, seq(300, 850, by=50))
train |> univar_analysis_categorical(owner.Channel)
train |> univar_analysis_categorical(owner.PetCount)
train |> mutate(
    pet_count_band = factor(
      ifelse(owner.PetCount >= 5, "5+", as.character(owner.PetCount)),
      levels = c("1", "2", "3", "4", "5+") )) |>
  univar_analysis_categorical(pet_count_band)



## 3.5 Multivariate Analysis ===================================================

### 3.5.1 Correlation Matrix for continuous predictors =========================

# pet.Weight & pet.Height have a high correlation 0.89
cont_vars <- train |>  select(where(is.numeric))
cor_matrix <- cor(cont_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
   addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex = 0.7,
   title = "Correlation Matrix - Continuous Predictors",
   mar = c(0,0,2,0)
 )

# For answers to the Questiosn section about "Raking absolute value of correlation"
# 1.  0.0073
# 2. -0.0109
# 3.  0.8902
# 4. -0.0023

### 3.5.2 Cramer's V for categorical predictors ================================

# We don't observe correlation between categorical predictors as well
cat_vars <- train |> select(where(is.character), -matches("ID|Breed"))
cols <- names(cat_vars)
cramer_matrix <- outer(cols, cols,
   Vectorize(function(i, j) {
    cramerV(cat_vars[[i]], cat_vars[[j]], bias.correct = TRUE) 
   }))
dimnames(cramer_matrix) <- list(cols, cols)
corrplot(cramer_matrix, method = "color", type = "upper",
  is.corr = FALSE, col.lim = c(0, 1), addCoef.col = "black",
  tl.col = "black", tl.srt = 45, number.cex = 0.7,
  title = "Cramer's V Association - Categorical Predictors",
  mar = c(0, 0, 2, 0)
)

### 3.5.3 Relationship between categorical predictors ==========================

#### 3.5.3.a pet.Type & pet.Group =============================================

# Observation: Dogs have twice the pure premium than cats.
train |>  summarize(
    n_groups = n_distinct(pet.Group),
    mean_pp = round(mean(pure_premium), 2),
    .by = pet.Type)

# We observe the same signal from the groups as well, Dog groups have
# twice the pure premium as that of Cat Groups
train |> group_by(pet.Group, Species = pet.Type) |>
  summarize(
    n_breeds = n_distinct(pet.Breed),
    mean_pp  = round(mean(pure_premium), 2),
    # exp_weighted_pp = round(sum(total_losses)/sum(policy.Exposure),2),
    .groups  = "drop"
  ) |> arrange(Species, mean_pp)

# Thus, we can skip pet.Type in favor of pet.Group as the latter captures the
# same signal but also reveals within-species differentiation (Working dogs: 397 vs Toy dogs: 201)

#### 3.5.3.b pet.Group & pet.Breed =============================================

# Within the Group, certain breeds seem to exhibit a signal where they diverge from
# the overall group behavior. This adverse or favorable behavior is a signal that can be
# utilized in the model.

breed_signal <- train |>
  mutate(group_pp = sum(total_losses) / sum(policy.Exposure), .by = pet.Group) |>
  summarize(
    n_policies = n(),
    breed_pp   = sum(total_losses) / sum(policy.Exposure),
    group_pp   = group_pp[1],
    diff       = breed_pp - group_pp,
    pct_diff   = diff / group_pp,
    .by        = c(pet.Group, pet.Breed)
  ) |>
  slice_max(abs(pct_diff), n = 24)

pal <- c(
  "Group Benchmark"        = "#5B9BD5",  # Blue
  "Adverse (+ Surcharge)"  = "#C00000",  # Red
  "Favorable (- Discount)" = "#385723"   # Green
)

breed_signal |>
  arrange(diff > 0, abs(pct_diff)) |>
  mutate(
    type        = if_else(diff > 0, "Adverse (+ Surcharge)", "Favorable (- Discount)"),
    breed_label = fct_inorder(glue("{pet.Breed} ({pet.Group})")),
    label_text  = sprintf("%+0.0f%%", pct_diff * 100)
  ) |>
  ggplot(aes(y = breed_label)) +
  geom_segment(aes(x = group_pp, xend = breed_pp, yend = breed_label), color = "grey65", linewidth = 1.2) +
  geom_point(aes(x = group_pp, fill = "Group Benchmark"), shape = 21, size = 3.5, stroke = 1) +
  geom_point(aes(x = breed_pp, fill = type), shape = 21, size = 3.8, stroke = 1) +
  geom_text(
    aes(x = pmax(group_pp, breed_pp) + 25, label = label_text, color = type),
    size = 3.5, fontface = "bold", hjust = 0
  ) +
  scale_fill_manual(name = "Premium Level", values = pal) +
  scale_color_manual(values = pal, guide = "none") +
  scale_x_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Top Outlier Breeds vs. Parent Group Benchmark Pure Premium",
    subtitle = "Top: Adverse Surcharges | Bottom: Favorable Discounts",
    x        = "Pure Premium ($)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey80"),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 10)
  )

# 24 breeds with high divergence with groups

# Surcharge (Adverse) Breeds: ordered by descending % surcharge magnitude
(surcharge_breeds <- breed_signal |>
  filter(diff > 0) |>
  arrange(desc(abs(pct_diff))) |>
  pull(pet.Breed))

# Discount (Favorable) Breeds: ordered by descending % discount magnitude
(discount_breeds <- breed_signal |>
  filter(diff < 0) |>
  arrange(desc(abs(pct_diff))) |>
  pull(pet.Breed))


### 3.5.4 Interaction Effects ==================================================

# pet.Age shows the highest correlation with pure premium and is the only variable
# that our competitor insurance carrier uses. Even veterinary science claims to
# show that there difference in risk profile of the same aged animals across species
# or sizes. (https://pubmed.ncbi.nlm.nih.gov/23535614/)

# Thus, pet.Age proves to be a strong candidate for checking interactions.

analyze_int_age_by_var <- function(.data, col) {
  col_name <- rlang::as_label(enquo(col))
  
  age_by_col <- .data |>
    mutate(age_band = cut(
      pet.Age,
      breaks = c(0, 5, 9, 13, 17, 30),
      labels = c("0-5", "5-9", "9-13", "13-17", "17+"),
      include.lowest = TRUE
    )) |>
    summarize(
      mean_pp = round(mean(pure_premium, na.rm = TRUE), 2),
      .by     = c(age_band, {{ col }})
    ) |>
    mutate({{ col }} := fct_reorder(as.factor({{ col }}), mean_pp, .fun = max, .desc = TRUE))
  
  wide_tbl <- age_by_col |>
    pivot_wider(names_from = {{ col }}, values_from = mean_pp) |>
    arrange(age_band)
  print(wide_tbl)
  
  p <- ggplot(age_by_col, aes(x = age_band, y = mean_pp, color = {{ col }}, group = {{ col }})) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = glue("Mean Pure Premium: pet.Age Band \u00d7 {col_name}"),
      x     = "Age Band",
      y     = "Mean Pure Premium ($)",
      color = col_name
    ) +
    theme_minimal()
  
  print(p)
  invisible(wide_tbl)
}

#### 3.5.4.a pet.Age x pet.Gender ==============================================

# Weak interaction candidate
train |> analyze_int_age_by_var(pet.Gender)

#### 3.5.4.b pet.Age x pet.Offspring ===========================================

# Weak interaction candidate
train |> analyze_int_age_by_var(pet.Offspring)

#### 3.5.4.c pet.Age x pet.Type ================================================

# Possible interaction exists!
train |> analyze_int_age_by_var(pet.Type)

#### 3.5.4.d pet.Age x pet.Group ===============================================

# This interaction looks like a byproduct of the effect of pet.Type, since
# we are already considering omitting pet.Type in favor of pet.Group as a predictor
# we can try using pet.Type as an interaction
train |>
  mutate(age_band = cut(pet.Age, breaks = c(0, 5, 9, 13, 17, 30),
                        labels = c("0-5", "5-9", "9-13", "13-17", "17+"))) |>
  group_by(age_band, pet.Group) |>
  summarize(
    mean_pp = round(mean(pure_premium), 2),
    # Extract species (each group/breed is exclusively Cat or Dog)
    Species = first(pet.Type),
    .groups = "drop"
  ) |>
  # Order legend by peak height
  mutate(pet.Group := forcats::fct_reorder(as.factor(pet.Group), mean_pp, .fun = max, .desc = TRUE)) |> 
  ggplot(aes(
    x        = age_band,
    y        = mean_pp,
    color    = pet.Group,
    group    = pet.Group,
    linetype = Species
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  # Set Dog = dotted, Cat = solid
  scale_linetype_manual(values = c("Dog" = "dotted", "Cat" = "solid")) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = glue("Mean Pure Premium by pet.Age band and pet.Group"),
    x        = "Age Band",
    y        = "Mean Pure Premium ($)",
    color    = glue("Pet Group (by peak)"),
    linetype = "Species"
  ) +
  theme_minimal()

#### 3.5.4.d pet.Weight x pet.Height ===============================================

hw_diag <- train |>
  mutate(
    h_q = factor(ntile(pet.Height, 4), labels = paste0("H_Q", 1:4)),
    w_q = factor(ntile(pet.Weight, 4), labels = paste0("W_Q", 1:4))
  ) |>
  summarize(pp = round(sum(total_losses) / sum(policy.Exposure), 1), .by = c(h_q, w_q)) |>
  arrange(h_q, w_q)

# Empirical Pure Premium Matrix ($)
hw_diag |> pivot_wider(names_from = w_q, values_from = pp) |> print()

# Interaction Curve Plot (Diverging lines = Interaction)
ggplot(hw_diag, aes(x = w_q, y = pp, color = h_q, group = h_q)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title    = "Empirical Interaction: pet.Height \u00d7 pet.Weight (Quartiles)",
    subtitle = "Non-parallel / crossing slopes prove the interaction exists in raw data",
    x        = "Weight Quartile",
    y        = "Exposure-Weighted Pure Premium ($)",
    color    = "Height Quartile"
  ) +
  theme_minimal()



## 3.5 Feature Engineering =====================================================

apply_feature_engineering <- function(df) {
  
  cat_cols <- c("policy.NBRB", "pet.Type", "pet.Group", "pet.Gender", "pet.Offspring",
    "pet.Microchip", "pet.Origin", "owner.Gender", "owner.Location", "owner.Channel", "breed_outlier")
  
  df |>
    mutate(
      # Added breed_outlier variable 
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
    ) |> 
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
  
}

train <- apply_feature_engineering(train)
test  <- apply_feature_engineering(test)

# ================ 4. GLM Development & Variable Selection =====================

compare_aic_and_return_better_model <- function(formula_1, formula_2, label_1, label_2, data, p) {
  m1 <- glm(as.formula(paste(formula_1, collapse = " ")), 
            data = data, family = statmod::tweedie(var.power = p, link.power = 0))
  m2 <- glm(as.formula(paste(formula_2, collapse = " ")), 
            data = data, family = statmod::tweedie(var.power = p, link.power = 0))
  aic1 <- tweedie_AIC(m1)
  aic2 <- tweedie_AIC(m2)
  print(tibble(
    model     = c(label_1, label_2),
    AIC       = round(c(aic1, aic2), 2),
    preferred = c(ifelse(aic1 < aic2, "YES", ""),
                  ifelse(aic2 < aic1, "YES", ""))
  ))
  if (aic2 < aic1) 
    return(m2)
  else NULL
}
show_tweedie_summary <- function(model, name, p = NULL, baseline = model_A) {
  tweedie_p <- p
  aic_val   <- tweedie_AIC(model)
  dev_exp   <- (1 - model$deviance / model$null.deviance) * 100
  delta_aic <- aic_val - tweedie_AIC(baseline)
  
  cat(glue("
      ===========================================
      {name}
      ===========================================
      Null Deviance:           {round(model$null.deviance, 2)}
      Residual Deviance:       {round(model$deviance, 2)}
      AIC:                     {round(aic_val, 2)}
      Delta AIC (vs Baseline): {delta_aic}
      Degrees of Freedom:      {model$df.residual}
      % Deviance Explained:    {round(dev_exp, 2)}%
      Tweedie p:               {round(tweedie_p, 4)}
      \n"))
}

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
  do.plot = TRUE,
  method  = "series"
)
p_initial <- p_prof_ref$p.max
cat("Initial Tweedie p:", round(p_initial, 4), "\n")

## Model A: Full Baseline GLM ==================================================
# (excluding pet.Breed for high cardinality and pet.Type for exact aliasing with pet.Group)
model_A <- glm(
  total_losses ~ pet.Group + pet.Age + pet.Gender + pet.Height + pet.Weight +
    pet.Offspring + pet.Microchip + pet.Origin + owner.Age + owner.Gender +
    owner.Location + owner.CreditScore + owner.Channel + owner.PetCount + policy.NBRB +
    offset(log(policy.Exposure)),
  data   = train,
  family = statmod::tweedie(var.power = p_initial, link.power = 0)
)

show_tweedie_summary(model_A, "Model A (Baseline)", p_initial)


##  Model B: Backward Variable Selection =======================================
# (using drop1 Chi-square test (p < 0.05))

# Implement Backward selection
backward_selection <- function(base_model = model_A, threshold = 0.05){
  print(glue("{strrep('=',100)}"))
  print(glue("Backward selection: Remove least significant variables until remaining have p-values < {threshold}"))
  print(glue("{strrep('=',100)}"))
  
  model_current <- base_model
  
  # Iteration zero
  n_round <- 0 
  print(d <- drop1(model_current, test = "Chisq"))
  print(td <- tidy(d) |>  select(term, p.value) |> arrange(desc(p.value)) |> mutate(p.value = round(p.value,3)) |> head())
  lowest_p <- td[[1,2]]
  
  while(lowest_p > threshold){
    print(glue("{strrep('=',50)}"))
    print(glue("Round {n_round <- n_round+1 } - remove {td[[1,1]]} (p = {lowest_p})"))
    print(glue("{strrep('=',50)}"))
    model_current <- update(model_current, as.formula(glue(". ~ . - {td[[1,1]]}")))
    print(d <- drop1(model_current, test = "Chisq"))
    print(td <- tidy(d) |>  select(term, p.value) |> arrange(desc(p.value)) |> mutate(p.value = round(p.value,3)) |> head())
    lowest_p <- td[[1,2]]
  }
  
  return(model_current)
}

# Variables removed: owner.Channel, owner.Age, owner.Location, owner.CreditScore, pet.Origin, pet.Microchip, pet.Offspring
model_B <- backward_selection()
show_tweedie_summary(model_B, "Model B (Backward Selection)", p_initial)
(base_formula <- deparse1(formula(model_B)))

## Model B1: Add Binned Predictors =======================================================

compare_aic_and_return_better_model(
  base_formula,
  gsub("owner.PetCount", "pet_count_band", base_formula),
  "Raw: owner.PetCount (Linear)",
  "Binned: pet_count_band (1, 2, 3, 4, 5+)",
  train, p_initial
)
# We prefer a more parsimonious model, and thus don't add binned owner.PetCount
best_formula <- base_formula

## Model C: Add Quadratic Age effect I(pet.Age^2) ==============================

formula_age_sq <- glue("{best_formula} + I(pet.Age^2)")
model_C <- compare_aic_and_return_better_model(
  best_formula,
  formula_age_sq,
  "Linear Age",
  "Linear + Quadratic Age (Age^2)",
  train, p_initial
)
# AIC has improved drastically by adding squared age
best_formula <- formula_age_sq
show_tweedie_summary(model_C, "Model C (Quadratic Age effect)", p_initial)

## Model D: Add pet.Height:pet.Weight interaction ==============================

formula_age_weight <- glue("{best_formula} + pet.Height:pet.Weight")
model_D <- compare_aic_and_return_better_model(
  best_formula,
  formula_age_weight,
  "Quadratic Age (Age^2)",
  "Age x Weight Interaction",
  train, p_initial
)
# AIC has improved drastically by adding squared age
best_formula <- formula_age_weight
show_tweedie_summary(model_D, "Model D (Height x Weight interaction)", p_initial)

## Model E: Add Outlier Breed Rating Tier ======================================

formula_breed_outlier <- glue("{best_formula} + breed_outlier")
model_E <- compare_aic_and_return_better_model(
  best_formula,
  formula_breed_outlier,
  "Height x Weight Interaction",
  "3 Tier Breed surcharge",
  train, p_initial
)
best_formula <- formula_breed_outlier
show_tweedie_summary(model_E, "Model E (Breed Rating)", p_initial)

## Summary of Candidate Models =================================================

candidate_models <- list(
  "Model A (Full Baseline)"               = model_A,
  "Model B (Selected Main Effects)"       = model_B,
  "Model C (+ Quadratic Age)"             = model_C,
  "Model D (+ Morphology Interaction)"    = model_D,
  "Model E (+ Outlier Breed Rating Tier)" = model_E
)

candidate_comparison <- tibble(
  Model = names(candidate_models),
  AIC = sapply(candidate_models, function(m) round(tweedie_AIC(m), 2)),
  Residual_Deviance = sapply(candidate_models, function(m) round(m$deviance, 1)),
  Pct_Dev_Explained = sapply(candidate_models, function(m) paste0(round((1 - m$deviance / m$null.deviance) * 100, 2), "%")),
  Parameters = sapply(candidate_models, function(m) length(coef(m))),
  Delta_AIC = sapply(candidate_models, function(m) round(tweedie_AIC(m) - tweedie_AIC(model_A), 2))
)

candidate_comparison

## Summary of Best Model =======================================================

model_refined <- model_E
summary(model_refined)

# ================= 5. Diagnostics & Iterative Refinement ======================

train_residuals <- train |> 
  mutate(
    fitted_values = predict(model_refined, type = "response"),
    deviance_resids = residuals(model_refined, type = "deviance"),
    pearon_resids = residuals(model_refined, type = "pearson"),
    log_fitted = log(fitted_values),
    decile = ntile(fitted_values, 10)
  )

## 5.1 Deviance Residuals ======================================================

# Skewed to the negative side, this doesn't mean the model overpredicts.
# For a tweedie GLM where 88% of policies have zero claims, deviance for any
# non-claimant is mathematically bounded below zero. The remaining 12% of claimant
# policies exhibit large positive residuals balancing the overall loss deviance.
summary(train_residuals$deviance_resids)

plot_dev_res_vs_var <- function(.data, var, x_lab, y_lab){
  .data |> 
    ggplot(aes(x = {{var}}, y = deviance_resids)) +
    geom_point(alpha = 0.2, color = "steelblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(formula = y ~ x, method = "loess", color = "darkgreen", se = FALSE) +
    #coord_cartesian(ylim = c(-12, 10)) +
    labs(
      title = glue("{y_lab} vs {x_lab}"),
      x = x_lab,
      y = y_lab
    ) +
    theme_minimal()
}

### 5.1.1 Deviance Residuals vs Fitted Values ==================================

train_residuals |> plot_dev_res_vs_var(fitted_values, "Fitted Values", "Deviance Residuals")

### 5.1.2 Deviance Residuals Summary ===========================================
(residual_summary <- train_residuals |> 
  group_by(decile) |> 
  summarize(
    n_policies = n(),
    mean_resid = round(mean(deviance_resids),4),
    median_resid = round(median(deviance_resids),4)
  )
)
residual_summary |> 
  ggplot(aes(x = decile, y = mean_resid)) +
  geom_point(fill = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Mean Deviance Residual by Fitted Value Decile",
    x = "Decile (1 = Lowest Fitted, 10 = Highest Fitted)",
    y = "Mean Deviance Residual"
  ) +
  theme_minimal()

### 5.1.3 Deviance Residuals vs Key Predictors =================================

train_residuals |> plot_dev_res_vs_var(pet.Age, "Pet Age", "Deviance Residuals")
train_residuals |> plot_dev_res_vs_var(pet.Group, "Pet Group", "Deviance Residuals")
train_residuals |> plot_dev_res_vs_var(pet.Gender, "Pet Gender", "Deviance Residuals")
train_residuals |> plot_dev_res_vs_var(pet.Height, "Pet Height (inch)", "Deviance Residuals")
train_residuals |> plot_dev_res_vs_var(pet.Weight, "Pet Weight (pound)", "Deviance Residuals")
train_residuals |> plot_dev_res_vs_var(policy.NBRB, "Policy NBRB", "Deviance Residuals")

# Height appears to have larger deviance residuals at extremes of heights (short and tall)
# compared to medium heights

## 5.2 Q-Q Normality Plot ======================================================

ggplot(train_residuals, aes(sample = deviance_resids)) +
  stat_qq(alpha = 0.3, color = "steelblue") +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(
    title = "Normal Q-Q Plot of Deviance Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

train_residuals |> 
  filter(total_losses > 0) |> 
  ggplot(aes(sample = deviance_resids)) +
  stat_qq(alpha = 0.3, color = "steelblue") +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(
    title = "Normal Q-Q Plot of Deviance Residuals (Claimants Only)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

train_residuals |> 
  arrange(desc(abs(deviance_resids))) |> 
  select(policy.ID, pure_premium, fitted_values, deviance_resids) |> 
  head(20)

# We note a big deviation for specific policies which show large deviances,
# it might be worth looking at influential observations that affect our model.

## 5.3 Influential Observations ================================================

### 5.3.1 Cook's Distance =======================================================

{ # Compute Cook's Distance
  cooks_d <- cooks.distance(model_refined)
  n <- nrow(train_residuals)
  threshold <- 4/n
  train_residuals <- train_residuals |> 
    mutate(cooks_d = cooks_d)
  tibble(
    "Cook's distance threshold (4/n)" = glue(round(threshold,6)),
    "Number of influenctial observations" = sum(cooks_d > threshold)
  )
}
# Very few claims cross the 4/n threshold
train_residuals |> 
  mutate(obs_index = row_number()) |> 
  ggplot(aes(x = obs_index, y = cooks_d)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_hline(yintercept = threshold,  color = "red", linetype = "dashed") +
  labs(
    title = "Cook's distance by observation",
    subtitle = glue("Red line = threshold of 4/n = {round(threshold, 6)}"),
    x = "Observation Index",
    y = "Cook's Distance"
  ) + theme_minimal()

# Top Influential observations
train_residuals |> 
  filter(cooks_d > threshold) |> 
  arrange(desc(cooks_d)) |> 
  select(policy.ID , pure_premium, fitted_values, deviance_resids, cooks_d) |> 
  head(20)

### 5.3.2 Leverage ==============================================================

{ # Compute Leverage
  leverage <- hatvalues(model_refined)
  lev_threshold <- 2 * length(coef(model_refined)) / n
  train_residuals <- train_residuals |> 
    mutate(leverage = leverage)
  tibble(
    "Leverage threshold (2p/n)" = glue(round(lev_threshold,6)),
    "Number of influenctial observations" = sum(leverage > lev_threshold)
  )
}

train_residuals |> 
  mutate(obs_index = row_number()) |> 
  ggplot(aes(x = obs_index, y = leverage)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_hline(yintercept = lev_threshold,  color = "red", linetype = "dashed") +
  labs(
    title = "Leverage by observation",
    subtitle = glue("Red line = threshold of 2p/n = {round(lev_threshold, 6)}"),
    x = "Observation Index",
    y = "Leverage"
  ) + theme_minimal()


# Top Influential observations
train_residuals |> 
  filter(leverage > lev_threshold) |> 
  arrange(desc(leverage)) |> 
  select(policy.ID , pure_premium, fitted_values, leverage,
         pet.Height, pet.Weight) |> 
  head(20)


## 5.4 Segment Level Diagnostics ===============================================

segment_diagnostic <- function(var){
  var_name <- rlang::as_name(ensym(var))
  seg_diag <- train_residuals |> 
    group_by({{var}}) |> 
    summarize(
      n_policies = n(),
      mean_resid = round(mean(deviance_resids), 4),
      mean_pp = round(mean(pure_premium), 4),
      mean_fitted = round(mean(fitted_values / policy.Exposure), 4)
    ) |> 
    arrange(desc(abs(mean_resid)))
  return(seg_diag)
}

segment_diagnostic(pet.Group)
segment_diagnostic(pet.Gender)
segment_diagnostic(policy.NBRB)
segment_diagnostic(breed_outlier)

## 5.5 Iterative Refinement ====================================================

# We go back to (3.5.4.e) and found out that pet.Height:pet.Weight 
# is an interaction worth noting

# Likelihood ratio Chi-square test for pet.Height:pet.Weight
anova(
  glm(total_losses ~ pet.Height + pet.Weight + offset(log(policy.Exposure)), data = train, family = statmod::tweedie(var.power = p_initial, link.power = 0)),
  glm(total_losses ~ pet.Height * pet.Weight + offset(log(policy.Exposure)), data = train, family = statmod::tweedie(var.power = p_initial, link.power = 0)),
  test = "Chisq"
)
# Test suggests that pet.Height:pet.Weight is significant, and we should add it to
# the model.


### 5.5.1 Add Quadratic Height ===============================================
# I(pet.Height^2) to capture non-linear stature/volume scaling

formula_height_sq <- glue("{best_formula} + I(pet.Height^2)")
model_F <- compare_aic_and_return_better_model(
  best_formula,
  formula_height_sq,
  "breed_",
  "Linear + Quadratic Age (Age^2)",
  train, p_initial
)
# AIC has improved drastically by adding squared age
best_formula <- formula_height_sq
show_tweedie_summary(model_F, "Model C (Quadratic Age effect)", p_initial)

model_final <- model_F

### 5.5.2 Re-profile Tweedie p on the refined model formula ====================
p_recheck <- tweedie.profile(
  formula(model_final),
  p.vec   = seq(1.2, 1.6, by = 0.05),
  data    = train,
  do.plot = FALSE,
  method  = "series"
)

p_refined <- tweedie.profile(
  formula(model_F),
  p.vec   = seq(p_recheck$p.max - 0.05, p_recheck$p.max + 0.05, by = 0.01),
  data    = train,
  do.plot = TRUE,
  method  = "series"
)
p_final <- p_refined$p.max
cat(glue("Re-profiled Final Tweedie p: {round(p_final, 4)}"))

# Fit Final Model with re-profiled p = 1.3173
model_final <- glm(
  best_formula,
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

## 6.1 Double Lift Analysis =====================================================
# across 10 deciles sorted by predicted-to-benchmark pure premium

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
print_header("Holdout Decile Calibration & Double Lift")
print(as.data.frame(double_lift))

# Double Lift chart
double_lift |> 
  pivot_longer(
    cols = c(mean_actual_pp, mean_predicted_pp),
    names_to = "type",
    values_to = "pure_premium"
  ) |> 
  mutate(
    type = case_when(
      type == "mean_actual_pp" ~ "Actual",
      type == "mean_predicted_pp" ~ "Predicted"
    )
  ) |> 
  ggplot(aes(x = decile, y = pure_premium, color = type, group = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = c("Actual" = "steelblue", "Predicted" = "darkgreen")) +
  labs(
    title = "Double Lift Curve",
    subtitle = "Policies sorted by predicted-to-benchmark ratio and grouped into deciles",
    x = "Decile (1 = Lowest Predicted Risk, 10 = Highest Predicted Risk)",
    y = "Mean Pure Premium ($)",
    color = NULL
  ) + theme_minimal()

## 6.2 Gini Coefficient & Lorenz Curves ========================================

{# Gini Coefficient Calculation
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
cat("\n",glue("Holdout Gini Coefficient: {round(gini_coef,4)}"))
}

# Since, gini > 0.4, it's a quite strong relationship.
# This model has a strong discrimination power.

gini_df |> 
  ggplot(aes(x = cum_exp, y = cum_loss)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Lorenz Curve - Cumualtive Loss Share vs Cumulative Exposure Share",
    x = "Cumulative Exopsure Share",
    y = "Cumulative Loss Share"
  ) +
  theme_minimal()



# ===================== 7. Scoring Assessment Data Set =========================
#  (assessment-data.csv -> scored.csv) 

# Load assessment dataset (unseen production data)
assess_raw <- read.csv("data/assessment-data.csv")

# Clean and format assessment data matching training pipeline
assess_clean <- assess_raw |>
  mutate(
    policy.NBRB = factor(recode_values(policy.NBRB, "NewBus" ~ "NB", "RenBus" ~ "RB", .default = policy.NBRB)),
    pet.Type    = factor(pet.Type),
    pet.Group   = factor(recode_values(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group)),
    pet.Gender  = factor(recode_values(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender)),
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
cat(glue("Scored {nrow(scored_output)} records. File saved to scored.csv","\n"))
