# ==== SETUP (set to automatically install packages if unavailable) ====
if (!requireNamespace("pacman", quietly = TRUE)) { install.packages("pacman") }
library(pacman)
pacman::p_load(tidyverse, lubridate, ggplot2, dplyr, rlang, janitor, skimr, naniar, glue)


# ==== INPUTS (appendices and raw data) ====

appendix_breed_cats <- "Abyssinian, American Bobtail, American Curl, American Shorthair, American Wirehair, Asian, Asian Semi Longhair, Australian Mist, Balinese, Bambino, Bengal, Birman, Bombay, Brazilian Shorthair, British Shorthair, Burmese, Chantilly Tiffany, Chartreux, Chausie, Cornish Rex, Cymric, Cyprus, Devon Rex, Donskoy, Egyptian Mau, European Burmese, European Shorthair, Exotic Shorthair, German Rex, Havana, Havana Brown, Highlander, Himalayan, Japanese Bobtail, Javanese, Korat, Kurilian Bobtail, LaPerm, Lykoi, Maine Coon, Manx, Minskin, Munchkin, Napoleon, Nebelung, Ocicat, Ojos Azules, Oriental, Oriental Bicolor, Oriental Longhair, Persian, Peterbald, Pixiebob, Ragamuffin, Ragdoll, Russian Blue, Scottish Fold, Selkirk Rex, Seychellois, Siamese, Siberian, Siberian Forest, Singapura, Snowshoe, Sokoke, Somali, Sphynx, Thai, Tiffany, Tonkinese, Toybob, Toyger, Turkish Angora, Turkish Van, Ukrainian Levkoy, Ussuri, York Chocolate"
appendix_breed_dogs <- " Afghan Hound, Airedale Terrier, Akita, Alaskan Malamute, American Eskimo Dog, American Foxhound, American Staffordshire Terrier, Anatolian Shepherd Dog, Australian Cattle Dog, Australian Shepherd, Australian Terrier, Basenji, Basset Hound, Beagle, Bearded Collie, Beauceron, Bedlington Terrier, Belgian Malinois, Belgian Sheepdog, Belgian Tervuren, Bernese Mountain Dog, Bichon Frise, Black and Tan Coonhound, Bloodhound, Border Collie, Border Terrier, Borzoi, Boston Terrier, Boxer, Boykin Spaniel, Briard, Brittany, Brussels Griffon, Bull Terrier, Bulldog, Cairn Terrier, Canaan Dog, Cardigan Welsh Corgi, Cavalier King Charles Spaniel, Chesapeake Bay Retriever, Chihuahua, Chinese Crested, Chow Chow, Clumber Spaniel, Cocker Spaniel, Collie, Coton de Tulear, Dachshund, Dalmatian, Dandie Dinmont Terrier, Doberman Pinscher, Dogo Argentino, English Cocker Spaniel, English Foxhound, English Setter, English Springer Spaniel, English Toy Spaniel, Entlebucher Mountain Dog, Field Spaniel, Finnish Lapphund, Finnish Spitz, French Bulldog, German Pinscher, German Shepherd Dog, German Shorthaired Pointer, German Wirehaired Pointer, Giant Schnauzer, Glen of Imaal Terrier, Golden Retriever, Gordon Setter, Great Dane, Great Pyrenees, Greater Swiss Mountain Dog, Greyhound, Harrier, Havanese, Irish Red and White Setter, Irish Setter, Irish Terrier, Irish Water Spaniel, Irish Wolfhound, Italian Greyhound, Japanese Chin, Keeshond, Kerry Blue Terrier, Komondor, Kuvasz, Labrador Retriever, Lagotto Romagnolo, Lakeland Terrier, Leonberger, Lhasa Rapso, Maltese, Manchester Terrier, Mastiff, Miniature Bull Terrier, Miniature Pinscher, Miniature Schnauzer, Newfoundland, Norfolk Terrier, Norwegian Buhund, Norwegian Elkhound, Norwegian Lundehund, Norwich Terrier, Nova Scotia Duck Tolling Retriever, Old English Sheepdog, Otterhound, Parson Russell Terrier, Pekingese, Pembroke Welsh Corgi, Pharaoh Hound, Pointer, Polish Lowland Sheepdog, Pomeranian, Poodle, Portuguese Water Dog, Pug, Puli, Pumi, Pyrenean Shepherd, Redbone Coonhound, Rhodesian Ridgeback, Rottweiler, Saint Bernard, Saluki, Samoyed, Schipperke, Scottish Deerhound, Scottish Terrier, Sealyham Terrier, Shetland Sheepdog, Shiba Inu, Shih Tzu, Siberian Husky, Silky Terrier, Skye Terrier, Sloughi, Soft Coated Wheaten Terrier, Spanish Water Dog, Standard Schnauzer, Sussex Spaniel, Swedish Vallhund, Tibetan Mastiff, Tibetan Spaniel, Tibetan Terrier, Toy Fox Terrier, Treeing Walker Coonhound, Vizsla, Weimaraner, Welsh Springer Spaniel, Welsh Terrier, West Highland White Terrier, Whippet, Wire Fox Terrier, Wirehaired Pointing Griffon, Wirehaired Vizsla, Xoloitzcuintli, Yorkshire Terrier. "

raw_pet_data <- read.csv("data/pet-data.csv")
raw_owner_data <- read.csv("data/owner-data.csv")
raw_policy_data <- read.csv("data/policy-data.csv")
raw_claims_data <- read.csv("data/claims-data.csv")


# ==== UTILITY FUNCTIONS (repetitive for all data sets) ====

# Prints a header for better readability
print_header <- function(heading, total_width = 70) {
  # Account for the 2 spaces added around heading
  n_char <- nchar(heading) + 2
  
  # Calculate padding per side, ensuring it's at least 1
  pad_len <- max(1, floor((total_width - n_char) / 2))
  
  cat(
    "\n\n\n",
    strrep("=", pad_len),
    glue(" {heading} "),
    strrep("=", pad_len),
    "\n\n",
    sep = ""
  )
}

# Shows a comprehensive data overview
show_data_overview <- function(df, name) {
  print_header(glue("{name}: A glimpse at the data"))
  glimpse(df)
  print_header(glue("{name}: Top 6 rows"))
  print(head(df))
  print_header(glue("{name}: Skim"))
  print(skim(df))
  print_header(glue("{name}: % of missing values"))
  print(
    df |>
      summarize(across(everything(), \(x) sum(is.na(
        x
      )))) |>
      pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
      mutate(pct_missing = round(n_missing / nrow(df) * 100, 2)) |>
      filter(n_missing > 0) |>
      arrange(desc(n_missing))
  )
  print_header(glue("{name}: Missing Completely at Random Test"))
  print(mcar_test(df))
}

# Profile categorical variables
profile_categorical_variables <- function(df, factor_cols, name) {
  print_header(glue("{name}: Profiling Categorical Variables"))
  print(factor_cols |>
          rlang::set_names() |>
          map(\(col) tabyl(df, all_of(col))))
}

# Checking for rows with duplicate IDs
check_for_duplicates <- function(df, df.ID, name) {
  id_name <- as_label(enquo(df.ID))
  
  df_dupes <- df |> get_dupes({{df.ID}})
  
  print_header(glue("{name}: Duplicate Data Summary"))
  print(summary(duplicates <- duplicated(df[id_name])))
  
  print(glue(
    "There are {sum(duplicates)} duplicate {id_name} in {name} dataset."
  ))
  
  print_header(glue("{name}: Rows with conflicting {id_name}"))
  print(
    conflicting_rows <- df |>
      group_by({{df.ID}}) |>
      filter(n_distinct(pick(everything(
        
      ))) > 1) |>
      ungroup() |>
      arrange({{df.ID}})
    ,
    n = Inf
  )
}

# Outlier analysis across multiple numeric columns (summary table, box-plots, histograms)
check_for_outliers <- function(df, cols, name) {
  print_header(glue("{name}: Outlier Analysis"))

  # 1. Summary table based on IQR method
  summary_tbl <- cols |>
    map_dfr(function(col) {
      vals <- df[[col]][!is.na(df[[col]])]
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      iqr_val <- q3 - q1
      cutoff_lower <- q1 - 1.5 * iqr_val
      cutoff_upper <- q3 + 1.5 * iqr_val
      n_outliers <- sum(vals < cutoff_lower | vals > cutoff_upper)

      tibble(
        Variable = col,
        Q1 = round(q1, 2),
        Q3 = round(q3, 2),
        IQR = round(iqr_val, 2),
        Lower_Cutoff = round(cutoff_lower, 2),
        Upper_Cutoff = round(cutoff_upper, 2),
        N_Outliers = n_outliers,
        Total = length(vals),
        Pct_Outliers = paste0(round(n_outliers / length(vals) * 100, 2), "%")
      )
    })
  print(as.data.frame(summary_tbl), row.names = FALSE)

  # Reshape data for facet grids
  df_long <- df |>
    select(all_of(cols)) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    filter(!is.na(value))

  # 2. Boxplots grid
  p_box <- df_long |>
    ggplot(aes(x = "", y = value)) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    facet_wrap(~ variable, scales = "free") +
    labs(title = glue("{name}: Boxplots"), x = NULL, y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  # 3. Histograms grid
  p_hist <- df_long |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ variable, scales = "free") +
    labs(title = glue("{name}: Histograms"), x = "Value", y = "Count") +
    theme_minimal()

  # Display plots
  print(p_box)
  print(p_hist)
}

# Profile claim frequency distribution across policies
analyze_claims_distribution <- function(claims_df, name = "Claims") {
  print_header(glue("{name}: Distribution of Claims per Policy"))
  dist_tbl <- claims_df |>
    count(policy.ID, name = "n_claims") |>
    count(n_claims, name = "n_policies") |>
    mutate(pct = round(n_policies / sum(n_policies) * 100, 2))
  print(as.data.frame(dist_tbl), row.names = FALSE)
  return(invisible(dist_tbl))
}

make_observations <- function(dataset_name, ...) {
  print_header(glue("Observations for `{dataset_name}` Dataset"))
  
  obs <- list(...)
  env <- parent.frame()
  
  for (i in seq_along(obs)) {
    text <- glue(obs[[i]], .envir = env)
    cat(sprintf("%d. %s\n", i, text))
  }
}

# ==== PETS DATASET ====

show_data_overview(raw_pet_data, "Pets")

profile_categorical_variables(
  raw_pet_data,
  c(
    "pet.Type",
    "pet.Group",
    "pet.Gender",
    "pet.Offspring",
    "pet.Microchip",
    "pet.Origin"
  ),
  "Pets"
)

check_for_duplicates(raw_pet_data, pet.ID, "Pets")

check_for_outliers(raw_pet_data, c("pet.Age", "pet.Height", "pet.Weight"), "Pets")

# Validate the breed names
check_breeds <- function(breed = "Dog",
                         appendix = appendix_breed_dogs ,
                         df = raw_pet_data) {
  # Dog breeds expected vs actual
  breeds_expected <-  appendix |>
    str_replace_all("[\r\n.]+", " ") |> # replace \n with space
    str_remove("\\.+$") |> # remove trailing dot
    str_squish() |>
    str_split_1("\\s*,\\s*")
  
  print_header(glue("Check discrepencies in `{breed}` breed"))
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


make_observations(
  "Pets",
  "pet.Age (1-18), pet.Height (4.9-32.9), and pet.Weight (3.1-137.9) fall into expected biological ranges.",
  "`p` in the MCAR test is significantly larger than 5%, confirming missing values are missing completely at random.",
  "Gender is coded with both `M` and `Male`, `F` and `Female`, requiring normalization to factors.",
  "Microchip is coded with both `No`, `Nope`, `Y`, `Yes`, requiring standardization.",
  "In pet.Group: `Non-Sporting` has two spellings ('Non-Sporting' and 'Non-sporting'), requiring consolidation.",
  "The {toString(extra_dog_breeds)} are dog breeds present in the data but missing from the appendix list; retained as valid population.",
  "There are 369 rows with duplicate pet.ID in Pets, out of which 32 have conflicting values mostly due to NA entries.",
  "In outlier analysis (1.5 * IQR), `pet.Age` and `pet.Height` exhibit 0 outliers, whereas `pet.Weight` exhibits 1,808 outliers (4.85% of total records) with values exceeding the upper cutoff of 106.6 lbs, driven by large/giant dog breeds."
)


# ==== CLEANING `PETS` DATASET ==== 

pets <- raw_pet_data |>
  mutate(
    pet.Type = as.factor(pet.Type),
    pet.Group = as.factor(
      case_match(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group)
    ),
    pet.Breed = as.factor(pet.Breed),
    pet.Gender = as.factor(
      case_match(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender)
    ),
    pet.Offspring = as.factor(pet.Offspring),
    pet.Microchip = as.factor(
      case_match(pet.Microchip, "Nope" ~ "No", "Y" ~ "Yes", .default = pet.Microchip)
    ),
    pet.Origin = as.factor(pet.Origin)
  )

# Verify extra dog breeds distribution and missing values
pets |>
  filter(pet.Breed %in% extra_dog_breeds) |>
  filter(if_any(everything(), is.na))

# These breeds are not outliers, they represent a valid population of their breeds
# in the data, and the data vendor likely omitted them in the appendices. We will retain them.

# Dedupe Pets: remove exact duplicates and resolve conflicting records by taking non-null values
cat(glue("Pet records before dedup: {nrow(pets)}\n\n"))

pets <- pets |>
  distinct() |>
  group_by(pet.ID) |>
  summarize(across(everything(), \(x) {
    v <- na.omit(x)
    if (length(v) > 0) v[1] else x[1]
  }), .groups = "drop")

cat(glue("Pet records after dedup: {nrow(pets)} (unique pets: {n_distinct(pets$pet.ID)})\n\n"))


# ==== OWNERS DATASET ==== 

show_data_overview(raw_owner_data, "Owners")

profile_categorical_variables(
  raw_owner_data,
  c("owner.Gender", "owner.Location", "owner.Channel"),
  "Owners"
)

check_for_duplicates(raw_owner_data, owner.ID, "Owners")

check_for_outliers(
  raw_owner_data,
  c("owner.Age", "owner.CreditScore", "owner.PetCount"),
  "Owners"
)

# Validate recorded owner.PetCount against raw pet data
validate_pet_counts(raw_owner_data, pets, "Owners (Raw)")

make_observations(
  "Owners",
  "owner.Age (22-78), owner.CreditScore (300-850), and owner.PetCount (1-4) fall into valid ranges.",
  "`p` in the MCAR test is well above 5%, confirming missing values are Missing Completely at Random.",
  "Categorical variables (Gender, Location, Channel) are clean with no miscoding or invalid levels.",
  "There are 219 duplicate owner.ID records in the raw data, with 4 unique owners exhibiting conflicting records.",
  "Discrepancy between recorded owner.PetCount and calculated pet counts is due to duplicate records; But we will keep ."
)

# ==== CLEANING `OWNERS` DATASET ==== 

# Dedupe Owners: take max pet.Count and non-NA values for other columns
owners <- raw_owner_data |>
  group_by(owner.ID) |>
  summarize(
    owner.PetCount = max(owner.PetCount, na.rm = TRUE),
    across(-owner.PetCount, ~ na.omit(.x)[1]),
    .groups = "drop"
  ) |>
  mutate(
    owner.Gender = as.factor(owner.Gender),
    owner.Location = as.factor(owner.Location),
    owner.Channel = as.factor(owner.Channel)
  )

stopifnot(n_distinct(owners$owner.ID) == nrow(owners))
cat(glue("Owner records after dedup: {nrow(owners)} (unique owners: {n_distinct(owners$owner.ID)})\n\n"))

# Re-validate pet count against cleaned pets
validate_pet_counts(owners, pets, "Owners (Cleaned)")


# ==== POLICY DATASET ==== 

show_data_overview(raw_policy_data, "Policy")

profile_categorical_variables(
  raw_policy_data,
  c("policy.NBRB"),
  "Policy"
)

check_for_duplicates(raw_policy_data, policy.ID, "Policy")

check_for_outliers(
  raw_policy_data,
  c("policy.Exposure"),
  "Policy"
)

make_observations(
  "Policy",
  "policy.Start and policy.End are character strings and require formatting to Date objects.",
  "Exposure is in (0, 1] range; records with zero exposure do not contribute to risk and must be excluded.",
  "`p` in the MCAR test is significantly above 5%, confirming missing values are Missing Completely at Random.",
  "policy.NBRB has miscoded categories ('NewBus' -> 'NB', 'RenBus' -> 'RB') requiring standardization.",
  "There are 0 duplicate policy.ID records found in the policy dataset."
)

# ==== CLEANING `POLICY` DATASET ==== 

policy <- raw_policy_data |>
  filter(policy.Exposure > 0) |>
  mutate(
    policy.Start = as.Date(policy.Start),
    policy.End = as.Date(policy.End),
    policy.NBRB = as.factor(case_match(
      policy.NBRB,
      "NewBus" ~ "NB",
      "RenBus" ~ "RB",
      .default = policy.NBRB
    ))
  )

cat(glue("Policy records after filtering zero exposure: {nrow(policy)} (unique policies: {n_distinct(policy$policy.ID)})\n\n"))


# ==== CLAIMS DATASET ==== 

show_data_overview(raw_claims_data, "Claims")

profile_categorical_variables(
  raw_claims_data,
  c("claim.Coverage"),
  "Claims"
)

check_for_duplicates(raw_claims_data, claim.ID, "Claims")

check_for_outliers(
  raw_claims_data,
  c("claim.Cost"),
  "Claims"
)

analyze_claims_distribution(raw_claims_data, "Claims")

make_observations(
  "Claims",
  "Zero missing values exist across all columns in the claims dataset.",
  "claim.Coverage contains two clean, consistent categories: 'Illness' and 'Accident'.",
  "84.8% of policies with claims have 1 claim, 12.8% have 2 claims, and remaining few have 3-5 claims.",
  "claim.Cost exhibits a right-skewed distribution with severe high-cost outliers typical of insurance loss data.",
  "There are zero duplicate claim.ID records."
)

# ==== CLEANING `CLAIMS` DATASET ==== 

claims <- raw_claims_data |>
  mutate(
    claim.Coverage = as.factor(claim.Coverage)
  )


# ==== MERGING DATASETS ==== 

print_header("Merging Datasets: Policy + Pets + Owners + Claims")

# 1. Aggregate claims by policy
# Retain granular Illness and Accident losses while calculating TotalLoss and total claim count
claims_agg <- claims |>
  group_by(policy.ID) |>
  summarize(
    n_claims = n(),
    illness_loss = sum(claim.Cost[claim.Coverage == "Illness"]),
    accident_loss = sum(claim.Cost[claim.Coverage == "Accident"]),
    total_losses = sum(claim.Cost),
    .groups = "drop"
  )

cat(glue("Aggregated claims: {nrow(claims_agg)} unique policies with claims.\n\n"))

# 2. Join Policy to Pets on pet.ID
merged_df <- policy |>
  left_join(pets, by = "pet.ID")

unmatched_pets <- sum(is.na(merged_df$pet.Type))
cat(glue("Unmatched pets after joining Policy and Pets: {unmatched_pets}\n"))

# 3. Join with Owners on owner.ID
merged_df <- merged_df |>
  left_join(owners, by = "owner.ID")

unmatched_owners <- sum(is.na(merged_df$owner.Location))
cat(glue("Unmatched owners after joining with Owners: {unmatched_owners}\n"))

# 4. Join with Claims Aggregation on policy.ID
# Impute non-claiming policies with 0 claims and 0 losses
merged_df <- merged_df |>
  left_join(claims_agg, by = "policy.ID") |>
  mutate(
    n_claims = replace_na(n_claims, 0),
    illness_loss = replace_na(illness_loss, 0),
    accident_loss = replace_na(accident_loss, 0),
    total_losses = replace_na(total_losses, 0)
  )

cat(glue("\nMerge completed successfully!\n"))
cat(glue("Final merged dimensions: {nrow(merged_df)} rows x {ncol(merged_df)} columns.\n\n"))

# --- Post-Merge Validation & Overview ---
show_data_overview(merged_df, "Merged Dataset")

make_observations(
  "Merged Dataset",
  "Policy dataset (exposure > 0) serves as the base population with {nrow(policy)} records.",
  "100% of policies successfully matched to a pet in `pets` (0 unmatched pet.IDs).",
  "100% of pet records successfully matched to an owner in `owners` (0 unmatched owner.IDs).",
  "Claims aggregated at policy level merged cleanly; non-claiming policies imputed with 0 claims and 0 losses.",
  "Final modeling dataset contains {nrow(merged_df)} records across {ncol(merged_df)} features ready for pure premium modeling."
)
