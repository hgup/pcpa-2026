# %% ===========================================================================
# 0. Setup
# ==============================================================================

# Load Libraries
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)
pacman::p_load(
  tidyverse, lubridate, ggplot2, dplyr, rlang,
  janitor, skimr, naniar, glue
)

# Import raw data sources
raw_pet_data    <- read.csv("data/pet-data.csv")
raw_owner_data  <- read.csv("data/owner-data.csv")
raw_policy_data <- read.csv("data/policy-data.csv")
raw_claims_data <- read.csv("data/claims-data.csv")

# nolint start
appendix_breed_cats <- "
Abyssinian, American Bobtail, American Curl, American Shorthair, American Wirehair, Asian, Asian Semi
Longhair, Australian Mist, Balinese, Bambino, Bengal, Birman, Bombay, Brazilian Shorthair, British
Shorthair, Burmese, Chantilly Tiffany, Chartreux, Chausie, Cornish Rex, Cymric, Cyprus, Devon Rex,
Donskoy, Egyptian Mau, European Burmese, European Shorthair, Exotic Shorthair, German Rex, Havana,
Havana Brown, Highlander, Himalayan, Japanese Bobtail, Javanese, Korat, Kurilian Bobtail, LaPerm, Lykoi,
Maine Coon, Manx, Minskin, Munchkin, Napoleon, Nebelung, Ocicat, Ojos Azules, Oriental, Oriental
Bicolor, Oriental Longhair, Persian, Peterbald, Pixiebob, Ragamuffin, Ragdoll, Russian Blue, Scottish Fold,
Selkirk Rex, Seychellois, Siamese, Siberian, Siberian Forest, Singapura, Snowshoe, Sokoke, Somali,
Sphynx, Thai, Tiffany, Tonkinese, Toybob, Toyger, Turkish Angora, Turkish Van, Ukrainian Levkoy, Ussuri,
York Chocolate"

appendix_breed_dogs <- "
Afghan Hound, Airedale Terrier, Akita, Alaskan Malamute, American Eskimo Dog, American Foxhound,
American Staffordshire Terrier, Anatolian Shepherd Dog, Australian Cattle Dog, Australian Shepherd,
Australian Terrier, Basenji, Basset Hound, Beagle, Bearded Collie, Beauceron, Bedlington Terrier, Belgian
Malinois, Belgian Sheepdog, Belgian Tervuren, Bernese Mountain Dog, Bichon Frise, Black and Tan
Coonhound, Bloodhound, Border Collie, Border Terrier, Borzoi, Boston Terrier, Boxer, Boykin Spaniel,
Briard, Brittany, Brussels Griffon, Bull Terrier, Bulldog, Cairn Terrier, Canaan Dog, Cardigan Welsh Corgi,
Cavalier King Charles Spaniel, Chesapeake Bay Retriever, Chihuahua, Chinese Crested, Chow Chow,
Clumber Spaniel, Cocker Spaniel, Collie, Coton de Tulear, Dachshund, Dalmatian, Dandie Dinmont Terrier,
Doberman Pinscher, Dogo Argentino, English Cocker Spaniel, English Foxhound, English Setter, English
Springer Spaniel, English Toy Spaniel, Entlebucher Mountain Dog, Field Spaniel, Finnish Lapphund,
Finnish Spitz, French Bulldog, German Pinscher, German Shepherd Dog, German Shorthaired Pointer,
German Wirehaired Pointer, Giant Schnauzer, Glen of Imaal Terrier, Golden Retriever, Gordon Setter,
Great Dane, Great Pyrenees, Greater Swiss Mountain Dog, Greyhound, Harrier, Havanese, Irish Red and
White Setter, Irish Setter, Irish Terrier, Irish Water Spaniel, Irish Wolfhound, Italian Greyhound, Japanese
Chin, Keeshond, Kerry Blue Terrier, Komondor, Kuvasz, Labrador Retriever, Lagotto Romagnolo, Lakeland
Terrier, Leonberger, Lhasa Apso, Maltese, Manchester Terrier, Mastiff, Miniature Bull Terrier, Miniature
Pinscher, Miniature Schnauzer, Newfoundland, Norfolk Terrier, Norwegian Buhund, Norwegian Elkhound,
Norwegian Lundehund, Norwich Terrier, Nova Scotia Duck Tolling Retriever, Old English Sheepdog,
Otterhound, Parson Russell Terrier, Pekingese, Pembroke Welsh Corgi, Pharaoh Hound, Pointer, Polish
Lowland Sheepdog, Pomeranian, Poodle, Portuguese Water Dog, Pug, Puli, Pumi, Pyrenean Shepherd,
Redbone Coonhound, Rhodesian Ridgeback, Rottweiler, Saint Bernard, Saluki, Samoyed, Schipperke,
Scottish Deerhound, Scottish Terrier, Sealyham Terrier, Shetland Sheepdog, Shiba Inu, Shih Tzu, Siberian
Husky, Silky Terrier, Skye Terrier, Sloughi, Soft Coated Wheaten Terrier, Spanish Water Dog, Standard
Schnauzer, Sussex Spaniel, Swedish Vallhund, Tibetan Mastiff, Tibetan Spaniel, Tibetan Terrier, Toy Fox
Terrier, Treeing Walker Coonhound, Vizsla, Weimaraner, Welsh Springer Spaniel, Welsh Terrier, West
Highland White Terrier, Whippet, Wire Fox Terrier, Wirehaired Pointing Griffon, Wirehaired Vizsla,
Xoloitzcuintli, Yorkshire Terrier.
" # nolint end


# %% ===========================================================================
# 1. EDA
# ==============================================================================
# %%

# ------------------------------------------------------------------------------
# 1.1 Pet Data
# ------------------------------------------------------------------------------
df <- raw_pet_data

glimpse(df)
head(df)
print(skim(df))
cat("PET: Age, Height, & Weight seem to be consistent range\n")

# Profile missing values
df |>
  summarize(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  mutate(pct_missing = round(n_missing / nrow(df) * 100, 2)) |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

mcar_test(df)
cat("Since p is considerably larger than 5%, we fail to reject\n",
    "the null hypothesis, and conclude that data is MCAR.\n\n",
    "Missing data in any of the columns, isn't significant and MCAR.\n")

# Categorical variables
factor_cols <- c(
  "pet.Type", "pet.Group", "pet.Gender",
  "pet.Offspring", "pet.Microchip", "pet.Origin"
) # left out pet.Breed as it had too many columns
factor_cols |>
  rlang::set_names() |>
  map(\(col) tabyl(df, all_of(col)))

cat("Gender is coded with both `M` and `Male` format, needs cleaning\n")
cat("Microchip is coded with both `No`, `Nope`, `Y`, `Yes`, needs cleaning\n")
cat("In pet.Group: `Non-Sporting` has two spellings, needs cleaning\n")

# Dog breeds expected vs actual
dog_breeds_expected <- appendix_breed_dogs |>
  str_replace_all("[\r\n.]+", " ") |> # replace \n with space
  str_remove("\\.+$") |> # remove trailing dot
  str_squish() |>
  str_split_1("\\s*,\\s*")

dog_breeds_actual <- as.list(
  df |>
    filter(pet.Type == "Dog" & !is.na(pet.Breed)) |>
    select(pet.Breed) |>
    unnest(pet.Breed) |>
    unique() |>
    pull()
)

print("Dog breeds present in the data, but missing the list:")
(extra_dog_breeds <- setdiff(dog_breeds_actual, dog_breeds_expected))

print("These breeds are not in the Appendix, but present in the dataset.")
df |>
  filter(pet.Breed %in% extra_dog_breeds) |>
  count(pet.Breed)

# Cat breeds expected vs actual
cat_breeds_expected <- appendix_breed_cats |>
  str_replace_all("[\r\n.]+", " ") |> # replace \n with space
  str_remove("\\.+$") |> # remove trailing dot
  str_squish() |>
  str_split_1("\\s*,\\s*")

cat_breeds_actual <- as.list(
  df |>
    filter(pet.Type == "Cat" & !is.na(pet.Breed)) |>
    select(pet.Breed) |>
    unnest(pet.Breed) |>
    unique() |>
    pull()
)

a <- setdiff(cat_breeds_expected, cat_breeds_actual)
print("Cat breeds present in the data, but missing the list:")
length(a) # 0 => all present!

# Check for duplicates

(duplicates <- raw_pet_data |> 
  add_count(pet.ID) |> 
  filter(n > 1) |> 
  arrange(pet.ID))

duplicate_counts <- duplicates |>
  summarize(duplicate_count = n_distinct(pet.ID))

cat(glue("There are {duplicate_counts} duplicate pet records.
Let's check how many of them have conflicting results:\n"))

(conflicting_pets <- raw_pet_data |> 
  group_by(pet.ID) |> 
  filter(n_distinct(pick(everything())) > 1) |> 
  ungroup() |> 
  arrange(pet.ID))

cat("We will take the non-null values for the conflicting cases, when cleaning the data.\n")


# ------------------------------------------------------------------------------
# 1.2 Owner Data
# ------------------------------------------------------------------------------
df <- raw_owner_data

glimpse(df)
head(df)
print(skim(df))
cat("Age, creditScore, & PetCount seem to be consistent range\n")

# Profile missing values
df |>
  summarize(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  mutate(pct_missing = round(n_missing / nrow(df) * 100, 2)) |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

mcar_test(df)
cat("Since p is considerably larger than 5%, we fail to reject\n",
    "the null hypothesis, and conclude that data is MCAR.\n\n",
    "Missing data in any of the columns, isn't significant and MCAR.\n")

# Categorical variables
df |>
  select(where(\(x) !is.numeric(x))) |>
  names()

factor_cols <- c(
  "owner.Gender", "owner.Location", "owner.Channel"
)
factor_cols |>
  rlang::set_names() |>
  map(\(col) tabyl(df, all_of(col)))

# Validate Pet count
cat("Owner data has a pet.Count column. Let's validate whether it is actually consistent with the pet data.\n")

(raw_owner_data |> 
  left_join(raw_pet_data, by = "owner.ID", relationship = "many-to-many") |> 
  group_by(owner.ID) |> 
  summarize(
    owner.PetCount = last(owner.PetCount),
    pet_count_calc = n_distinct(pet.ID, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  summarise(
    total_owners     = n(),
    matched_count    = sum(owner.PetCount == pet_count_calc, na.rm = TRUE),
    mismatched_count = sum(owner.PetCount != pet_count_calc | is.na(owner.PetCount) != is.na(pet_count_calc))
  ))

cat("This is odd, the pet count doesn't match. This might be due to presence of duplicates in the data.\n")

# Check for duplicates

(duplicates = raw_owner_data |> 
  add_count(owner.ID) |> 
  filter(n > 1) |> 
  arrange(owner.ID))
  
duplicate_counts <- duplicates |>
  summarize(duplicate_count = n_distinct(owner.ID))

cat(glue("Our suspicion was correct. There are {duplicate_counts} duplicate records
Let's check how many of them have conflicting results:\n"))

(conflicting_owners <- raw_owner_data |> 
  group_by(owner.ID) |> 
  filter(n_distinct(pick(everything())) > 1) |> 
  ungroup() |> 
  arrange(owner.ID))

cat("We will take the non-null values for the conflicting cases, when cleaning the data.\n")


# ------------------------------------------------------------------------------
# 1.3 Policy Data
# ------------------------------------------------------------------------------
df <- raw_policy_data

glimpse(df)
head(df)
print(skim(df))
cat("\nPolicy Start and End seem to be in consistent range\n")
cat("Exposure is also consistent (0,1], and are non-zero\n")

# Profile missing values
df |>
  summarize(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  mutate(pct_missing = round(n_missing / nrow(df) * 100, 2)) |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

mcar_test(df)
cat("Since p is considerably larger than 5%, we fail to reject\n",
    "the null hypothesis, and conclude that data is MCAR.\n\n",
    "Missing data in any of the columns, isn't significant and MCAR.\n")

# Categorical variables
df |>
  select(where(\(x) !is.numeric(x))) |>
  names()

factor_cols <- c("policy.NBRB")
factor_cols |>
  rlang::set_names() |>
  map(\(col) tabyl(df, all_of(col)))

cat("NBRB has some miscoding, needs cleaning.\n")


# ------------------------------------------------------------------------------
# 1.4 Claims Data
# ------------------------------------------------------------------------------
df <- raw_claims_data

glimpse(df)
head(df)
print(skim(df))
cat("No values are missing\n")

# Profile missing values
df |>
  summarize(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  mutate(pct_missing = round(n_missing / nrow(df) * 100, 2)) |>
  arrange(desc(n_missing))

# Categorical variables
factor_cols <- c("claim.Coverage")
factor_cols |>
  rlang::set_names() |>
  map(\(col) tabyl(df, all_of(col)))

cat("Categorical Values seem consistent\n")

claims <- raw_claims_data
claims |>
  count(policy.ID, name = "n_claims") |>
  count(n_claims, name = "n_policies") |>
  mutate(pct = round(n_policies / sum(n_policies) * 100, 2))

cat(c(
  "84.8% of the policies have a single claim",
  "12.79% have two claims, and others have 3-5 claims, rarely 5."
), sep = "\n")


# %% ===========================================================================
# 2. Cleaning
# ==============================================================================
# We will not drop any records until the join.

# ------------------------------------------------------------------------------
# 2.1 Pet Data
# - Recode data appropriately
# - Investigate extra breed of dogs, not in appendix
# - Deduplicate pet records (resolve conflicts with non-null values)
# ------------------------------------------------------------------------------
pets <- raw_pet_data |>
  mutate(
    pet.Type = as.factor(pet.Type),
    pet.Group = as.factor(case_match(
      pet.Group,
      "Non-sporting" ~ "Non-Sporting",
      .default = pet.Group
    )),
    pet.Breed = as.factor(pet.Breed), # NOTE: 3 extra dog breeds
    pet.Gender = as.factor(case_match(
      pet.Gender,
      "F" ~ "Female",
      "M" ~ "Male",
      .default = pet.Gender
    )),
    pet.Offspring = as.factor(pet.Offspring),
    pet.Microchip = as.factor(case_match(
      pet.Microchip,
      "Nope" ~ "No",
      "Y" ~ "Yes",
      .default = pet.Microchip
    )),
    pet.Origin = as.factor(pet.Origin)
  )

pets |>
  count(pet.Breed, sort = TRUE) |>
  head(10)

pets |>
  filter(pet.Breed %in% extra_dog_breeds) |>
  count(pet.Breed, sort = TRUE)

pets |>
  filter(pet.Breed %in% extra_dog_breeds) |>
  filter(if_any(everything(), is.na))

# NOTE: These breeds are not outliers, they seem to represent a population of their breeds
# in the data, and the data vendor probably missed listing them in the appendices, nor
# do they have significant missing records. Thus, we will retain them.

# --- Validation ---
skim(pets)

factor_cols <- c(
  "pet.Type", "pet.Group", "pet.Gender",
  "pet.Offspring", "pet.Microchip", "pet.Origin"
) # left out pet.Breed as it had too many columns
factor_cols |>
  rlang::set_names() |>
  map(\(col) tabyl(pets, all_of(col)))

# --- Deduplicate Pets ---
# Remove exact duplicates and resolve conflicting records by taking non-null values
cat(glue("Pet records before dedup: {nrow(pets)}\n"))

pets <- pets |>
  distinct() |>
  group_by(pet.ID) |>
  summarize(across(everything(), \(x) {
    v <- na.omit(x)
    if (length(v) > 0) v[1] else x[1]
  }), .groups = "drop")

cat(glue("Pet records after dedup: {nrow(pets)} (unique pets: {n_distinct(pets$pet.ID)})\n"))


# ------------------------------------------------------------------------------
# 2.2 Owner Data
# ------------------------------------------------------------------------------
cat("Since there are duplicate records, and 4 (distinct) of them with conflicting values,
we will take the max pet.Count and the non NA values for the rest of the columns.\n")

owners <- raw_owner_data |> 
  group_by(owner.ID) |> 
  summarise(
    # Take the maximum recorded pet count or the first non-NA entry
    owner.PetCount = max(owner.PetCount, na.rm = TRUE),
    # If other columns exist, pick the first non-NA or most complete entry
    across(-owner.PetCount, ~ na.omit(.x)[1]),
    .groups = "drop"
  )

stopifnot(n_distinct(owners$owner.ID) == nrow(owners))

cat("\n\nSuccessfully deduped owner data\n")

(owners |> 
  left_join(pets, by = "owner.ID", relationship = "many-to-many") |> 
  group_by(owner.ID) |> 
  summarize(
    owner.PetCount = last(owner.PetCount),
    pet_count_calc = n_distinct(pet.ID, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  summarise(
    total_owners     = n(),
    matched_count    = sum(owner.PetCount == pet_count_calc, na.rm = TRUE),
    mismatched_count = sum(owner.PetCount != pet_count_calc | is.na(owner.PetCount) != is.na(pet_count_calc))
  ))


# ------------------------------------------------------------------------------
# 2.3 Policy Data
# - Remove records with zero exposures, as they shouldn't contribute to a pure premium calculation.
# - Format the `policy.Start` and `policy.End` as dates
# - rename miscodings in `policy.NBRB`
# ------------------------------------------------------------------------------
# As observed earlier, there are missing values but are significantly small
# in proportion compared to the population, let's check if they are MCAR or not.

# We can go through the profiles of subsets of each missing column one at
# a time to check if the values are MCAR, but we have a convenient test from
# the `naniar` package, which lets us do a pre-test.
# If we do find that there are non-MCAR values, we will do a drill down on
# the individual records

mcar_test(raw_policy_data)
# Since p is considerably large, we fail to reject the null hypothesis,
# and conclude that data is not MCAR.

policy <- raw_policy_data |>
  filter(policy.Exposure > 0) |>
  mutate(
    policy.Start = as.Date(date_decimal(policy.Start)),
    policy.End   = as.Date(date_decimal(policy.End)),
    policy.NBRB  = case_match(
      policy.NBRB,
      "NewBus" ~ "NB",
      "RenBus" ~ "RB",
      .default = policy.NBRB
    )
  )

# --- Validation ---
skim(policy)
factor_cols <- c("policy.NBRB")
factor_cols |>
  rlang::set_names() |>
  map(\(col) tabyl(policy, all_of(col)))


# ------------------------------------------------------------------------------
# 2.4 Claims Data
# ------------------------------------------------------------------------------
# As observed earlier, there were no missing values in claims data
# and categorical columns seemed consistent, so we are ready to go.
claims <- raw_claims_data


# %% ===========================================================================
# 3. Merge
# ==============================================================================
# We have four files to join

# Inspect raw tables before merge
head(claims, 2)
# ID cols: claim.ID, policy.ID
print(glue("Missing Claim IDs: {sum(is.na(claims$claim.ID))}"))

head(owners, 2)
# ID cols: owner.ID
print(glue("Missing pet IDs: {sum(is.na(pets$pet.ID))}"))

# Firstly, we will aggregate the claims by policy
# 1. Aggregate claims by policy
claims_agg <- claims |>
  group_by(policy.ID) |>
  summarize(
    claims_agg.N = n(),
    claims_agg.IllnessLoss = sum(claim.Cost[claim.Coverage == "Illness"]),
    claims_agg.AccidentLoss = sum(claim.Cost[claim.Coverage == "Accident"]),
    .groups = "drop"
  )
# Though we are mentioned to not differentiate between Illness and Accident, we will retain
# the granularity initially, and collapse it into a single total later.
head(claims_agg)

# Then we will perform merge on the various tables
head(policy, 2)
# ID cols: policy.ID, pet.ID
print(glue("Missing Policy IDs: {sum(is.na(policy$policy.ID))}"))

head(pets, 2)
# ID cols: pet.ID owner.ID
print(glue("Missing pet IDs: {sum(is.na(pets$pet.ID))}"))

# Merge Policy & Pets
df <- policy |>
  left_join(pets, by = "pet.ID", relationship = "many-to-many")

df |>
  summarize(n_unmatched_pets = sum(is.na(owner.ID)))

cat("There are no missing Owner.IDs
Thus, there are no pet.IDs in `policy` that was missing in `pets`\n")

# Merge (Policy + Pets) & owners
df <- df |>
  left_join(owners, by = "owner.ID", relationship = "many-to-many")

head(owners, 2)

df |>
  summarize(n_unmatched_pets = sum(is.na(owner.ID)))

cat("There are no missing Owner.IDs
Thus, there are no pet.IDs in `policy` that was missing in `pets`\n")

# Merge (Policy + Pets + owners) + claims_agg
df <- df |>
  left_join(claims_agg, by = "policy.ID", relationship = "many-to-many")

df |>
  summarize(n_unmatched_pets = sum(is.na(owner.ID)))

cat("There are no missing Owner.IDs
Thus, there are no pet.IDs in `policy` that was missing in `pets`\n")

cat("Merge complete. Dimensions of final joined df: ", paste(dim(df), collapse = " x "), "\n")
