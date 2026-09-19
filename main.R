# ===============================================
# SETUP (set to automatically install packages if unavailable)
# ===============================================
if (!requireNamespace("pacman", quietly = TRUE)) { install.packages("pacman") }
library(pacman)
pacman::p_load(tidyverse, lubridate, ggplot2, dplyr, rlang, janitor, skimr, naniar, glue)


# ===============================================
# INPUTS (appendices and raw data)
# ===============================================

appendix_breed_cats <- "Abyssinian, American Bobtail, American Curl, American Shorthair, American Wirehair, Asian, Asian Semi Longhair, Australian Mist, Balinese, Bambino, Bengal, Birman, Bombay, Brazilian Shorthair, British Shorthair, Burmese, Chantilly Tiffany, Chartreux, Chausie, Cornish Rex, Cymric, Cyprus, Devon Rex, Donskoy, Egyptian Mau, European Burmese, European Shorthair, Exotic Shorthair, German Rex, Havana, Havana Brown, Highlander, Himalayan, Japanese Bobtail, Javanese, Korat, Kurilian Bobtail, LaPerm, Lykoi, Maine Coon, Manx, Minskin, Munchkin, Napoleon, Nebelung, Ocicat, Ojos Azules, Oriental, Oriental Bicolor, Oriental Longhair, Persian, Peterbald, Pixiebob, Ragamuffin, Ragdoll, Russian Blue, Scottish Fold, Selkirk Rex, Seychellois, Siamese, Siberian, Siberian Forest, Singapura, Snowshoe, Sokoke, Somali, Sphynx, Thai, Tiffany, Tonkinese, Toybob, Toyger, Turkish Angora, Turkish Van, Ukrainian Levkoy, Ussuri, York Chocolate"
appendix_breed_dogs <- " Afghan Hound, Airedale Terrier, Akita, Alaskan Malamute, American Eskimo Dog, American Foxhound, American Staffordshire Terrier, Anatolian Shepherd Dog, Australian Cattle Dog, Australian Shepherd, Australian Terrier, Basenji, Basset Hound, Beagle, Bearded Collie, Beauceron, Bedlington Terrier, Belgian Malinois, Belgian Sheepdog, Belgian Tervuren, Bernese Mountain Dog, Bichon Frise, Black and Tan Coonhound, Bloodhound, Border Collie, Border Terrier, Borzoi, Boston Terrier, Boxer, Boykin Spaniel, Briard, Brittany, Brussels Griffon, Bull Terrier, Bulldog, Cairn Terrier, Canaan Dog, Cardigan Welsh Corgi, Cavalier King Charles Spaniel, Chesapeake Bay Retriever, Chihuahua, Chinese Crested, Chow Chow, Clumber Spaniel, Cocker Spaniel, Collie, Coton de Tulear, Dachshund, Dalmatian, Dandie Dinmont Terrier, Doberman Pinscher, Dogo Argentino, English Cocker Spaniel, English Foxhound, English Setter, English Springer Spaniel, English Toy Spaniel, Entlebucher Mountain Dog, Field Spaniel, Finnish Lapphund, Finnish Spitz, French Bulldog, German Pinscher, German Shepherd Dog, German Shorthaired Pointer, German Wirehaired Pointer, Giant Schnauzer, Glen of Imaal Terrier, Golden Retriever, Gordon Setter, Great Dane, Great Pyrenees, Greater Swiss Mountain Dog, Greyhound, Harrier, Havanese, Irish Red and White Setter, Irish Setter, Irish Terrier, Irish Water Spaniel, Irish Wolfhound, Italian Greyhound, Japanese Chin, Keeshond, Kerry Blue Terrier, Komondor, Kuvasz, Labrador Retriever, Lagotto Romagnolo, Lakeland Terrier, Leonberger, Lhasa Rapso, Maltese, Manchester Terrier, Mastiff, Miniature Bull Terrier, Miniature Pinscher, Miniature Schnauzer, Newfoundland, Norfolk Terrier, Norwegian Buhund, Norwegian Elkhound, Norwegian Lundehund, Norwich Terrier, Nova Scotia Duck Tolling Retriever, Old English Sheepdog, Otterhound, Parson Russell Terrier, Pekingese, Pembroke Welsh Corgi, Pharaoh Hound, Pointer, Polish Lowland Sheepdog, Pomeranian, Poodle, Portuguese Water Dog, Pug, Puli, Pumi, Pyrenean Shepherd, Redbone Coonhound, Rhodesian Ridgeback, Rottweiler, Saint Bernard, Saluki, Samoyed, Schipperke, Scottish Deerhound, Scottish Terrier, Sealyham Terrier, Shetland Sheepdog, Shiba Inu, Shih Tzu, Siberian Husky, Silky Terrier, Skye Terrier, Sloughi, Soft Coated Wheaten Terrier, Spanish Water Dog, Standard Schnauzer, Sussex Spaniel, Swedish Vallhund, Tibetan Mastiff, Tibetan Spaniel, Tibetan Terrier, Toy Fox Terrier, Treeing Walker Coonhound, Vizsla, Weimaraner, Welsh Springer Spaniel, Welsh Terrier, West Highland White Terrier, Whippet, Wire Fox Terrier, Wirehaired Pointing Griffon, Wirehaired Vizsla, Xoloitzcuintli, Yorkshire Terrier. "

raw_pet_data <- read.csv("data/pet-data.csv")
raw_owner_data <- read.csv("data/owner-data.csv")
raw_policy_data <- read.csv("data/policy-data.csv")
raw_claims_data <- read.csv("data/claims-data.csv")


# ===============================================
# UTILITY FUNCTIONS (repetitive for all data sets)
# ===============================================

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

# Outlier analysis across multiple numeric columns (summary table, boxplots, histograms)
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



make_observations <- function(dataset_name, ...) {
  print_header(glue("Observations for `{dataset_name}` Dataset"))
  
  obs <- list(...)
  env <- parent.frame()
  
  for (i in seq_along(obs)) {
    text <- glue(obs[[i]], .envir = env)
    cat(sprintf("%d. %s\n", i, text))
  }
}

# ===============================================
# PETS DATASET
# ===============================================

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
  
  print(glue("Cat breeds present in the data, but missing the list:"))
  extra_breeds <- setdiff(breeds_actual, breeds_expected)
  cat(paste0(seq_along(extra_breeds), ". ", extra_breeds, "\n"), sep = "")
  
  return(extra_breeds)
}
extra_dog_breeds <- check_breeds("Dog", appendix_breed_dogs)
extra_cat_breeds <- check_breeds("Cat", appendix_breed_cats)


make_observations(
  "Pets",
  "Policy Start and End seem to be in consistent range according to the data dictionary",
  "Exposure is also consistent (0,1], and are non-zero according to the data dictionary",
  "`p` in the MCAR test is significantly larger than 5%, thus its safe to affirm that data is missing completely at random, and also a very low percentage of claims are missing. Thus, we can drop those records (after merging the datasets).",
  "Gender is coded with both `M` and `Male` format, needs cleaning",
  "Microchip is coded with both `No`, `Nope`, `Y`, `Yes`, needs cleaning",
  "In pet.Group: `Non-Sporting` has two spellings, needs cleaning",
  "The {toString(extra_dog_breeds)} are dog breeds which are not present in the appendix.",
  "There are 369 rows with duplicate pet.ID in Pets, out of which 32 have conflicting values. Observing the 32 conflicting values, most of them are due to NA values.",
  "In outlier analysis (1.5 * IQR), `pet.Age` and `pet.Height` exhibit 0 outliers, whereas `pet.Weight` exhibits 1,808 outliers (4.85% of total records) with values exceeding the upper cutoff of 106.6 lbs, driven by large/giant dog breeds."
)


# ========
# CLEANING `PETS` DATASET
# ========

pets <- raw_pet_data |>
  mutate(
    pet.Type = as.factor(pet.Type),
    pet.Group = as.factor(
      case_match(pet.Group, "Non-sporting" ~ "Non-Sporting", .default = pet.Group)
    ),
    pet.Breed = as.factor(pet.Breed),
    # NOTE: 3 extra dog breeds
    pet.Gender = as.factor(
      case_match(pet.Gender, "F" ~ "Female", "M" ~ "Male", .default = pet.Gender)
    ),
    pet.Offspring = as.factor(pet.Offspring),
    pet.Microchip = as.factor(
      case_match(pet.Microchip, "Nope" ~ "No", "Y" ~ "Yes", .default = pet.Microchip)
    ),
    pet.Origin = as.factor(pet.Origin)
  )

# How are we to handle extra dog breeds?
pets |>
  filter(pet.Breed %in% extra_dog_breeds) |>
  count(pet.Breed, sort = TRUE)

pets |>
  filter(pet.Breed %in% extra_dog_breeds) |>
  filter(if_any(everything(), is.na))

# These breeds are not outliers, they seem to represent a population of their breeds
# in the data, and the data vendor probably missed listing them in the appendices, nor
# do they have significant missing records. Thus, we will retain them.


# ===============================================
# OWNERS DATASET
# ===============================================
