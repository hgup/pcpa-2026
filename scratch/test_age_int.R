suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(glue)
})

train <- read.csv("data/train.csv")

analyze_int_age_by_var <- function(df = train, col) {
  col_name <- rlang::as_label(rlang::enquo(col))

  # 1. Summary by age band & factor
  age_by_col <- df |>
    mutate(age_band = cut(pet.Age, breaks = c(0, 5, 9, 13, 17, 30),
                          labels = c("0-5", "5-9", "9-13", "13-17", "17+"),
                          include.lowest = TRUE)) |>
    summarize(mean_pp = round(mean(pure_premium, na.rm = TRUE), 2), .by = c(age_band, {{ col }})) |>
    mutate({{ col }} := fct_reorder(as.factor({{ col }}), mean_pp, .fun = max, .desc = TRUE))

  # 2. Pivot table display
  wide_tbl <- age_by_col |>
    pivot_wider(names_from = {{ col }}, values_from = mean_pp) |>
    arrange(age_band)
  print(wide_tbl)

  # 3. Interaction plot
  p <- ggplot(age_by_col, aes(x = age_band, y = mean_pp, color = {{ col }}, group = {{ col }})) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_y_continuous(labels = comma) +
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

analyze_int_age_by_var(train, pet.Type)
