suppressPackageStartupMessages({library(glue)})

# Function to report model fit and comparative AIC using glue()
compare_model_fit <- function(model_b, model_a, name_b = "Model B", name_a = "Model A", aic_b = NULL, aic_a = NULL) {
  aic_a <- aic_a %||% AIC(model_a)
  aic_b <- aic_b %||% AIC(model_b)
  
  delta_aic <- round(aic_b - aic_a, 2)
  dev_exp   <- round((1 - model_b$deviance / model_b$null.deviance) * 100, 2)
  
  msg <- glue("
  Baseline AIC ({name_a}) : {round(aic_a, 2)}
  {name_b} AIC            : {round(aic_b, 2)}
  Delta AIC ({name_b} - {name_a})      : {delta_aic}
  % Deviance Explained   : {dev_exp}%
  ")
  
  cat(msg, "\n")
  invisible(list(aic_base = aic_a, aic_new = aic_b, delta_aic = delta_aic, dev_explained = dev_exp))
}

# Example test
m1 <- glm(mpg ~ wt, data = mtcars)
m2 <- glm(mpg ~ wt + hp, data = mtcars)

compare_model_fit(m2, m1)
