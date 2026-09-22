suppressPackageStartupMessages({
  library(statmod)
  library(tweedie)
  library(broom)
})

y <- rpois(100, 5) + 0.1
x <- rnorm(100)
m1 <- glm(y ~ 1, family = tweedie(var.power = 1.5, link.power = 0))
m2 <- glm(y ~ x, family = tweedie(var.power = 1.5, link.power = 0))

cat("--- anova m1, m2 ---\n")
a <- anova(m1, m2, test = "Chisq")
tryCatch(print(tidy(a)), error = function(e) print(e))

cat("\n--- anova single m2 ---\n")
a2 <- anova(m2, test = "Chisq")
tryCatch(print(tidy(a2)), error = function(e) print(e))

cat("\n--- drop1 m2 ---\n")
d <- drop1(m2, test = "Chisq")
tryCatch(print(tidy(d)), error = function(e) print(e))
