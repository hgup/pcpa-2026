suppressPackageStartupMessages({
  library(statmod)
  library(tweedie)
})

y <- rpois(100, 5) + 0.1
x <- rnorm(100)
m1 <- glm(y ~ 1, family = tweedie(var.power = 1.5, link.power = 0))
m2 <- glm(y ~ x, family = tweedie(var.power = 1.5, link.power = 0))

a <- anova(m1, m2, test = "Chisq")
print(a)
print(colnames(a))
print(str(a))
