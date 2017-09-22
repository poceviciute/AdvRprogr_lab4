rm(list = ls())

# devtools::install_github("poceviciute/AdvRprogr_lab4")

library(linreg)
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

# devtools::test()

# Print
linreg_mod$print()

# Plot
linreg_mod$plot()

# resid
linreg_mod$resid()

# pred
linreg_mod$pred()

# coef
linreg_mod$coef()

# summary
linreg_mod$summary()


formula <- Petal.Length~Sepal.Width+Sepal.Length
data <- iris

a <- lm(formula, data)
summary(a)
print(a)
