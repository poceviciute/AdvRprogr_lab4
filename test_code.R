
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

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
