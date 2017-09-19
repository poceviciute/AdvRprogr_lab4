
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

# Print
linreg_mod$print()

# Plot
linreg_mod$plot()

# resid
linreg_mod$resid()

# pred
linreg_mod$pred()

c <- linreg_mod$pred()[c(1,5,7)]
c[0]
# a <- round(unname(),2)
b <- c(1.85, 1.53, 1.09)
a == b

# coef
linreg_mod$coef()

# summary
linreg_mod$summary()
