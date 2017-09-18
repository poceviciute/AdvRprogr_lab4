head(mtcars)
head(iris)

library(linreg)
# formula <- hp ~ disp + mpg + cyl
# data <- mtcars

# formula <- Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species
# data <- iris

eval <- linreg(hp ~ disp + mpg + cyl, mtcars)
eval <- linreg(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species, iris)

eval 
class(eval)
str(eval)

print(eval)
plot(eval)
resid(eval)
pred(eval)
coef(eval)
summary(eval)

eval$print.linreg()
