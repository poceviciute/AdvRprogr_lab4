attach(mtcars)
par(mfrow=c(2,1))
qplot(mtcars$wt, mtcars$mpg, geom = "point")
qplot(mtcars$wt, mtcars$disp, geom = "point")

par(mfrow = c(1,2))
ggplot(data = mtcars, aes(wt, mpg)) +
    geom_point()

ggplot(mtcars, aes(wt, disp)) +
    geom_point()

par(mfrow = c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
