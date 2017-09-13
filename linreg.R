
# linreg ------------------------------------------------------------------
data <- iris
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

linreg <- function(formula, data){
    stopifnot()
    #mf <- model.frame(formula, data)
    mm <- model.matrix(formula, data)

    y <- all.vars(expr = formula)[1]
    
    print(class(mm))
    print(formula)
    #return(mm)
}

linreg(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris)

