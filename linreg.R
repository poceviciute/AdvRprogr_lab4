
# linreg ------------------------------------------------------------------
data <- iris
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

linreg <- function(formula, data){
    stopifnot()
    mf <- model.frame(formula, data)
    mm <- model.matrix(formula, mf)

    dep <- all.vars(expr = formula, functions = )
    
    print(class(mm))
    #return()
}

linreg(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris)


