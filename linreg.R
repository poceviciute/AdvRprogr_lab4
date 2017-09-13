



# linreg ------------------------------------------------------------------
data <- iris
formula <-
    Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width

linreg <- function(formula, data) {
    stopifnot()
    #mf <- model.frame(formula, data)
    X <- model.matrix(formula, data)
    
    # extract the dependant variable from data set
    dep_name <- all.vars(expr = formula)[1]
    y <- (data[, dep_name])
    
    # calculated the predicted values for beta parameter vector
    beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% y
    
    # find the fitted values of y using beta_hat
    y_hat <- X %*% beta_hat
    
    # find the residual values
    e_hat <- y - y_hat
    
    n <- nrow(X) #number of observations
    p <- length(all.vars(formula)) - length(dep_name) #number of parameters
    
    # degrees of freedom
    df <- n - p
    
    # residual variance
    sigma_hat <- as.numeric((t(e_hat) %*% e_hat) / df)
    var_beta_hat <- sigma_hat * solve((t(X) %*% X))
    
    #The t-values for each coefficient
    t_value <- beta_hat / sqrt(diag(var_beta_hat))
    p_value <- pt(t_value, df = df)
}

# linreg(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#        iris)
