#' @title linreg
#' @name linreg
#' @param formula a formula, format y ~ x_1 + x_2 + ... + x_n.
#' @param data a data frame.
#' @return Returns the result of the Linear Regression
#' @description Returns the result of the Linear Regression


linreg <-
function(formula, data) {
    # Define matrix
    X <- model.matrix(formula, data)
    
    # extract the dependant variable from data set
    dep_name <- all.vars(expr = formula)[1]
    y <- (data[, dep_name])
    
    # calculated the predicted values for beta parameter vector
    beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% y
    # colnames(beta_hat) <- "Reg_Coef"
    
    # find the fitted values of y using beta_hat
    y_hat <- X %*% beta_hat
    
    # find the residual values
    e_hat <- y - y_hat
    
    # degrees of freedom
    n <- nrow(X) # number of observations
    p <- ncol(X) # number of parameters
    
    df <- n - p
    
    # residual variance
    sigma_hat <- as.numeric((t(e_hat) %*% e_hat) / df)
    var_beta_hat <- sigma_hat * solve((t(X) %*% X))
    
    #The t-values for each coefficient
    t_value <- beta_hat / sqrt(diag(var_beta_hat))
    p_value <- pt(t_value, df = df)
    
    
    # Results
    result <- output(
        f_formula = formula,
        f_reg_coef = beta_hat,
        f_fitted_values = y_hat,
        f_resid = e_hat,
        f_df = df,
        f_var_resid = sigma_hat,
        f_p_value = p_value,
        f_t_value = t_value,
        f_var_res_coef = var_beta_hat
    )
    # Can't print for 
    # result2 <-
    #     list(Formula = result$f_formula,
    #          Regression_Coefficients = result$reg_coef)
     return(result)
}

data <- iris
formula <-     Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width
lin <- linreg(Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width,iris)