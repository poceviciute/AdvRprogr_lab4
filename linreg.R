# linreg ------------------------------------------------------------------
data <- iris
formula <- Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width


linreg <- function(formula, data) {
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
    n <- nrow(X) #number of observations
    p <- length(all.vars(formula)) - length(dep_name) #number of parameters
    
    df <- n - p
    
    # residual variance
    sigma_hat <- as.numeric((t(e_hat) %*% e_hat) / df)
    var_beta_hat <- sigma_hat * solve((t(X) %*% X))
    
    #The t-values for each coefficient
    t_value <- beta_hat / sqrt(diag(var_beta_hat))
    p_value <- pt(t_value, df = df)
    
    # Build RC-class
    output <- setRefClass(
        "linreg",
        fields = list(
            f_formula = "formula",
            reg_coef = "matrix",
            fitted_values = "numeric",
            resid = "matrix",
            df = "numeric",
            var_resid = "numeric",
            p_value = "matrix",
            t_value = "matrix",
            var_res_coef = "matrix"
        ),
        methods = list(
            print.linreg <<- function(result) {
                return(list(Formula_call = result$f_formula,
                            Regression_Coefficients = result$reg_coef))
            }
            plot.linreg <<- function(result){
                ggplot(data = result, aes(x = fitted_values, y = resid)) +
                    geom_point()
            }
        )
    )
    
    result <- output(
        f_formula = formula,
        reg_coef = beta_hat,
        fitted_values = y,
        resid = e_hat,
        df = df,
        var_resid = sigma_hat,
        p_value = p_value,
        t_value = t_value,
        var_res_coef = var_beta_hat)
    return(result)
}
eval <- linreg(formula, iris)

print(eval)
plot(eval)

library(ggplot2)
ggplot(data = result, aes(x = fitted_values, y = resid)) +
    geom_point()
