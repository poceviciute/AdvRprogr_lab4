library(ggplot2)
library(gridExtra)

# linreg ------------------------------------------------------------------
data <- iris
formula <- Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width
formula <- Petal.Length ~ Species

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
            fitted_values = "matrix",
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
            },
            plot.linreg <<- function(result) {
                plot_df <- data.frame(
                    resid = result$resid,
                    fitted_values = result$fitted_values,
                    y = y
                )
                p1 <- ggplot(data = plot_df, aes(x = fitted_values, y = resid)) +
                    geom_point() +
                    geom_smooth(method = "loess",
                                color = "red",
                                se = FALSE) +
                    geom_abline(slope = 0,
                                intercept = 0,
                                linetype = "dotted") +
                    ggtitle("Residual vs Fitted") +
                    ylab("Residuals") +
                    xlab("Fitted Values")
                
                p2 <- ggplot(data = plot_df, 
                             aes(x = fitted_values, y = sqrt(abs((resid - mean(resid)) / sqrt(result$var_resid))))) +
                    geom_point() +
                    geom_smooth(method = "loess",
                                color = "red",
                                se = FALSE) +
                    ggtitle("Scale-Location") +
                    ylab("sqrt(abs(Standardized Residuals))") +
                    xlab("Fitted Values")
             
               return(grid.arrange(p1, p2))       
            }
        )
    )
    
    result <- output(
        f_formula = formula,
        reg_coef = beta_hat,
        fitted_values = y_hat,
        resid = e_hat,
        df = df,
        var_resid = sigma_hat,
        p_value = p_value,
        t_value = t_value,
        var_res_coef = var_beta_hat)
    return(result)
}
eval <- linreg(formula, iris)

plot(eval)
print(eval)

lin <- lm(formula, iris)
plot(lin)

table(iris$Species)
