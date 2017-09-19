#' @title linreg
#' @name linreg
#' @param formula a formula, format y ~ x_1 + x_2 + ... + x_n.
#' @param data a data frame.
#' @return Returns the result of the Linear Regression
#' @description Returns the result of the Linear Regression
#' @import methods
#' @export linreg
#' @exportClass linreg

linreg <- setRefClass(
    Class = "linreg",
    fields = list(
        formula = "formula",
        data = "data.frame",
        beta_hat = "matrix",
        y_hat = "matrix",
        e_hat = "matrix",
        df = "numeric",
        var_sigma_hat = "numeric",
        p_value = "matrix",
        t_value = "matrix",
        var_beta_hat = "matrix"
    ),
    methods = list(
        initialize = function(formula, data){
            formula <<- formula
            data <<- data
            
            # Define matrix
            X <- model.matrix(formula, data)
            
            # extract the dependant variable from data set
            dep_name <- all.vars(expr = formula)[1]
            y <- (data[, dep_name])
           
             # calculated the predicted values for beta parameter vector
            beta_hat <<- solve((t(X) %*% X)) %*% t(X) %*% y
            
            # find the fitted values of y using beta_hat
            y_hat <<- X %*% beta_hat
            
            # find the residual values
            e_hat <<- y - y_hat
            
            # degrees of freedom
            n <- nrow(X) # number of observations
            p <- ncol(X) # number of parameters
            
            df <<- n - p
            
            # residual variance
            var_sigma_hat <<- as.numeric((t(e_hat) %*% e_hat) / df)
            var_beta_hat <<- var_sigma_hat * solve((t(X) %*% X))
            
            #The t-values for each coefficient
            t_value <<- beta_hat / sqrt(diag(var_beta_hat))
            p_value <<- pt(t_value, df = df)
        },
        # Build linreg print function
        print = function() {
            return(
                list(
                    Formula_call = formula,
                    Regression_Coefficients = beta_hat
                )
            )
        },
        
        # Build linreg plot function
        plot = function() {
            library(ggplot2)
            
            plot_df <- data.frame(
                df_resid = e_hat,
                df_fitted_values = y_hat)
            p1 <-
                ggplot(data = plot_df, aes(x = df_fitted_values, y = df_resid)) +
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
                         aes(x = df_fitted_values, y = sqrt(abs((df_resid - mean(df_resid)) / sqrt(var_sigma_hat)
                         )))) +
                geom_point() +
                geom_smooth(method = "loess",
                            color = "red",
                            se = FALSE) +
                ggtitle("Scale-Location") +
                ylab("sqrt(abs(Standardized Residuals))") +
                xlab("Fitted Values")
            
            return(list(Residual_vs_Fitted = p1, 
                        Scale_Location = p2))
        },
        # Build linreg resid print function
        resid = function() {
            return((Residuals = round(e_hat, 2)))
        },
        # Build linreg pred print function
        pred = function() {
            return((Fitted_values = round(y_hat, 2)))
        },
        # Build linreg coef print function
        coef = function() {
            vector <- as.vector(beta_hat)
            vect_names <-  rownames(beta_hat)
            names(vector) <-  vect_names
            return(vector)
        },
        # Build linreg summary print function
        summary = function() {
            coef_mx <- as.matrix(cbind(
                beta_hat,
                round(sqrt(diag(
                    var_beta_hat
                )), 3),
                round(t_value, 3),
                round(p_value, 3)
            ))
            colnames(coef_mx) <-
                c("Estimate", "Sd. Error", "T-value", "P-value")
            text <-
                return(
                    list(
                        Formula = formula,
                        Residuals = c(
                            Min = min(e_hat),
                            quantile(e_hat, .25),
                            Median = median(e_hat),
                            quantile(e_hat, .75),
                            Max = max(e_hat)
                        ),
                        Coefficients = coef_mx,
                        Evaluation = c(
                            paste(
                                "Residual standard error: ",
                                round(sqrt(var_sigma_hat), 4),
                                " on ",
                                df,
                                " degrees of freedom"
                            )
                        )
                    )
                )
        }
    )
)
