# Build RC-class
output <- setRefClass(
    "linreg",
    fields = list(
        f_formula = "formula",
        f_reg_coef = "matrix",
        f_fitted_values = "matrix",
        f_resid = "matrix",
        f_df = "numeric",
        f_var_resid = "numeric",
        f_p_value = "matrix",
        f_t_value = "matrix",
        f_var_res_coef = "matrix"
    ),
    methods = list(
        # Build linreg print function
        print = function() {
            return(
                list(
                    Formula_call = f_formula,
                    Regression_Coefficients = f_reg_coef
                )
            )
        },
        
        # Build linreg plot function
        plot = function() {
            plot_df <- data.frame(
                df_resid = f_resid,
                df_fitted_values = f_fitted_values)
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
                         aes(x = df_fitted_values, y = sqrt(abs((df_resid - mean(df_resid)) / sqrt(f_var_resid)
                         )))) +
                geom_point() +
                geom_smooth(method = "loess",
                            color = "red",
                            se = FALSE) +
                ggtitle("Scale-Location") +
                ylab("sqrt(abs(Standardized Residuals))") +
                xlab("Fitted Values")
            
            return(grid.arrange(p1, p2))
        },
        # Build linreg resid print function
        resid = function() {
            return(list(Residuals = f_resid))
        },
        # Build linreg pred print function
        pred = function() {
            return(list(Fitted_values = f_fitted_values))
        },
        # Build linreg coef print function
        coef = function() {
            vector <- as.vector(f_reg_coef)
            vect_names <-  rownames(f_reg_coef)
            names(vector) <-  vect_names
            return(vector)
        },
        # Build linreg summary print function
        summary = function() {
            coef_mx <- as.matrix(cbind(
                f_reg_coef,
                round(sqrt(diag(
                    f_var_res_coef
                )), 3),
                round(f_t_value, 3),
                round(f_p_value, 3)
            ))
            colnames(coef_mx) <-
                c("Estimate", "Sd. Error", "T-value", "P-value")
            text <-
                return(
                    list(
                        Formula = f_formula,
                        Residuals = c(
                            Min = min(f_resid),
                            quantile(f_resid, .25),
                            Median = median(f_resid),
                            quantile(f_resid, .75),
                            Max = max(f_resid)
                        ),
                        Coefficients = coef_mx,
                        Evaluation = c(
                            paste(
                                "Residual standard error: ",
                                round(sqrt(f_var_resid), 4),
                                " on ",
                                f_df,
                                " degrees of freedom"
                            )
                        )
                    )
                )
        }
    )
)