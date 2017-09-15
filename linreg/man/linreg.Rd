\name{linreg}
\alias{linreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
linreg(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (formula, data) 
{
    X <- model.matrix(formula, data)
    dep_name <- all.vars(expr = formula)[1]
    y <- (data[, dep_name])
    beta_hat <- solve((t(X) \%*\% X)) \%*\% t(X) \%*\% y
    y_hat <- X \%*\% beta_hat
    e_hat <- y - y_hat
    n <- nrow(X)
    p <- ncol(X)
    df <- n - p
    sigma_hat <- as.numeric((t(e_hat) \%*\% e_hat)/df)
    var_beta_hat <- sigma_hat * solve((t(X) \%*\% X))
    t_value <- beta_hat/sqrt(diag(var_beta_hat))
    p_value <- pt(t_value, df = df)
    output <- setRefClass("linreg", fields = list(f_formula = "formula", 
        reg_coef = "matrix", fitted_values = "matrix", resid = "matrix", 
        df = "numeric", var_resid = "numeric", p_value = "matrix", 
        t_value = "matrix", var_res_coef = "matrix"), methods = list(print.linreg <<- function(result) {
        return(list(Formula_call = result$f_formula, Regression_Coefficients = result$reg_coef))
    }, plot.linreg <<- function(result) {
        plot_df <- data.frame(resid = result$resid, fitted_values = result$fitted_values, 
            y = y)
        p1 <- ggplot(data = plot_df, aes(x = fitted_values, y = resid)) + 
            geom_point() + geom_smooth(method = "loess", color = "red", 
            se = FALSE) + geom_abline(slope = 0, intercept = 0, 
            linetype = "dotted") + ggtitle("Residual vs Fitted") + 
            ylab("Residuals") + xlab("Fitted Values")
        p2 <- ggplot(data = plot_df, aes(x = fitted_values, y = sqrt(abs((resid - 
            mean(resid))/sqrt(result$var_resid))))) + geom_point() + 
            geom_smooth(method = "loess", color = "red", se = FALSE) + 
            ggtitle("Scale-Location") + ylab("sqrt(abs(Standardized Residuals))") + 
            xlab("Fitted Values")
        return(grid.arrange(p1, p2))
    }, resid.linreg <<- function(result) {
        return(list(Residuals = result$resid))
    }, pred.linreg <<- function(result) {
        return(list(Fitted_Values = result$fitted_values))
    }, coef.linreg <<- function(result) {
        vector <- as.vector(result$reg_coef)
        vect_names <- rownames(result$reg_coef)
        names(vector) <- vect_names
        return(vector)
    }, summary.linreg <<- function(result) {
        coef_mx <- as.matrix(cbind(result$reg_coef, round(sqrt(diag(result$var_res_coef)), 
            3), round(result$t_value, 3), round(result$p_value, 
            3)))
        colnames(coef_mx) <- c("Estimate", "Sd. Error", "T-value", 
            "P-value")
        text <- return(list(Formula = result$f_formula, Residuals = c(Min = min(result$resid), 
            quantile(result$resid, 0.25), Median = median(result$resid), 
            quantile(result$resid, 0.75), Max = max(result$resid)), 
            Coefficients = coef_mx, Evaluation = c(paste("Residual standard error: ", 
                round(sqrt(result$var_resid), 4), " on ", result$df, 
                " degrees of freedom"))))
    }))
    result <- output(f_formula = formula, reg_coef = beta_hat, 
        fitted_values = y_hat, resid = e_hat, df = df, var_resid = sigma_hat, 
        p_value = p_value, t_value = t_value, var_res_coef = var_beta_hat)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
