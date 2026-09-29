#' Fit a linear regression model
#'
#' Creates a multiple linear regression model based on a given formula
#' and data set.
#'
#' @param formula The formula that describes the regression model.
#' @param data The data frame used to fit the model.
#'
#' @return A "linreg" object with the estimated coefficients,
#' fitted values, residuals and other model results.
#'
#' @examples
#' model <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, iris)
#'
#' @export
linreg <- function(formula, data){
  # 1. function sturcture
  # 1.1 Using ordinary linear algebra and calculate
  # generate the matrix x using model.matrix
  X <- model.matrix(formula, data)

  # find the variable y, x1, x2, x3
  vari <- all.vars(formula)
  # extract data[y]
  y <- data[[vari[1]]]

  # 1.1.1 regression coefficient beta_hat=(x^TX)^{-1}X^Ty
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

  # 1.1.2 fitted value y_hat=X*beta_hat
  y_hat <- X %*% beta_hat

  # 1.1.3 the residuals e_hat=y-y_hat
  e_hat<-y-y_hat

  # 1.1.4 The degrees of freedom
  # 1.1.4.1 n is the number of the data
  n<-nrow(X)
  # 1.1.4.2 p is the number of parameters
  p<-ncol(X)
  # 1.1.4.3 df=n-p
  df<-n-p

  # 1.1.5 the residual variance sigma_hat_square=e^Te/df
  sigma_hat_square<-as.numeric(t(e_hat) %*% e_hat / df)

  # 1.1.6 the variance of the regression coefficients
  var_beta <- sigma_hat_square * solve(t(X) %*% X)

  # 1.1.7 t-values for each coefficient
  # 1.1.7.1 the diagonal entries of var_beta to get the variance of each coefficient
  var_beta_diag<-diag(var_beta)
  # 1.1.7.2 calculate t_values of each coefficient
  t_beta<- as.vector(beta_hat)/sqrt(var_beta_diag)

  # 1.1.8 p_values for each regression coefficient
  p_beta <- 2*pt(abs(t_beta), df=df, lower.tail=FALSE)

  # 1.2 store results as objects
  results <- list(
    beta_hat = as.vector(beta_hat),
    y_hat = as.vector(y_hat),
    e_hat = as.vector(e_hat),
    df = df,
    sigma_hat_square = sigma_hat_square,
    var_beta = var_beta,
    var_beta_diag = var_beta_diag,
    t_beta = t_beta,
    p_beta = p_beta,
    formula = formula,
    call = match.call()
    # linreg(formula = Petal.Length ~ Sepal.Width, data = iris)
  )
  # 1.3 give names to the following parameters
  names(results$beta_hat) <- colnames(X)
  names(results$var_beta_diag) <- colnames(X)
  names(results$t_beta) <- colnames(X)
  names(results$p_beta) <- colnames(X)

  # 1.4 return an object of class linreg,
  class(results) <- "linreg"

  return(results)
}
