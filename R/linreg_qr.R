#' Linear regression using QR decomposition
#'
#' Fits a multiple linear regression model using QR decomposition
#' to calculate the regression coefficients and their variances.
#'
#' @param formula A formula defining the response and predictor variables.
#' @param data A data frame containing the variables used in the model.
#'
#' @return An object of class "linreg_qr" containing the regression results.
#'
#' @examples
#' fit <- linreg_qr(
#'   Petal.Length ~ Sepal.Width + Sepal.Length,
#'   data = iris
#' )
#'
#' @export
linreg_qr <- function(formula, data){
  # generate the matrix x using model.matrix
  X <- model.matrix(formula, data)

  # find the variable y
  vari <- all.vars(formula)
  y <- data[[vari[1]]]

  # QR decomposition
  qr_X <- qr(X)

  # get Q and R
  Q <- qr.Q(qr_X)
  R <- qr.R(qr_X)

  # acquire the regression coefficient beta_hat
  Qt_y <- t(Q) %*% y
  beta_hat <- backsolve(R, Qt_y)

  # calculate the fitted value y_hat
  y_hat <- X %*% beta_hat

  # calculate the residual
  e_hat<-y-y_hat

  # n is the number of the data
  n<-nrow(X)
  # p is the number of parameters
  p<-ncol(X)
  # calculate degrees of freedom
  df<-n-p

  # calculate the residual variance
  sigma_hat_square<-as.numeric(t(e_hat) %*% e_hat / df)

  # calculate the variance of the regression coefficients
  R_inv <- backsolve(R, diag(ncol(R)))
  var_beta <-
    sigma_hat_square * R_inv %*% t(R_inv)

  # extract the diagonal entries of var_beta to get the variance of each coefficient
  var_beta_diag<-diag(var_beta)
  # calculate t_values of each coefficient
  t_beta<- as.vector(beta_hat)/sqrt(var_beta_diag)

  # calculate p_values
  p_beta <- 2*pt(abs(t_beta), df=df, lower.tail=FALSE)

  # store results
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
  )

  names(results$beta_hat) <- colnames(X)
  names(results$var_beta_diag) <- colnames(X)
  names(results$t_beta) <- colnames(X)
  names(results$p_beta) <- colnames(X)

  class(results) <- "linreg_qr"

  return(results)
}
