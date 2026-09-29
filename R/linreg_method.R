#' Print a linreg object
#'
#' Prints the model call and estimated coefficients.
#'
#' @param x An object of class "linreg".
#' @param ... Not used.
#'
#' @return The linreg object invisibly.
#' @export
print.linreg<-function(x, ...){
  # 2.1 give action of print
  # 2.1.1 print coefficients
  cat("Call:\n")
  cat(deparse(x$call), "\n")

  # 2.1.2 print coefficient names
  cat("\nCoefficients:\n")
  print(x$beta_hat)
  invisible(x)
}

#' Plot a linreg object
#'
#' Creates diagnostic plots for a fitted linear regression model.
#'
#' @param x An object of class "linreg".
#' @param ... Not used.
#'
#' @return Two diagnostic plots.
#' @export
plot.linreg <- function(x, ...) {
  # 2.2 plot the following two plots using ggplot2
  # 2.2.1 create dataframe
  plot_data <- data.frame(
    fitted = as.numeric(x$y_hat),
    residuals = as.numeric(x$e_hat)
  )

  # 2.2.2 Standardized residuals
  standardized_residuals <-
    plot_data$residuals / sqrt(x$sigma_hat_square)

  # 2.2.3 Scale-location values
  plot_data$scale_location <-
    sqrt(abs(standardized_residuals))

  # 2.2.4 Median residual for each fitted value
  median_residuals <- aggregate(
    residuals ~ fitted,
    data = plot_data,
    FUN = median
  )

  # 2.2.5 Median scale-location value for each fitted value
  median_scale <- aggregate(
    scale_location ~ fitted,
    data = plot_data,
    FUN = median
  )

  # Formula shown below the x-axis
  formula_text <- paste(
    deparse(x$formula),
    collapse = ""
  )

  # 1.2 Residuals vs Fitted
  p1 <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["fitted"]],
      y = .data[["residuals"]]
    )
  ) +
    ggplot2::geom_point(
      shape = 1
    ) +
    ggplot2::geom_line(
      data = median_residuals,
      ggplot2::aes(
        x = .data[["fitted"]],
        y = .data[["residuals"]]
      ),
      colour = "red",
      linewidth = 0.5
    ) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = paste0(
        "Fitted values\n",
        formula_text
      ),
      y = "Residuals"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA
      ),
      axis.line = ggplot2::element_blank()
    )

  # 1.2 Scale-Location
  p2 <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["fitted"]],
      y = .data[["scale_location"]]
    )
  ) +
    ggplot2::geom_point(
      shape = 1
    ) +
    ggplot2::geom_line(
      data = median_scale,
      ggplot2::aes(
        x = .data[["fitted"]],
        y = .data[["scale_location"]]
      ),
      colour = "red",
      linewidth = 0.5
    ) +
    ggplot2::labs(
      title = "Scale-Location",
      x = paste0(
        "Fitted values\n",
        formula_text
      ),
      y = "Sqrt(|Standardized residuals|)"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA
      ),
      axis.line = ggplot2::element_blank()
    )

  print(p1)
  print(p2)

  invisible(
    list(
      residuals_vs_fitted = p1,
      scale_location = p2
    )
  )
}

#' Extract residuals
#'
#' Returns the residuals from a linreg object.
#'
#' @param object An object of class "linreg".
#' @param ... Not used.
#'
#' @return A numeric vector of residuals.
#' @export
residuals.linreg <- function(object, ...) {
  return(object$e_hat)
}


#' Extract predicted values
#'
#' Generic function for extracting predicted values.
#'
#' @param object An object.
#' @param ... Not used.
#'
#' @return Predicted values.
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' Extract predicted values from a linreg object
#'
#' Returns the fitted values from a linreg object.
#'
#' @param object An object of class "linreg".
#' @param ... Not used.
#'
#' @return A numeric vector of predicted values.
#' @export
pred.linreg <- function(object, ...) {
  return(object$y_hat)
}

#' Extract regression coefficients
#'
#' Returns the estimated coefficients from a linreg object.
#'
#' @param object An object of class "linreg".
#' @param ... Not used.
#'
#' @return A named vector of regression coefficients.
#' @export
coef.linreg <- function(object, ...) {
  return(object$beta_hat)
}

#' Summarize a linreg object
#'
#' Prints the coefficient estimates, standard errors, t-values,
#' p-values and residual standard error.
#'
#' @param object An object of class "linreg".
#' @param ... Not used.
#'
#' @return A summary of the fitted regression model.
#' @export
summary.linreg <- function(object, ...) {

  coefficient_table <- cbind(
    Estimate = object$beta_hat,
    `Std. Error` = sqrt(object$var_beta_diag),
    `t value` = object$t_beta,
    `Pr(>|t|)` = object$p_beta
  )

  cat("Coefficients:\n")

  stats::printCoefmat(
    coefficient_table,
    P.values = TRUE,
    has.Pvalue = TRUE,
    signif.stars = TRUE
  )

  cat(
    "\nResidual standard error:",
    sqrt(object$sigma_hat_square),
    "on",
    object$df,
    "degrees of freedom\n"
  )
}

