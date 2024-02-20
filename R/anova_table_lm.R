#' Lack of fit of the regression model.
#'
#' This function performs a hypothesis test for lack of fit
#' in a simple regression model.
#'
#' @param mod A lm object.
#' @example examples/examples_anova_table_lm.R
#' @return \code{anova_table_lm} function returns the anova table.
#'
#' @importFrom stats residuals pf
#' @importFrom stats update
#' @export
anova_table_lm <- function(mod) {
  stopifnot(class(mod) == 'lm')
  
  # mod <- update(mod, x=TRUE, y=TRUE)
  # x <- mod$x
  # y <- mod$y
  
  z <- data.matrix(mod$model, rownames.force=NA)
  y <- z[, 1]
  x <- z[, -1]
  
  n <- length(y)
  
  # Usual anova
  ss_t <- sum(y^2) - sum(y)^2 / n
  ss_res <- sum(residuals(mod)^2)
  ss_r   <- ss_t - ss_res
  ms_r   <- ss_r / (length(coef(mod))-1)
  ms_res <- ss_res / (n-length(coef(mod)))
  F0 <- ms_r / ms_res
  p_value <- pf(F0, df1=length(coef(mod))-1, df2=(n-length(coef(mod))), lower.tail=FALSE)
  
  # Organizing the output
  sum_of_square <- c(ss_r, ss_res, ss_t)
  df <- c(length(coef(mod))-1, n-length(coef(mod)), n-1)
  mean_square <- c(ms_r, ms_res, NA)
  F <- c(F0, NA, NA)
  pvalue <- c(p_value, NA, NA)
  
  # The output
  result <- data.frame(sum_of_square, df, mean_square, F, pvalue)
  row.names(result) <- c("Regression","Residuals", "Total")
  names(result) <- c("Sum Sq", "Df", "Mean Sq", "F value", "Pr(>F)")
  
  class(result) <- c("anova", "data.frame")
  
  attr(result, "heading") <- c("Anova Table")
  
  result
}
