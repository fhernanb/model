#' Lack of fit of the regression model.
#'
#' This function performs a hypothesis test for lack of fit
#' in a simple regression model.
#'
#' @param mod A lm object.
#' @example examples/examples_lack_fit_test.R
#' @return \code{lack_fit} function returns the anova table.
#'
#' @importFrom stats residuals pf
#' @export
lack_fit_test <- function(mod) {
  stopifnot(class(mod) == 'lm')
  
  z <- data.matrix(mod$model, rownames.force=NA)
  y <- z[, 1]
  x <- z[, 2]
  n <- length(x)
  
  # Usual anova
  ss_t <- sum(y^2) - sum(y)^2 / n
  ss_res <- sum(residuals(mod)^2)
  ss_r <- ss_t - ss_res
  ms_r <- ss_r / 1
  ms_res <- ss_res / (n-2)
  F0_1 <- ms_r / ms_res
  p_value_1 <- pf(F0_1, df1=1, df2=n-2, lower.tail=FALSE)
  
  # lof anova
  m <- length(unique(x))
  if (m == n) stop("The test requires x's not all different")
  ss_pe <- sum(sapply(split(x=y, f=x), function(w) sum((w-mean(w))^2)))
  ss_lof <- ss_res - ss_pe
  ms_lof <- ss_lof / (m-2)
  ms_pe <- ss_pe / (n-m)
  F0_2 <- ((ss_lof / (m-2)) / (ss_pe / (n-m)))
  p_value_2 <- pf(F0_2, df1=m-2, df2=n-m, lower.tail=FALSE)
  
  # Organizing the output
  sum_of_square <- c(ss_r, ss_res, ss_lof, ss_pe, ss_t)
  df <- c(1, n-2, m-2, n-m, n-1)
  mean_square <- c(ms_r, ms_res, ms_lof, ms_pe, NA)
  F <- c(F0_1, NA, F0_2, NA, NA)
  pvalue <- c(p_value_1, NA, p_value_2, NA, NA)
  
  # The output
  result <- data.frame(sum_of_square, df, mean_square, F, pvalue)
  row.names(result) <- c("Regression","Residuals", 
                         "Lack of fit", "Pure error",
                         "Total")
  names(result) <- c("Sum Sq", "Df", "Mean Sq", "F value", "Pr(>F)")
  
  class(result) <- c("anova", "data.frame")
  
  attr(result, "heading") <- c("Lack of fit test - Anova Table")
  
  result
}
