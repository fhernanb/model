#' Confidence interval for sigma2 in lm objects.
#'
#' The \code{confint_sigma2} function computes confidence interval for the sigma2 parameter in a linear model fitted with the \link{lm} function.
#'
#' @param object a \link{lm} object.
#' @param level the confidence level required.
#' @examples
#' mod <- lm(dist ~ speed, data=cars)
#' confint_sigma2(object=mod, level=0.95)
#' confint_sigma2(object=mod, level=0.80)
#' @return A matrix with columns giving lower and upper confidence limits.
#'
#' @importFrom stats coef sigma qchisq
#' @export
confint_sigma2 <- function(object, level=0.95) {
  if (class(object) != "lm") stop("The object is not a lm object")
  alpha <- 1 - level
  n <- length(object$residuals)
  p <- length(coef(object))
  s2 <- sigma(object) ^ 2
  den_lower <- qchisq(p=alpha/2, df=n-p, lower.tail=FALSE)
  den_upper <- qchisq(p=1-alpha/2, df=n-p, lower.tail=FALSE)
  lower <- (n-p) * s2 / den_lower
  upper <- (n-p) * s2 / den_upper
  int <- matrix(c(lower, upper), ncol=2)
  colnames(int) <- paste0(100*c(alpha/2, 1-alpha/2), " %")
  rownames(int) <- "Sigma2"
  return(int)
}
