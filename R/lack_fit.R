#' Lack of fit of the regression model.
#'
#' This function performs a hypothesis test for lack of fit
#' in a simple regression model.
#'
#' @param mod A lm object.
#' @example examples/examples_lack_fit.R
#' @return \code{lack_fit} function returns a list with the statistica and p value.
#'
#' @importFrom stats residuals pf
#' @export
lack_fit <- function(mod) {
  stopifnot(class(mod) == 'lm')
  z <- data.matrix(mod$model, rownames.force=NA)
  y <- z[, 1]
  x <- z[, 2]
  n <- length(x)
  m <- length(unique(x))
  if (m == n) stop("The test requires x's not all different")
  SSres <- sum(residuals(mod)^2)
  SSep <- sum(sapply(split(x=y, f=x), function(w) sum((w-mean(w))^2)))
  SSfa <- SSres - SSep
  Fo <- ((SSfa / (m-2)) / (SSep / (n-m)))
  p.value <- pf(Fo, df1=m-2, df2=n-m, lower.tail=F)
  res <- list(SSep=SSep, SSres=SSres, SSfa=SSfa, Fo=Fo, p.value=p.value)
  res
}
