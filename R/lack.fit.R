#' Lack of fit of the regression model.
#'
#' This function performs a hypothesis test for lack of fit
#' for simple regression model.
#'
#' @param mod A lm object.
#' @examples
#' x <- c(1, 1, 2, 3, 3, 4, 4, 4, 4.7, 5, 5.6, 5.6, 5.6, 6, 6, 6.5, 6.9)
#' y <- c(10.84, 9.3, 16.35, 22.88, 24.35, 24.56, 25.86, 29.16, 24.59,
#'        22.25, 25.9, 27.2, 25.61, 25.45, 26.56, 21.03, 21.46)
#' mod <- lm(y ~ x)
#' result <- lack.fit(mod)
#' result
#' # To explore the elements inside result object
#' names(result)
#' result$SSep
#' @return \code{lack.fit} function returns a list with the statistica and p value.
#'
#' @importFrom stats residuals pf
#' @export
lack.fit <- function(mod) {
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
