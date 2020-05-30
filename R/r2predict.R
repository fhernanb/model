#' Calculates R2 prediction statistic.
#'
#' This function calculates R2 prediction statistic.
#'
#' @param mod A lm object.
#' @examples
#' x <- c(1, 1, 2, 3, 3, 4, 4, 4, 4.7, 5, 5.6, 5.6, 5.6, 6, 6, 6.5, 6.9)
#' y <- c(10.84, 9.3, 16.35, 22.88, 24.35, 24.56, 25.86, 29.16, 24.59,
#'        22.25, 25.9, 27.2, 25.61, 25.45, 26.56, 21.03, 21.46)
#' mod <- lm(y ~ x)
#' r2predict(mod)
#' @return The \code{press} function returns the press statistic defined as \eqn{R2predict=1-PRESS/SST}.
#'
#' @importFrom stats lm.influence residuals
#' @export
r2predict <- function(mod) {
  stopifnot(class(mod) == 'lm')
  z <- data.matrix(mod$model, rownames.force=NA)
  y <- z[, 1]
  n <- length(y)
  hii <- lm.influence(mod)$hat     # hii
  ei <- residuals(mod)             # raw residuals
  press <- ei/(1-hii)              # Press Residuals or Deleted Residuals
  sst <- sum(y^2) - sum(y)^2/n
  return(1 - sum(press^2) / sst)
}
