#' Calculates R2 prediction statistic.
#'
#' This function calculates R2 prediction statistic.
#'
#' @param mod A lm object.
#' @example examples/examples_r2predict.R
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
