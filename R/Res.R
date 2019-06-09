#' Compute residuals for \code{lm} objects.
#'
#' \code{Res} function computes Standardized Residuals,
#' Studentized Residuals, Press Residuals or Deleted Residuals
#' and Externally studentized residuals.
#'
#' @param mod A lm object.
#' @examples
#' # Multiple linear regression
#' mod <- lm(mpg ~ hp + wt, data=mtcars)
#' #
#' res <- Res(mod)  # Residuals
#' res$ri  # To obtain Studentized Residuals (ri)
#' #
#' par(mfrow=c(2, 2))
#' plot(res$di, ylab=expression(d[i]))
#' abline(h=-3, lty='dotted')
#' abline(h=+3, lty='dotted')
#' plot(res$ri, ylab=expression(r[i]))
#' plot(res$pres, ylab=expression(e[(i)]))
#' plot(res$ti, ylab=expression(t[i]))
#' @return \code{Res} function returns a dataframe with residuals.
#'
#' @importFrom stats lm.influence residuals
#' @export
Res <- function(mod) {
  if (class(mod) != "lm") stop("The model is not a lm object")
  hii <- lm.influence(mod)$hat     # hii
  sd_e <- summary(mod)$sigma       # Sigma_e
  ei <- residuals(mod)             # raw residuals
  di <- (ei-mean(ei)) / sd_e       # Standardized Residuals
  ri <- ei/sqrt(sd_e^2*(1-hii))    # Studentized Residuals
  press <- ei/(1-hii)              # Press Residuals or Deleted Residuals
  n <- length(mod$residuals)
  p <- length(mod$coefficients)
  ti <- ri * ((n-p-1)/(n-p-ri^2))^0.5 # Externally studentized residuals
  data.frame(ei=ei, di=di, ri=ri, press=press, ti=ti)
}
