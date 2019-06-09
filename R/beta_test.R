#' Hypothesis testing for coefficients in lm objects.
#'
#' The \code{beta_test} function performs a hypothesis test for each coefficient when
#' the reference value is different to zero.
#'
#' @param object A \link{lm} object.
#' @param alternative a character string specifying the alternative
#'  hypothesis, must be one of "two.sided" (default), "greater" or
#'  "less". You can specify just the initial letter.
#' @param ref.value is a numeric vector with the reference values
#'  to perform the test \eqn{Ho: \betaj = \betajo}.
#' @param parm is a vector with the coefficient names.
#' @examples
#'
#' # Linear regression
#' mod1 <- lm(dist ~ speed, data=cars)
#' coef(mod1)
#' #
#' # to test beta_speed = 3 vs beta_speed < 3
#' beta_test(object=mod1, parm='speed', ref.value=3, alternative='less')
#' #-------------
#' # Multiple linear regression
#' mod2 <- lm(mpg ~ hp + wt, data=mtcars)
#' #
#' # to test separately
#' # beta_hp = -0.05 vs beta_hp < -0.05 and
#' # beta_wt = -3 vs beta_wt != -3
#' alternative <- c("less", "two.sided")
#' ref.value <- c(-0.05, -3)
#' parm <- c("hp", "wt")
#' beta_test(mod2, alternative, parm, ref.value)
#' @return The \code{beta_test} function returns a matrix with the estimated coefficient,
#' standard error, t value and p-value.
#'
#' @importFrom stats coefficients residuals coef pt
#' @export
beta_test <- function (object,
                       alternative = c("two.sided", "less", "greater"),
                       parm,
                       ref.value)
{
  if (class(object) != "lm") stop("The object is not a lm object")
  if (!is.numeric(ref.value)) stop("The reference values need to be numeric")
  if (!any(parm %in% names(object$coefficients))) stop("Some parm name is wrong")
  bj <- coefficients(summary(object))[parm, "Estimate"]
  es.bj <- coefficients(summary(object))[parm, "Std. Error"]
  t0 <- (bj - ref.value)/es.bj
  n <- length(residuals(object))
  p <- length(coef(object))
  df <- n - p
  # to ensure
  statistic <- ifelse(alternative != 'two.sided', t0, abs(t0))
  PT <- Vectorize(pt)  # to vectorize the pt function
  p.value <- PT(q=statistic, df=rep(df, length(parm)),
                lower.tail=alternative == 'less')
  # to ensure correct p.value for two tails
  p.value <- p.value * 2 ^ (alternative == "two.sided")
  res <- cbind(bj, es.bj, t0, p.value)
  colnames(res) <- c("Estimate", "Std.Err", "t value", "Pr(>t)")
  rownames(res) <- parm
  printCoefmat(res)
}
