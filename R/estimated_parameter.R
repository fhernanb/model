#' Estimation parameters for gamlss model.
#' 
#' This function estimates parameters like mean, standard deviation for a gamlss object. This function is very useful when the mean or standard deviation do not match with mu, sigma or some function of mu and/or sigma.
#' 
#' @param mod a gamlss object.
#' @param fun the estimated parameter, by default is mean but can be change for sd or var.
#' @param m a number to indicate the number of observations to simulate, by default its values is 100.
#' 
#' @examples
#' # Example for NOrmal regression ------------------------
#' n <- 50
#' x <- runif(n=n)
#' y <- rnorm(n=n, mean=-5 + 10 * x, sd=exp(1 + 2 * x))
#' library(gamlss)
#' mod <- gamlss(y ~ x, sigma.fo=~x, family=NO)
#' # To explore the coefficients
#' coef(mod, "mu")
#' coef(mod, "sigma")
#' 
#' # Obtaining the means
#' m1 <- fitted(mod, what="mu")
#' m2 <- estimated_parameter(mod, m=10000, fun="mean")
#' 
#' # To explore the first 5 estimated means
#' m1[1:5]
#' 
#' # Obtaining the standard deviations
#' s1 <- fitted(mod, what="sigma")
#' s2 <- estimated_parameter(mod, m=10000, fun="sd")
#' 
#' # Comparing the estimated means
#' cor(m1, m2)
#' # Comparing the estimated standard deviations
#' cor(s1, s2)
#' 
#' # Example for GAmma regression --------------------------
#' n <- 50
#' x <- runif(n=n)
#' y <- rGA(n=n, mu=exp(-5 + 10 * x), sigma=exp(-1 + 2 * x))
#' mod <- gamlss(y ~ x, sigma.fo=~x, family=GA)
#' # To explore the coefficients
#' coef(mod, "mu")
#' coef(mod, "sigma")
#' 
#' # Obtaining the means
#' m1 <- fitted(mod, what="mu")
#' m2 <- estimated_parameter(mod, m=10000, fun="mean")
#' 
#' # To explore the first 5 estimated means
#' m1[1:5]
#' 
#' # Obtaining the standard deviations
#' s1 <- fitted(mod, what="sigma")
#' s2 <- estimated_parameter(mod, m=10000, fun="sd")
#' 
#' # Comparing the estimated means
#' cor(m1, m2)
#' # Comparing the estimated standard deviations
#' # in GA parameterization standard devition = mu * sigma
#' cor(s1*m1, s2)
#' 
#' @return \code{estimated_parameter} function returns a vector.
#' 
#' @details The function obtains the fitted values for mu, sigma, nu and tau. The functions simulates m observations and then it obtains the mean/fun defined by the user.
#' 
#' @import gamlss
#' @export
estimated_parameter <- function(mod, fun="mean", m=100) {
  stopifnot(class(mod)[1] == 'gamlss')
  aux <- paste0("mod$", mod$parameters, ".fv")
  aux <- paste0("cbind(", paste(aux, sep="", collapse=", "), ")")
  fit <- eval(parse(text=aux))
  generator <- function(x) {
    aux <- paste0("r", mod$family[1], "(n=m, ", paste(x, sep="", collapse=", "), ")")
    eval(parse(text=aux))
  }
  x <- t(apply(fit, 1, generator))
  return(apply(x, 1, fun))
}
