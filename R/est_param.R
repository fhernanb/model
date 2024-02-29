#' Estimation of mean and/or variance for models fitted with gamlss.
#' 
#' This function estimates parameters like mean, standard deviation for a gamlss object. This function is very useful when the mean or standard deviation do not match with mu, sigma or some function of mu and/or sigma.
#' 
#' @param mod a gamlss object.
#' @param fun the estimated parameter, by default is mean but can be change for sd or var.
#' @param m a number to indicate the number of observations to simulate, by default its values is 100.
#' @param data the original data frame used to fit mod.
#' @param newdata a data frame containing new values for the explanatory variables.
#' 
#' @example examples/examples_est_param.R
#' @return \code{est_param} function returns a vector.
#' 
#' @details The function obtains the fitted values for mu, sigma, nu and tau. The functions simulates m observations and then it obtains the mean/fun defined by the user.
#' 
#' @importFrom stats predict
#' @export
est_param <- function(mod, fun="mean", m=100, data, newdata=NULL) {
  stopifnot(class(mod)[1] == "gamlss")
  
  if (is.null(newdata)) {
    aux <- paste0("mod$", mod$parameters, ".fv")
    aux <- paste0("cbind(", paste(aux, sep="", collapse=", "), ")")
    fit <- eval(parse(text=aux))
  }
  else {
    fit <- NULL
    for (i in mod$parameters) {
      temp <- predict(mod, what=i, data=data, newdata=newdata, type="response")
      fit <- cbind(fit, temp)
    }
  }
  # Auxiliar function to simulate
  generator <- function(x) {
    aux <- paste0("r", mod$family[1], "(n=m, ", paste(x, sep="", collapse=", "), ")")
    eval(parse(text=aux))
  }
  x <- t(apply(fit, 1, generator))
  return(apply(x, 1, fun))
}
