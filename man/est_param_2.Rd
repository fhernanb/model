% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_param_2.R
\name{est_param_2}
\alias{est_param_2}
\title{Estimation of mean and/or variance for models fitted with gamlss2.}
\usage{
est_param_2(mod, fun = "mean", m = 100, data, newdata = NULL, ...)
}
\arguments{
\item{mod}{a gamlss2 object.}

\item{fun}{the estimated parameter, by default is mean but can be 
changed for sd, var or any statistical function.}

\item{m}{a number to indicate the number of observations to simulate, 
by default its values is 100.}

\item{data}{the original data frame used to fit the mod.}

\item{newdata}{a data frame containing new values for the 
explanatory variables.}

\item{...}{additional parameters for "fun" function.}
}
\value{
\code{est_param_2} function returns a vector.
}
\description{
This function estimates parameters like mean, standard deviation for a 
gamlss2 object. This function is very useful when the mean or 
standard deviation do not match with the parameters
\eqn{\mu}, \eqn{\sigma}, \eqn{\nu} and \eqn{\tau}, ..., or some 
function of the parameters \eqn{\mu}, \eqn{\sigma}, \eqn{\nu} and \eqn{\tau}.
}
\details{
The function obtains the fitted values for mu, sigma, nu and tau. 
The functions simulates m observations and then it obtains the mean/fun 
defined by the user.
}
\examples{

# Example for NOrmal regression ------------------------
# Here E(y) = mu and Var(y)= sigma
n <- 7
x <- runif(n=n)
y <- rnorm(n=n, mean=-5 + 10 * x, sd=exp(1 + 2 * x))
data1 <- data.frame(y, x)
data1

library(gamlss2)
mod1 <- gamlss2(y ~ x, sigma.fo=~x, family=NO, data=data1)

# Obtaining the means
m1 <- fitted(mod1, model="mu", type="parameter")
m2 <- est_param_2(mod1, m=10000, fun="mean")
cbind(m1, m2) # Comparing the estimated means
cor(m1, m2)

# Obtaining the standard deviations
s1 <- fitted(mod1, model="sigma", type="parameter")
s2 <- est_param_2(mod1, m=10000, fun="sd")
cbind(s1, s2) # Comparing the estimated standard deviations
cor(s1, s2)

# Example for GAmma regression --------------------------
# Here E(y) = mu and Sd(y) = mu * sigma
n <- 7
x <- runif(n=n)
y <- rGA(n=n, mu=exp(-1 + 5 * x), sigma=exp(-1 + 2 * x))
data2 <- data.frame(y, x)
data2

mod2 <- gamlss2(y ~ x, sigma.fo=~x, family=GA, data=data2)

summary(mod2)

# Obtaining the means
m1 <- fitted(mod2, model="mu", type="parameter")
m2 <- est_param_2(mod2, m=10000, fun="mean")
cbind(m1, m2) # Comparing the estimated means
cor(m1, m2)

# Obtaining the standard deviations
s1 <- fitted(mod2, what="sigma") * fitted(mod2, what="mu")
s2 <- est_param_2(mod2, m=10000, fun="sd")
cbind(s1, s2) # Comparing the estimated standard deviations
cor(s1, s2)

# To predict mean and standard deviation for new observations
new_data <- data.frame(x=c(0.26, 0.52, 0.77))

est_param_2(mod=mod2, fun="mean", m=10000, data=data2, newdata=new_data)
est_param_2(mod=mod2, fun="sd"  , m=10000, data=data2, newdata=new_data)
}
