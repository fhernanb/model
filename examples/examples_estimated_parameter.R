
# Example for NOrmal regression ------------------------
# Here E(y) = mu and Var(y)= sigma
n <- 7
x <- runif(n=n)
y <- rnorm(n=n, mean=-5 + 10 * x, sd=exp(1 + 2 * x))
library(gamlss)
mod <- gamlss(y ~ x, sigma.fo=~x, family=NO)

# Obtaining the means
m1 <- fitted(mod, what="mu")
m2 <- estimated_parameter(mod, m=10000, fun="mean")
cbind(m1, m2) # Comparing the estimated means
cor(m1, m2)

# Obtaining the standard deviations
s1 <- fitted(mod, what="sigma")
s2 <- estimated_parameter(mod, m=10000, fun="sd")
cbind(s1, s2) # Comparing the estimated standard deviations
cor(s1, s2)

# Example for GAmma regression --------------------------
# Here E(y) = mu and Var(y)= sigma^2 mu^2
n <- 7
x <- runif(n=n)
y <- rGA(n=n, mu=exp(-5 + 10 * x), sigma=exp(-1 + 2 * x))
mod <- gamlss(y ~ x, sigma.fo=~x, family=GA)

# Obtaining the means
m1 <- fitted(mod, what="mu")
m2 <- estimated_parameter(mod, m=10000, fun="mean")
cbind(m1, m2) # Comparing the estimated means
cor(m1, m2)

# Obtaining the standard deviations
s1 <- fitted(mod, what="sigma") * fitted(mod, what="mu")
s2 <- estimated_parameter(mod, m=10000, fun="sd")
cbind(s1, s2) # Comparing the estimated standard deviations
cor(s1, s2)

