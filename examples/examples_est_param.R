
# Example for NOrmal regression ------------------------
# Here E(y) = mu and Var(y)= sigma
n <- 7
x <- runif(n=n)
y <- rnorm(n=n, mean=-5 + 10 * x, sd=exp(1 + 2 * x))
data1 <- data.frame(y, x)
data1

library(gamlss)
mod1 <- gamlss(y ~ x, sigma.fo=~x, family=NO, data=data1)

# Obtaining the means
m1 <- fitted(mod1, what="mu")
m2 <- est_param(mod1, m=10000, fun="mean")
cbind(m1, m2) # Comparing the estimated means
cor(m1, m2)

# Obtaining the standard deviations
s1 <- fitted(mod1, what="sigma")
s2 <- est_param(mod1, m=10000, fun="sd")
cbind(s1, s2) # Comparing the estimated standard deviations
cor(s1, s2)

# Example for GAmma regression --------------------------
# Here E(y) = mu and Var(y)= sigma^2 mu^2
n <- 7
x <- runif(n=n)
y <- rGA(n=n, mu=exp(-1 + 5 * x), sigma=exp(-1 + 2 * x))
data2 <- data.frame(y, x)
data2

mod2 <- gamlss(y ~ x, sigma.fo=~x, family=GA, data=data2)

summary(mod2)

# Obtaining the means
m1 <- fitted(mod2, what="mu")
m2 <- est_param(mod2, m=10000, fun="mean")
cbind(m1, m2) # Comparing the estimated means
cor(m1, m2)

# Obtaining the standard deviations
s1 <- fitted(mod2, what="sigma") * fitted(mod2, what="mu")
s2 <- est_param(mod2, m=10000, fun="sd")
cbind(s1, s2) # Comparing the estimated standard deviations
cor(s1, s2)

# To predict mean and standard deviation for new observations
new_data <- data.frame(x=c(0.26, 0.52, 0.77))

est_param(mod=mod2, fun="mean", m=10000, data=data2, newdata=new_data)
est_param(mod=mod2, fun="sd"  , m=10000, data=data2, newdata=new_data)

