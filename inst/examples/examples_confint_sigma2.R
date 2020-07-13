# Example for simple linear regression
mod <- lm(dist ~ speed, data=cars)
confint_sigma2(object=mod, level=0.95)
confint_sigma2(object=mod, level=0.80)

# Example for multiple linear regression
mod <- lm(mpg ~ wt + hp, data=mtcars)
confint_sigma2(object=mod, level=0.95)
confint_sigma2(object=mod, level=0.80)
