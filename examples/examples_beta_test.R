# In simple linear regression
mod1 <- lm(dist ~ speed, data=cars)
coef(mod1)

# to test beta_speed = 3 vs beta_speed < 3
beta_test(object=mod1, parm='speed', ref.value=3, alternative='less')

# In multiple linear regression
mod2 <- lm(mpg ~ hp + wt, data=mtcars)

# to test separately
# beta_hp = -0.05 vs beta_hp < -0.05 and
# beta_wt = -3 vs beta_wt != -3
alternative <- c("less", "two.sided")
ref.value <- c(-0.05, -3)
parm <- c("hp", "wt")
beta_test(mod2, alternative, parm, ref.value)