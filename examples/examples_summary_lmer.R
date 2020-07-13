\dontrun{
require(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1) # Usual table without p values
summary_lmer(fm1) # Table with p values
}