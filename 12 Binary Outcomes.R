ravenData
mdl <- glm(ravenWinNum ~ ravenScore, family = 'binomial', data = ravenData)
lodds <- predict(mdl, data.frame(ravenScore = c(0, 3, 6)))
exp(lodds) / (1 + exp(lodds))
summary(mdl)
exp(confint(mdl))

# linear regression minimizes the variance of the residuals. 
# Deviance extends this idea to generalized linear regression, using (negative) log likelihoods in place of variance
anova(mdl)
qchisq(0.95, 1)
mdl0 <- glm(ravenWinNum ~ 1, binomial, ravenData)
summary(mdl0)
