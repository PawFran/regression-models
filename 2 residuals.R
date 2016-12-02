fit <- lm(child ~ parent, galton)
summary(fit)

# mean of residuals should be (close to) zero
mean(fit$residuals)
# correlation of siduals and predictors also shold be close to zero
cov(fit$residuals, galton$parent)

?deviance

ols.ic <- fit$coef[1]
ols.slope <- fit$coef[2]

lhs - rhs
all.equal(lhs, rhs)

# showing that the children's heights is the sum of the variance in the OLS estimates and the variance in the OLS residuals
varChild <- var(galton$child)
varRes <- var(fit$residuals)
varEst <- var(est(ols.slope, ols.ic))
all.equal(varChild, varEst+varRes)


    # earthquakes in california example
efit <- lm(accel ~ mag + dist, attenu)
mean(efit$residuals)
cov(efit$residuals, attenu$mag)
cov(efit$residuals, attenu$dist)
