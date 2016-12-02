fit<- lm(y ~ x, out2)

# residuals versus fitted values
plot(fit, which = 1)

fitno <- lm(y ~ x, out2[-1, ])
plot(fitno, which = 1)

coef(fit) - coef(fitno)
head(dfbeta(fit)) # the same but for every row in the data


resno <- out2[1, "y"] - predict(fitno, out2[1, ])
1 - resid(fit)[1] / resno
head(hatvalues(fit))

# standarized residuals
sigma <- sqrt(deviance(fit) / df.residual(fit))
rstd <- resid(fit) / (sigma * sqrt(1 - hatvalues(fit)))
head(cbind(rstd, rstandard(fit)))
plot(fit, which=3)

plot(fit, which = 2)

# studentized residuals
sigma1 <- sqrt(deviance(fitno) / df.residual(fitno))
resid(fit)[1] / (sigma1 * sqrt(1-hatvalues(fit)[1]))
head(rstudent(fit))

# cook's distance
dy <- predict(fitno, out2) - predict(fit, out2)
sum(dy^2) / (2*sigma^2)
plot(fit, which=5)
