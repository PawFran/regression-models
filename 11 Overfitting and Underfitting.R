x1c <- simbias()
apply(x1c, 1, mean)

fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data = swiss)
# null hypothesis is that the added regressors are not significant
anova(fit1, fit3)
deviance(fit3)
d <- deviance(fit3) / 43
n <- (deviance(fit1) - deviance(fit3)) / 2
n / d
pf(n / d, 2, 43, lower.tail = FALSE)

# testing normality of residuals (it's anova's assumption)
shapiro.test(fit3$residuals)
anova(fit1, fit3, fit5, fit6)
