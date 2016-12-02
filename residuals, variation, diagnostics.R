data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ ., data = swiss)
plot(fit)

?influence.measures

# simulation examples
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))

fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3) # change in slope
round(dfbetas(fit)[1 : 10, 1], 3) # change in intercept
round(dffits(fit)[1 : 10], 3)   # change in predicted response
round(cooks.distance(fit)[1 : 10], 3) # overall change in coefficients

round(hatvalues(fit)[1 : 10], 3)
sum(hatvalues(fit))

# simulation demonstrating variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n);
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2],
      coef(lm(y ~ x1 + x2))[2],
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

# swiss data and variance inflation
library(car)
fit <- lm(Fertility ~ ., data = swiss)
vif(fit)
sqrt(vif(fit))


# nested model testing and anova
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)