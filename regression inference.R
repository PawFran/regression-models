# fitting linear model. diamond data example
library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat
fit <- lm(y ~ x)
summary(fit)$coefficients

    # getting a confidence interval for regression coeffincients
# intercept
coef <- summary(fit)$coef
coef[1, 1] + c(-1, 1) * qt(.975, df = fit$df) * coef[1, 2]
# slope
coef[2, 1] + c(-1, 1) * qt(.975, df = fit$df) * coef[2, 2]

    # prediction of outcomes
library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g