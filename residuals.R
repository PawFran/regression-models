    # diamond data example
library(UsingR)
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)

e <- resid(fit)

# obtaining residuals manually
yhat <- predict(fit) # residuals are y - yhat

# compare two ways
max(abs(e - (y - yhat)))


    # estimating residuals variation
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma
# another way
sqrt(sum(resid(fit)^2) / (n - 2))


    # residuals for intercept only(total var) vs liner model with slope
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 20)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g

# Anscombe's example of misleading R^2
data(anscombe);example(anscombe)
