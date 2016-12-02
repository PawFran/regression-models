# q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)

# q2
summary(fit)$sigma

# q3 - In the mtcars data set, 
# fit a linear regression model of weight (predictor) 
# on mpg (outcome). Get a 95% confidence interval for 
# the expected mpg at the average weight. 
# What is the lower endpoint?
data("mtcars")
x <- mtcars$wt
y <- mtcars$mpg
fit2 <- lm(y ~ x)
summary(fit2)

predict(fit2, newdata = data.frame(x = mean(x)), interval = "confidence")

coef <- summary(fit2)$coef
x0 <- mean(mtcars$wt)
yhat <- coef[1, 1] + coef[2, 1] * x0
yhat

sigma <- summary(fit)$sigma
n <- length(mtcars$mpg)
se <- sigma * sqrt(1 / n)

yhat + c(-1, 1) * qt(.975, df = fit2$df) * summary(fit2)$sigma / sqrt(n) # why not se ?



# solution from the internet
data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_car <- lm(y ~ x)
yhat <- fit_car$coef[1] + fit_car$coef[2] * mean(x)
yhat + c(-1, 1) * qt(.975, df = fit_car$df) * summary(fit_car)$sigma / sqrt(length(y))

predict(fit_car, newdata = data.frame(x = mean(x)), interval = ("confidence"))

# q5 - A new car is coming weighing 3000 pounds. 
# Construct a 95% prediction interval for its mpg. 
# What is the upper endpoint?
predict(fit2, newdata = data.frame(x = 3), interval = "prediction")

# q6 - A “short” ton is defined as 2,000 lbs. 
# Construct a 95% confidence interval for the 
# expected change in mpg per 1 short ton increase 
# in weight. Give the lower endpoint.
fit_car2 <- lm(y ~ I(x/2))
coef <- summary(fit_car2)$coef
coef[2, 1] + c(-1, 1) * qt(.975, df = fit_car2$df) * coef[2, 2]

# q9 - About what is the ratio of the 
# sum of the squared errors, ∑i=1n(Yi−Y^i)2 when 
# comparing a model with just an intercept (denominator) 
# to the model with the intercept and slope (numerator)?
denominator <- var(mtcars$mpg) * (length(mtcars$mpg) - 1)

y <- mtcars$mpg
denominator2 <- sum((y - mean(y)) ^ 2)

numerator <- sum(fit2$residuals ^ 2)

denominator
numerator
numerator / denominator
numerator / denominator2
