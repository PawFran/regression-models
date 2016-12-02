# q1 -  the adjusted estimate for the expected change in mpg 
# comparing 8 cylinders to 4
data(mtcars)
names(mtcars)
str(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)

# q2 - compare the effect of 8 versus 4 cylinders on mpg for the adjusted 
# and unadjusted by weight models
fitno <- lm(mpg ~ factor(cyl), data = mtcars) 
summary(fitno)$coef
summary(fit)$coef

# q3
# one method
fit_inter <- lm(mpg ~ factor(cyl) * wt, data = mtcars)
anova(fit, fit_inter, test = "Chisq")
# another
library(lmtest)
summary(fit)$adj.r.squared
summary(fit_inter)$adj.r.squared
lrtest(fit, fit_inter)
# P-value > 0.05, fail to reject, interaction terms may not be necessary

# q4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# q5 - Give the hat diagonal for the most influential point
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
?hatvalues
fit5 <- lm(y ~ x)
hatvalues(fit5)

# q6 - Give the slope dfbeta for the point with the highest hat value
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit6 <- lm(y ~ x)
dfbetas(fit6)
