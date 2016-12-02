# q1
library(MASS)
?shuttle
head(shuttle)
str(shuttle$use)
str(shuttle$wind)
mdl <- glm(use ~ wind, family = binomial, data = shuttle)
summary(mdl)
cf <- summary(mdl)$coef
exp(cf[2, 1])

# q2
mdl2 <- glm(use ~ wind + magn, family = binomial, data = shuttle)
summary(mdl2)
cf2 <- summary(mdl2)$coef
exp(cf2[2, 1])
exp(coef(mdl2))

# q3 


# q4
data("InsectSprays")
head(InsectSprays)
mdl4 <- glm(count ~ spray - 1, family = poisson, data = InsectSprays)
exp(coef(mdl4))
14.500000 / 15.333333

# q6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
