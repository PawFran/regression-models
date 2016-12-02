x <- c(1, 2, 3, 4)
y <- c(-100, -100, 0 , 0)
z <- c(2, 1, 4, 3)

summary(lm(z ~ x + y))$coef

summary(lm(z ~ x))$coef

summary(lm(z ~ y))$coef
