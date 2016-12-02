dim(InsectSprays)
head(InsectSprays, 15)

sA

summary(InsectSprays[, 2])

sapply(InsectSprays, class)

fit <- lm(count ~ spray, data = InsectSprays)
summary(fit)$coef

est <- summary(fit)$coef[, 1]

mean(sA)
mean(sB)

nfit <- lm(count ~ spray - 1, data = InsectSprays)
summary(nfit)$coef

spray2 <- relevel(InsectSprays$spray, 'C')
fit2 <- lm(count ~ spray2, data = InsectSprays)
summary(fit2)$coef

mean(sC)

(fit$coef[2] - fit$coef[3]) / 1.6011
