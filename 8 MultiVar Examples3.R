dim(hunger)
948
names(hunger)

fit <- lm(Numeric ~ Year, data = hunger)
summary(fit)$coef

lmF <- lm(Numeric[Sex == 'Female'] ~ Year[Sex == 'Female'], data = hunger)
lmM <- lm(Numeric[hunger$Sex == 'Male'] ~ Year[hunger$Sex == 'Male'], data = hunger)

lmBoth <- lm(Numeric ~ Year + Sex, data = hunger)
summary(lmBoth)

lmInter <- lm(Numeric ~ Year + Sex + Sex*Year, data = hunger)
summary(lmInter)
