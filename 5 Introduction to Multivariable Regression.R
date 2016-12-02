ones <- rep(1, nrow(galton))
lm(child ~ ones + parent - 1, galton)
lm(child ~ parent, galton)

lm(child ~ 1, galton)

head(trees)

fit <- lm(Volume ~ Girth + Height + Constant - 1, trees)

trees2 <- eliminate("Girth", trees)
head(trees2)

fit2 <- lm(Volume ~ Height + Constant - 1, trees2)
lapply(list(fit, fit2), coef)
