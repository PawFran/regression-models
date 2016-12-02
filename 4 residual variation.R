fit <- lm(child ~ parent, galton)
sqrt(sum(fit$residuals ^ 2) / (n - 2))

summary(fit)$sigma

sqrt(deviance(fit)/(n-2))

mu <- mean(galton$child)

sTot <- sum((galton$child - mu)^2)
sRes <- deviance(fit)
1 - sRes / sTot

summary(fit)$r.squared

cor(galton$child, galton$parent) ^ 2
