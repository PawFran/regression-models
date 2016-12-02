# plotting galton's data
plot(child ~ parent, galton)

# adding noise
plot(jitter(child, 4) ~ parent, galton)

# regression line
regrline <- lm(child ~ parent, galton)
abline(regrline, lwd = 3, col = 'red')
summary(regrline)
