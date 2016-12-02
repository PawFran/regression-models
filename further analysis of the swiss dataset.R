 library(datasets); data(swiss)
 head(swiss)
 
 # create a binary variable: Catholic majority (=1) or not (=0)
 library(dplyr); library(ggplot2)
 swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))
 head(swiss)
 g <- ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
 g <- g + geom_point(size = 6, colour = 'black') + geom_point(size = 4)
 g <- g + xlab("% in agriculture") + ylab("Fertility")
 g 
 
 # model with Agriculture only
 fit1 <- lm(Fertility ~ Agriculture, data = swiss)
 summary(fit1)$coef
 
 # model with factor variable
 fit2 <- lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
 summary(fit2)$coef 
 # ChatholicBin is expected change in intercept between non-Catholic and Catholic
 
 # model with factor variable to obtain both change in intercept and slope
 fit3 <- lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
 summary(fit3)$coef 
 
 
 # plots
 with(swiss, plot(Agriculture, Fertility))
 abline(fit1, col = 'red')
 
 abline(fit2, col = 'blue')
 abline(a = summary(fit2)$coef[1, 1] + summary(fit2)$coef[3, 1], b = summary(fit2)$coef[2, 1], col = 'yellow')
 
 abline(fit3, col = 'purple')
 abline(a = summary(fit3)$coef[1, 1] + summary(fit3)$coef[3, 1], b = summary(fit3)$coef[2, 1] + summary(fit3)$coef[4, 1], col = 'black')
 
 