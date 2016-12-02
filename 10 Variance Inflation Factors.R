rgp1()
rgp2()

head(swiss)
mdl <- lm(Fertility ~ ., data = swiss)
vif(mdl) # probably most of the variance inflation for Education is due to including Examination (they're probably correlated)
mdl2 <- lm(Fertility ~ . - Examination, data = swiss)
vif(mdl2) # now Education has less vif

