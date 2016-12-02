var(rpois(1000, 50))

head(hits)

class(hits[, 'date'])
as.integer(head(hits[,'date']))

mdl <- glm(visits ~ date, family = poisson, data = hits)
summary(mdl)
exp(confint(mdl, 'date'))

which.max(hits[, 'visits'])
hits[704,]
lambda <- mdl$fitted.values[704]
qpois(.95, lambda)

# a model for the proportion of visits from Simply Statistics
mdl2 <- glm(simplystats ~ date, family = poisson, offset = log(visits + 1), data = hits)
qpois(.95, mdl2$fitted.values[704])
