
library(pscl)

NMES1988 <- read.csv('week-4/data/NMES1988.txt')
nmes <- NMES1988[, c('visits','hospital','health','chronic','gender','school','insurance')]

plot(table(nmes$visits), ylab = 'Count',xlab = 'Visits')

sum(nmes$visits < 1)

sum(nmes$visits > 50)

fit1 <- glm(visits ~ ., data = nmes, family = "poisson")
fit.nb <- glm.nb(visits ~ ., data = nmes)

mu <- predict(fit1, type = "response")
exp <- sum(dpois(x = 0, lambda = mu))
round(exp)

table(round(exp(mu)))

theta <- 1.2066
mu.nb <- predict(fit.nb , type = "response")
exp <-sum(dnbinom(x = 0, size = theta,mu = mu.nb))
round(exp)


sum(nmes$visits < 1)

fit.hurdle <- hurdle(visits ~ ., data = nmes)
# same as this:
# fit.hurdle <- hurdle(visits ~ ., data = nmes, dist = "poisson", zero.dist = "binomial")

summary(fit.hurdle)

sum(predict(fit.hurdle, type = "prob")[,1])



# First 5 expected counts
predict(fit.hurdle, type = "response")[1:5]

# ratio of non-zero probabilities (1 - type = 'prob' 0 prediction)
predict(fit.hurdle, type = "zero")[1:5]

# mean for untruncated process
predict(fit.hurdle, type = "count")[1:5]

# multiply ratio and mean
predict(fit.hurdle, type = "zero")[1:5] * predict(fit.hurdle, type = "count")[1:5]

# equals hurdle model expected count
predict(fit.hurdle, type = "response")[1:5]

hist(round(predict(fit.hurdle, type = "prob"))*predict(fit.hurdle, type = "response"))

#install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)
rootogram(fit1, max = 80)
rootogram(fit.nb, max = 80)
rootogram(fit.hurdle, max = 80) # fit up to count 80



fit.hurdle.nb <- hurdle(visits ~ ., data = nmes, dist = "negbin")

AIC(fit1)
AIC(fit.nb)
AIC(fit.hurdle)
AIC(fit.hurdle.nb) # lower is better

rootogram(fit.hurdle.nb, max = 80) # fit up to count 80

fit.hurdle.nb2 <- hurdle(visits ~ . | gender + insurance, data = nmes, dist = "negbin")

AIC(fit.hurdle.nb2)
