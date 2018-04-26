
library(pscl)

NMES1988 <- read.csv('week-4/data/NMES1988.txt')
nmes <- NMES1988[, c('visits','hospital','health','chronic','gender','school','insurance')]

plot(table(nmes$visits))

sum(nmes$visits < 1)

sum(nmes$visits > 50)

mod1 <- glm(visits ~ ., data = nmes, family = "poisson")

mu <- predict(mod1, type = "response")
exp <- sum(dpois(x = 0, lambda = mu))
round(exp)
sum(nmes$visits < 1)

mod.hurdle <- hurdle(visits ~ ., data = nmes)
# same as this:
# mod.hurdle <- hurdle(visits ~ ., data = nmes, dist = "poisson", zero.dist = "binomial")

summary(mod.hurdle)

sum(predict(mod.hurdle, type = "prob")[,1])

# First 5 expected counts
predict(mod.hurdle, type = "response")[1:5]

# ratio of non-zero probabilities
predict(mod.hurdle, type = "zero")[1:5]

# mean for untruncated process
predict(mod.hurdle, type = "count")[1:5]

# multiply ratio and mean
predict(mod.hurdle, type = "zero")[1:5] * predict(mod.hurdle, type = "count")[1:5]

# equals hurdle model expected count
predict(mod.hurdle, type = "response")[1:5]

mod.hurdle.nb <- hurdle(visits ~ ., data = nmes, dist = "negbin")

AIC(mod.hurdle)
AIC(mod.hurdle.nb) # lower is better

mod.hurdle.nb2 <- hurdle(visits ~ . | gender + insurance, data = nmes, dist = "negbin")

AIC(mod.hurdle.nb2)
