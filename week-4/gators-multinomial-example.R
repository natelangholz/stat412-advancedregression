#in the following code we chose the multinom function because it does not require the data to be reshaped (as the mlogit package does) 
#this resides in the nnet library
library(nnet)

gators <- read.csv("week-4/gators.txt")

## Descriptive
table(gators$Food)
table(gators$Lake)
table(gators$Size)
table(gators$Sex)

## Set fish as baseline
Food <- relevel(gators$Food, "fish")
Size <- relevel(gators$Size, "small")

## Some model selection
AIC(multinom(Food~1, trace=FALSE, data = gators))
AIC(multinom(Food~Size, trace=FALSE, data = gators))
AIC(multinom(Food~Size+Lake, trace=FALSE, data = gators))
AIC(multinom(Food~Size+Lake+Sex, trace=FALSE, data = gators))
AIC(multinom(Food~Size*Lake, trace=FALSE, data = gators))
fit <- multinom(Food~Size+Lake, trace=FALSE, data = gators) ## Final model

## Plots of probability
New <- data.frame(Size="large", Lake=levels(gators$Lake))
p <- predict(fit, New, type="prob")
col <- c("#FF4E37FF", "#979B00FF", "#00BE5DFF", "#00AFFFFF", "#FF00FFFF")
matplot(p, xaxt="n", pch=19, col=col, type="o", lty=1, ylab="Pr")
axis(1, at=1:4, labels=levels(gators$Lake))
legend("topright", colnames(p), col=col, pch=19, lwd=1)

New <- data.frame(Lake="Trafford", Size=levels(gators$Size))
p <- predict(fit, New, type="prob")
matplot(p, xaxt="n", pch=19, col=col, type="o", lty=1, ylab="Pr")
axis(1, at=1:2, labels=levels(gators$Size))
legend("topright", colnames(p), col=col, pch=19, lwd=1)

## ORs
Food <- relevel(gators$Food, "invert")
fit <- multinom(Food~Size+Lake, trace=FALSE, data = gators)
exp(cbind(coef(fit)[,2], t(drop(confint(fit, 2)))))

Food <- relevel(gators$Food, "fish")
fit <- multinom(Food~Size+Lake, trace=FALSE,data = gators)
exp(cbind(coef(fit)[,5], t(drop(confint(fit, 5)))))

## Wald p-values
summ <- summary(fit)
z <- summ$coefficients/summ$standard.errors
2*pnorm(-abs(z))

## LR p-values
fit0 <- multinom(Food~Size, trace=FALSE, data = gators)
anova(fit0, fit, test="Chisq")

fit0 <- multinom(Food~Lake, trace=FALSE, data = gators)
anova(fit0, fit, test="Chisq")


