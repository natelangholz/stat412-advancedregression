## Read in Data 
aids <- read.csv("week-3/aids.txt")

## Descriptive
plot(aids$Year,aids$Cases,pch=19,ylim=c(0,300))

## Fit basic model
fit <- glm(Cases~Year, data=aids, family=poisson,data = aids)
summary(fit)
confint(fit)
fit0 <- glm(Cases~1, data=aids, family=poisson ,data = aids)
anova(fit0, fit, test="Chisq")

## Visualizing the model
library(visreg)
visreg(fit, partial=FALSE, ylab="Linear predictor")
visreg(fit, partial=FALSE, scale="response", ylim=c(0, 350), ylab="Cases")
points(aids$Year, aids$Cases, pch=19, cex=0.7)
visreg(fit, ylab="Linear predictor") ## Suggests a quadratic model

## Re-centering at 1981
aids %<>% mutate(cYear = Year-1981)
summary(glm(Cases~cYear, family=poisson,data = aids))

## Diagnostics
df <- data.frame(aids, Leverage=hatvalues(fit), Cook=cooks.distance(fit), db=dfbeta(fit)[,"Year"], pi=fit$fitted.values, d=rstudent(fit))
xyplot(Leverage~Year,df,type="h",lwd=2,ylim=c(-0.02,max(df$Leverage)+.02))
xyplot(Cook~Year,df,type="h",lwd=2,ylab="Cook's distance")
xyplot(db~Year,df,type="h",lwd=2,ylab=expression(Delta[beta]))
with(df, plot(pi, d, pch=19, cex=3*Leverage/max(Leverage), xlab=expression(lambda), ylab=expression(d*"(Studentized deleted)"), las=1))
with(df, plot(pi, d^2, pch=19, cex=3*Leverage/max(Leverage), xlab=expression(lambda), ylab=expression(d^2*"(Studentized deleted)"), las=1))
abline(h=qchisq(.975,1),col="red")

## Predictive power
1-crossprod(fit$y-fit$fitted)/crossprod(fit$y-mean(fit$y))
1-crossprod(fit$y-fit$fitted)/fit$df.residual/(crossprod(fit$y-mean(fit$y))/fit$df.null)
1-fit$deviance/fit$null.deviance
1-fit$deviance/fit$df.residual/(fit$null.deviance/fit$df.null)

## Quadratic model
fit <- glm(Cases~Year+I(Year^2), data=aids, family=poisson)
visreg(fit, partial=FALSE, ylab="Linear predictor")
visreg(fit, partial=FALSE, scale="response", ylim=c(0, 350), ylab="Cases")
points(Year, Cases, pch=19, cex=0.7)

## Residuals/diagnostics for quadratic model
df <- data.frame(aids, Leverage=hatvalues(fit), Cook=cooks.distance(fit), db=dfbeta(fit)[,"Year"], pi=fit$fitted.values, d=rstudent(fit))
xyplot(Cook~Year,df,type="h",lwd=2,ylab="Cook's distance")
xyplot(db~Year,df,type="h",lwd=2,ylab=expression(Delta[beta]))
with(df, plot(pi, d, pch=19, cex=3*Leverage/max(Leverage), xlab=expression(lambda), ylab=expression(d*"(Studentized deleted)"), las=1))
with(df, plot(pi, d^2, pch=19, cex=3*Leverage/max(Leverage), xlab=expression(lambda), ylab=expression(d^2*"(Studentized deleted)"), las=1))
abline(h=qchisq(.975,1),col="red") ## Off the chart