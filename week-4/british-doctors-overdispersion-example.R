
## British doctors 

britdoc <- read.csv("week-4/data/britdoc.txt")
PY <- britdoc$PersonYears/1000

## No interaction
fit <- glm(Deaths~Age+Smoking, britdoc, offset=log(PY), family=poisson)
New <- data.frame(britdoc, PY=1)
p <- predict(fit, New, type="response")
matrix(p, 5, 2, dimnames=list(levels(britdoc$Age), levels(britdoc$Smoking)))

## Interaction
fit2 <- glm(Deaths~Age*Smoking, britdoc, offset=log(PY), family=poisson)
anova(fit2, fit, test="Chisq")
AIC(fit)
AIC(fit2)
p <- predict(fit2, New, type="response")
R <- matrix(p, 5, 2, dimnames=list(levels(britdoc$Age), levels(britdoc$Smoking)))
R <- cbind(R, RR=R[,1]/R[,2])
R

####################
## Overdispersion 

aids <- read.csv("week-3/aids.txt")
library(visreg)
library(MASS)

##poisson
fit <- glm(Cases~Year, aids, family=poisson)
visreg(fit, scale="response", ylim=c(0, 350), partial=FALSE, ylab="Cases")
points(aids$Year,aids$Cases,pch=19, cex=0.7)

#quasipoisson
fit.qp <- glm(Cases~Year, aids, family=quasipoisson)
visreg(fit.qp, scale="response", ylim=c(0, 350), partial=FALSE, ylab="Cases")
points(aids$Year,aids$Cases,pch=19, cex=0.7)

## Negative binomial: Mean-variance relationship
fit.nb <- glm.nb(Cases~Year, aids)
l <- seq(0,300,len=101)
plot(l,l,type="l",xlab=expression(lambda),ylab="Variance",lwd=3,col="gray",ylim=range(l+l^2/fit.nb$theta), las=1)
lines(l,l+l^2/fit.nb$theta,lwd=3,col="red")
legend("topleft", legend=c("Poisson","Negative Binomial"),lwd=3,col=c("gray","red"), ncol=2)

fit <- glm(Cases~Year, Data, family=poisson)
visreg(fit, scale="response", ylim=c(0, 350), partial=FALSE, ylab="Cases", main="Poisson")
points(aids$Year,aids$Cases,pch=19, cex=0.7)

visreg(fit.qp, scale="response", ylim=c(0, 350), partial=FALSE, ylab="Cases", main="Quasipoisson")
points(aids$Year,aids$Cases,pch=19, cex=0.7)

visreg(fit.nb, scale="response", ylim=c(0, 350), partial=FALSE, ylab="Cases", main="Negative binomial")
points(aids$Year,aids$Cases,pch=19, cex=0.7)


fit <- glm.nb(Cases~Year+I(Year^2), data=aids)

