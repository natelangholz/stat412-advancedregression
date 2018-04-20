## Read in the data
wells <- read.csv("week-2/data/wells.txt")

## Models under consideration
fit0 <- glm(Switch~1, family=binomial, data=wells)
fit <- vector("list",3)
summary(fit[[1]] <- glm(Switch~Dist, family=binomial, data=wells))
summary(fit[[2]] <- glm(Switch~Dist+Arsenic, family=binomial, data=wells))
summary(fit[[3]] <- glm(Switch~., family=binomial, data=wells))

## compare r2 and DEadj
r2 <- R2 <- R2adj <- DE <- DEadj <- numeric(length(fit))
y <- wells$Switch
for (i in 1:length(fit)) {
  r2[i] <- cor(y, fit[[i]]$fitted)^2
  R2[i] <- 1-crossprod(y-fit[[i]]$fitted)/crossprod(y-mean(y))
  R2adj[i] <- 1-crossprod(y-fit[[i]]$fitted)/fit[[i]]$df.residual/(crossprod(y-mean(y))/fit[[i]]$df.null)
  DE[i] <- 1-fit[[i]]$deviance/fit[[i]]$null.deviance
  DEadj[i] <- 1-fit[[i]]$deviance/fit[[i]]$df.residual/(fit[[i]]$null.deviance/fit[[i]]$df.null)
}
rbind(r2,R2,R2adj,DE,DEadj)

#Low values for R2 and deviance explained are fairly common in health behavior
#studies such as this one

## confusion matrix
table(fit[[1]]$fitted>0.5, y)
table(fit[[3]]$fitted>0.5, y)

#use caret confusionMatrix function
library(caret)
confusionMatrix(round(fit[[1]]$fitted),fit[[1]]$y)
confusionMatrix(round(fit[[1]]$fitted),fit[[3]]$y)


## ROC
require(ROCR)
pred <- perf <- vector("list",3)
for (i in 1:3) pred[[i]] <- prediction(fit[[i]]$fitted, y)
for (i in 1:3) perf[[i]] <- performance(pred[[i]], measure="tpr", x.measure="fpr")
for (i in 1:3) print(performance(pred[[i]], measure="auc")@y.values[[1]]) ## Area under curve
col <- c("#FF4E37FF", "#00B500FF", "#008DFFFF")
plot(perf[[1]], lwd=2, col=col[1])
plot(perf[[2]], add=TRUE, col=col[2], lwd=2)
plot(perf[[3]], add=TRUE, col=col[3], lwd=2)
legend("topleft", col=col, legend=paste("Model", 1:3), lwd=2)

## AIC and BIC
AIC(fit[[3]]) ## Basic usage
sapply(fit, AIC)
B <- sapply(fit, BIC)
BB <- B-min(B) ## Otherwise exp(B) is too large for the computer to handle
exp(-0.5*BB)/sum(exp(-0.5*BB))

#################################################################
ilogit <- binomial()$linkinv

## Descriptive
prop.table(table(wells$Switch))
prop.table(table(wells$Community))
hist(wells$Arsenic, breaks=50, col="gray", border="white")
hist(wells$Dist, breaks=50, col="gray", border="white")
barplot(table(wells$Educ), border="white", xlab="Education")

## Descriptive vs. Switch
prop.table(table(wells$Community, wells$Switch),1)
boxplot(Arsenic~Switch, col="gray", las=1, ylab="Arsenic", xlab="Switch",data = wells)
boxplot(Dist~Switch, col="gray", las=1, ylab="Distance", xlab="Switch",data = wells)
boxplot(Educ~Switch, col="gray", las=1, ylab="Education", xlab="Switch",data = wells)

library(dplyr)
library(magrittr)
## New variables
wells %<>% mutate(cDist = (Dist-mean(Dist))/100,
                     cArsn = (Arsenic-mean(Arsenic))/2,
                     cEduc = (Educ-mean(Educ))/8)
)

## Additive model
summary(fit <- glm(Switch~cArsn+cDist+cEduc+Community, family=binomial,data = wells))

## Compare to unadjusted odds ratios
adj <- coef(fit)[-1]
uadj <- c(coef(glm(Switch~cArsn, family=binomial,data = wells))[2],
          coef(glm(Switch~cDist, family=binomial,data = wells))[2],
          coef(glm(Switch~cEduc, family=binomial,data = wells))[2],
          coef(glm(Switch~Community, family=binomial,data = wells))[2])
exp(cbind(uadj, adj))
cor(wells$Arsenic, wells$Dist)

## Get rid of Community
summary(fit <- glm(Switch~cArsn+cDist+cEduc, family=binomial,data = wells))

## Plot
library(visreg)
par(mfrow=c(1,3))
fit <- glm(Switch~Arsenic+Dist+Educ, family=binomial,data = wells)
visreg(fit, scale="response", partial=FALSE, ylim=c(0,1), ylab="Pr(Switch)")

## Transformations
wells %<>% mutate(EduCat = cut(Educ, c(-1, 0, 5, 10, 18), labels=c("0", "1-5", "6-10", "11+")))
summary(fit <- glm(Switch~cArsn+cDist+cEduc, family=binomial,data = wells))        ## AIC: 3918
summary(fit <- glm(Switch~log(Arsenic)+cDist+cEduc, family=binomial,data = wells)) ## AIC: 3886
summary(fit <- glm(Switch~cArsn+log(Dist)+cEduc, family=binomial,data = wells))    ## AIC: 3951
summary(fit <- glm(Switch~cArsn+Dist+EduCat, family=binomial,data = wells))        ## AIC: 3910

## New additive model
wells %<>% mutate(clArsn = log(Arsenic)-mean(log(Arsenic)))
summary(fit <- glm(Switch~clArsn+Dist+EduCat, family=binomial, data = wells))       ## AIC: 3878

## Plot
fit <- glm(Switch~log(Arsenic)+Dist+EduCat, family=binomial, data = wells)
visreg(fit, scale="response", partial=FALSE, ylim=c(0,1), ylab="Pr(Switch)")

## Interactions
summary(fit <- glm(Switch~clArsn*cDist+EduCat, family=binomial, data = wells))      ## AIC: 3879    
summary(fit <- glm(Switch~clArsn*EduCat+cDist, family=binomial, data = wells))      ## AIC: 3878
summary(fit <- glm(Switch~clArsn+cDist*EduCat, family=binomial, data = wells))      ## AIC: 3860

## Plot
fit <- glm(Switch~Dist*EduCat + log(Arsenic), family=binomial, data = wells)
visreg(fit, "Arsenic", scale="response", partial=FALSE, ylim=c(0,1), ylab="Pr(Switch)")
visreg(fit, "Dist", "EduCat", scale="response", partial=FALSE, ylim=c(0,1), ylab="Pr(Switch)")

#arsenic and distance have been centered and a one-unit 
#change in each corresponds approximately to a 2-SD difference

## Summarization
tab <- table(fit$fitted > .5, wells$Switch)
sum(tab[2:3])/sum(tab)                 ## 36.8% Error rate
## 42.5% Error rate for null model
1-fit$deviance/fit$null.deviance       ## 7% Null deviance explained

## The march of progress for AIC and BIC
fit.list <- list(glm(Switch~1, family=binomial, data = wells),
                 glm(Switch~Dist+Arsenic+Educ+Community, family=binomial, data = wells),
                 glm(Switch~Dist+Arsenic+Educ, family=binomial, data = wells),
                 glm(Switch~Dist+log(Arsenic)+EduCat, family=binomial, data = wells),
                 glm(Switch~Dist*EduCat+log(Arsenic), family=binomial, data = wells))
plot(sapply(fit.list, AIC), pch=19, type="o", xaxt="n", xlab="", ylab="AIC")
lab <- c("Null model", "Basic\nadditive", "Removed\ncommunity", "Transformations", "Interaction")
axis(1, at=1:5, labels=lab, tck=0)
plot(sapply(fit.list, BIC), pch=19, type="o", xaxt="n", xlab="", ylab="BIC")
axis(1, at=1:5, labels=lab, tck=0)
