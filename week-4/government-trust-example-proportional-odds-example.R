## Setup
martin <- read.csv("week-4/data/martin.txt")
Educ <- factor(martin$Educ, labels=c("Less than HS", "High School", "Some college", "College degree"))
Year <- factor(martin$Year)
TrustLG <- factor(martin$TrustLG, labels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

## Simple model
require(MASS)
summary(fit <- polr(TrustLG~Year))
fit0 <- polr(TrustLG~1)
anova(fit0, fit, test="Chisq")
exp(c(coef(fit), confint(fit)))

## Dotplot
df <- as.data.frame(100*prop.table(table(Year, TrustLG),1))
names(df)[3] <- "Pct"
require(lattice)
trellis.par.set(superpose.symbol=list(pch=19, col=c("gray50","red"), cex=1.2))
dotplot(TrustLG~Pct, df, group=Year, auto.key=list(columns=2), xlim=c(-1,1.05*max(df$Pct)),xlab="Percentage")

## Estimated probabilities
New <- data.frame(Year=c("2001", "2011"))
predict(fit, New, type="probs")
prop.table(table(Year, TrustLG),1)

## Multinomial
require(nnet)
y <- relevel(TrustLG, "Disagree")
fit.mn <- multinom(y~Year)
AIC(fit.mn)
exp(coef(fit.mn)[,2])
summary(fit.mn)

## Model selection
AIC(polr(TrustLG~1))
AIC(polr(TrustLG~Year))
AIC(polr(TrustLG~Year+Educ))
AIC(polr(TrustLG~Year*Educ))
EducNum <- as.numeric(Educ)
AIC(polr(TrustLG~Year*EducNum))

## Interaction model
eci <- function(fit, j) c(coef(fit)[j], confint(fit, j))
Ed <- relevel(Educ, "Less than HS")
fit1 <- polr(TrustLG~Year*Ed)
exp(eci(fit1, 1))
Ed <- relevel(Educ, "High School")
fit2 <- polr(TrustLG~Year*Ed)
exp(eci(fit2, 1))
Ed <- relevel(Educ, "Some college")
fit3 <- polr(TrustLG~Year*Ed)
exp(eci(fit3, 1))
Ed <- relevel(Educ, "College degree")
fit4 <- polr(TrustLG~Year*Ed)
exp(eci(fit4, 1))

## Plot
New <- rbind(data.frame(Ed=levels(Educ), Year="2001"),
             data.frame(Ed=levels(Educ), Year="2011"))
p <- predict(fit1, New, type="probs")
col <- c("#FF4E37FF", "#5FA600FF", "#00C1C9FF", "#D63EFFFF")
matplot(100*t(p[New$Year=="2001",]), col=col, lwd=3, lty=1, type="l", xaxt="n", ylim=c(0, 60), ylab="Percent", las=1)
axis(1, at=1:5, labels=gsub(" ", "\n", levels(TrustLG)), tck=0)
mtext("2001")
matplot(100*t(p[New$Year=="2011",]), col=col, lwd=3, lty=1, type="l", xaxt="n", ylim=c(0, 60), ylab="Percent", las=1)
axis(1, at=1:5, labels=gsub(" ", "\n", levels(TrustLG)), tck=0)
mtext("2011")

## Equivalence with wilcoxon
fit0 <- polr(TrustLG~1)
fit <- polr(TrustLG~Year)
anova(fit0, fit, test="Chisq")
wilcox.test(martin$TrustLG~Year)

