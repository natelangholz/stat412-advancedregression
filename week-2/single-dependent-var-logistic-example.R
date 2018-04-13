# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/nes

#install.packages('foreign')
library(foreign)


## Read the data
brdata <- read.dta("week-2/data/nes5200_processed_voters_realideo.dta",convert.factors=F)

# Clean the data
brdata <- brdata[is.na(brdata$black)==FALSE&is.na(brdata$female)==FALSE&is.na(brdata$educ1)==FALSE
                 &is.na(brdata$age)==FALSE&is.na(brdata$income)==FALSE&is.na(brdata$state)==FALSE,]
kept.cases <- 1952:2000
matched.cases <- match(brdata$year, kept.cases)
keep <- !is.na(matched.cases)
data <- brdata[keep,]
plotyear <- unique(sort(data$year))
year.new <- match(data$year,unique(data$year))
n.year <- length(unique(data$year))
income.new <-data$income-3
age.new <- (data$age-mean(data$age))/10
y <- data$rep_pres_intent
data <- cbind(data, year.new, income.new, age.new, y)
nes.year <- data[,"year"]
age.discrete <- as.numeric (cut (data[,"age"], c(0,29.5, 44.5, 64.5, 200)))
race.adj <- ifelse (data[,"race"]>=3, 1.5, data[,"race"])
data <- cbind (data, age.discrete, race.adj)

female <- data[,"gender"] - 1
black <- ifelse (data[,"race"]==2, 1, 0)
rvote <- ifelse (data[,"presvote"]==1, 0, ifelse(data[,"presvote"]==2, 1, NA))

region.codes <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,
                  1,3,2,3,3,4,1,3,4,1,2,4)


yr <- '1992'
ok <- nes.year==yr & data$presvote<3
vote <- data$presvote[ok] - 1
income <- data$income[ok]


#model estimation
fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
summary(fit.1)


#inverse logit function
invlogit<-function (x) {
  1/(1 + exp(-x))
}

#
plot(seq(-5,5,.2),invlogit(seq(-5,5,.2)),type = "l", xlab = 'x', ylab = 'invlogit')


#logistic regression estimating the prob of supporting Bush
curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), 1, 5, ylim=c(-.01,1.01),
       xlim=c(-2,8), xaxt="n", xaxs="i", mgp=c(2,.5,0),
       ylab="Pr (Republican vote)", xlab="Income", lwd=4)

curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), -2, 8, lwd=.5, add=T)
axis (1, 1:5, mgp=c(2,.5,0))
mtext ("(poor)", 1, 1.5, at=1, adj=.5)
mtext ("(rich)", 1, 1.5, at=5, adj=.5)
points (jitter (income, .5), jitter (vote, .08), pch=20, cex=.1)


## Evaluation at the mean
invlogit(coef(fit.1)[1] + coef(fit.1)[2]*mean(income, na.rm=T))


## Displaying the results of several logistic regressions
income.year <- NULL
income.coef <- NULL
income.se <- NULL
for (yr in seq(1952,2000,4)){
  ok <- nes.year==yr & data$presvote<3
  vote <- data$presvote[ok] - 1
  income <- data$income[ok]
  fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
  income.year <- c (income.year, yr)
  income.coef <- c (income.coef, fit.1$coef[2])
  income.se <- c (income.se, se.coef(fit.1)[2])
}

plot (income.year, income.coef, xlim=c(1950,2000), ylim=range(income.coef+income.se, 
                                                              income.coef-income.se), mgp=c(2,.5,0), pch=20, ylab="Coefficient of income", xlab="Year")
for (i in 1:length(income.year)){
  lines (rep(income.year[i],2), income.coef[i]+income.se[i]*c(-1,1), lwd=.5)
}
abline (0,0,lwd=.5, lty=2)

