
###----------------------------------------------------------------
### Fearon & Laitin. First, show predicted probabilities of war
### at two different levels of instability
###---------------------------------------------------------------

#load data stored as .RData file
load("week-2/data/fearon_laitin.RData")

#fit logistic regression with 2 predictor variables
glm.out <- glm(war~instab+lmtnest, data=fearon_laitin, family=binomial(link="logit"))

#Could use predict, or get \hat{pi} by hand for the two cases:
pi0 <- 1/(1+exp(-glm.out$coef[1]-0-glm.out$coef[3]*fearon_laitin$lmtnest))
pi1 <- 1/(1+exp(-glm.out$coef[1]-glm.out$coef[2]-glm.out$coef[3]*fearon_laitin$lmtnest))

#set color scheme
cola <- rgb(80,20,0,10,maxColorValue=255)
colb <- rgb(0,100,100,10,maxColorValue=255)

#store plot colors
plot_colors <- c(rgb(r=0.4,g=0.1,b=0.0),rgb(r=0,g=.5,b=.5) )

#plot pi0
plot(fearon_laitin$lmtnest,pi0, ylim=c(0,.6),ylab="Pr(war)", xlab="mountainous terrain", col=cola, pch=16)
#add pi1
points(fearon_laitin$lmtnest,pi1, col=colb, pch=16)
#add 
abline(v=mean(fearon_laitin$lmtnest),lty=2)
text(mean(fearon_laitin$lmtnest+.3), .4, "mean(lmtn)")

legend("topleft", legend=c("Pr[war|instab=0,lmtn]", "Pr[war|instab=1,lmtn]"), 
       col=plot_colors, pch=16)

#Get difference in mean:
firstdiff_overall <- mean(pi1)-mean(pi0)
mean_lmtn <- mean(fearon_laitin$lmtnest)

#Compare to approach of first fixing the other covariates
#at their mean and then computing 
#tau = Pr(Y=1|T=1,X=mean(X))-Pr(Y=1|T=0,X=mean(X))
firstdiff_fixedmean <- 1/(1+exp(-glm.out$coef[1]-glm.out$coef[2]-glm.out$coef[3]*mean_lmtn))-1/(1+exp(-glm.out$coef[1]-0-glm.out$coef[3]*mean_lmtn))

###----------------------------------------------------------
### Hand-rolled optimization for a logit model
###----------------------------------------------------------

#Logit log-likelihood
loglik=function(par, X, y){  
  #check if there is already an intercept or not
  if (prod(apply(X,2,var))!=0) X=cbind(1,X)  
  #compute likelihoods for each individual (vectorize, so all at once)
  pi_est=1/(1+exp(-1*X%*%par))
  #log likelihood is sum of logs of individual likelihoods
  ll=sum(y*log(pi_est)+(1-y)*log(1-pi_est))
  return(ll)
}


X=as.matrix(fearon_laitin[,c("instab","lmtnest")])
y=fearon_laitin$war

#use optim to maximize likelihood, BFGS procedure
opt.out = optim(par = c(0,0,0), fn = loglik, X=X, y = y,
                method = "BFGS", control = list(fnscale = -1),
                hessian = TRUE)


#See chosen coefficients
opt.out$par

#check that the model matches...yaay!
glm(war~instab+lmtnest, data=fearon_laitin, family=binomial(link="logit"))$coef
