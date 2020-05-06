rm(list = ls())
library(MASS)
library(leaps)
source("http://www.ics.uci.edu/~dgillen/STAT211/Handouts/Stat211Functions.R")
AD <- read.table("http://www.ics.uci.edu/~dgillen/STAT211/Data/studycompletion.csv",
                 sep = ',', header = TRUE)
AD$X <- NULL
AD$rid <- NULL
AD$ptgender <- factor(AD$ptgender)
AD$ptalcohol.hx <- factor(AD$ptalcohol.hx)
AD$ptdrug.hx <- factor(AD$ptdrug.hx)
AD$ptsmoking.hx <- factor(AD$ptsmoking.hx)
AD$ptcvd.hx <- factor(AD$ptcvd.hx)
AD$ptcancer.hx <- factor(AD$ptcancer.hx)
AD$mpmemory <- factor(AD$mpmemory)
AD$mprepeat <- factor(AD$mprepeat)
AD$mphelp <- factor(AD$mphelp)
AD$mpdrive <- factor(AD$mpdrive)
AD$mpsocial <- factor(AD$mpsocial)
AD$mpapplia <- factor(AD$mpapplia)
AD$msmemory <- factor(AD$msmemory)
AD$msrepeat <- factor(AD$msrepeat)
AD$mshelp <- factor(AD$mshelp)
AD$msdrive <- factor(AD$msdrive)
AD$mssocial <- factor(AD$mssocial)
AD$msapplia <- factor(AD$msapplia)
AD$inf.gender <- factor(AD$inf.gender)
AD$inf.ethnc <- factor(AD$inf.ethnc)
AD$inf.live <- factor(AD$inf.live)
AD$inf.dclin <- factor(AD$inf.dclin)
AD$comp.study <- factor(AD$comp.study)
AD$inf.relat <- relevel(AD$inf.relat, ref = "other")
AD$ptgender <- relevel(AD$ptgender, ref = "Male")
AD1 <- AD[AD$comp.study==1,]
AD0 <- AD[AD$comp.study==0,]
summary(AD)
summary(AD1)
summary(AD0)

AD$cds <- rep(0, 644)
AD$cds[AD$cdsum.bl==0] <- 1

fit0 <- glm(comp.study~inf.relat, family = "binomial", data = AD)
fit1 <- glm(comp.study~ptgender+pt.age+pteducat+ptsmoking.hx+ptcvd.hx+ptcancer.hx+cds*inf.relat+inf.educa,
           family = "binomial", data = AD)
linearContrast <- function(model, cov.names, contr.coef,
                           conf.level = 0.95, exp.rslt = FALSE){
  X <- model.matrix(model)
  beta <- model$coefficients
  mu <- fit$family$linkinv(X %*% beta)
  V <- diag(as.vector(model$family$variance(mu)))
  Iinv <- solve(t(X) %*% V %*% X)[cov.names, cov.names]
  est <- sum(contr.coef*beta[cov.names])
  variance <- t(contr.coef) %*% Iinv %*% contr.coef
  se <- sqrt(variance)
  CI <- c(est + se*qnorm((1 - conf.level)/2), 
          est + se*qnorm((1 + conf.level)/2))
  Wald <- est^2 / variance
  Pr <- 1 - pchisq(Wald, 1)
  if(exp.rslt == FALSE) {
    return(list(est = est, CI = CI, se = se, Wald = Wald, Pr = Pr))
  } else {
    return(list(est = exp(est), CI = exp(CI), se = se, Wald = Wald, Pr = Pr))
  }
}


full <- glm(comp.study~ptgender+pt.age+pteducat+ptsmoking.hx+ptcvd.hx+ptcancer.hx+m3total.bl+cdsum.bl+inf.dclin+inf.live+inf.ttim+inf.ethnc+inf.gender+inf.relat+inf.educa,
              family = "binomial", data = AD)
fmla <- full$formula
prdfit0 <- glm(comp.study~1, family = "binomial", data = AD)
full2 <- glm(comp.study~ pt.age + pteducat + m3total.bl + cdsum.bl + inf.dclin,
             family = "binomial", data = AD)

muhat <- fitted( full2 )
pr <- (AD$comp.study - muhat) / (muhat*(1-muhat)) + full2$coef[4]*AD$m3total.bl
plot( AD$m3total.bl, pr, xlab="mMMSE", ylab="Partial Residual" )
sfit <- loess( pr ~ AD$m3total.bl )
idx <- order(sfit$x)
lines( sfit$x[idx], fitted( sfit )[idx], col="red", lwd=2 )
lines( AD$m3total.bl, fitted( lm( pr ~ AD$m3total.bl ) ), col="blue", lwd=2 )

plot( hatvalues(fit1), cooks.distance(fit1), xlab="Leverage",
      ylab="Cooks Distance", ylim=c(0,.3) )
abline( h=qf(.05,length(fit1$coef), fit1$df.residual), col="blue", lwd=2 )
abline( v=2*mean(hatvalues(fit1) ), col="red", lwd=2 )

y <- AD$comp.study
mu <- fitted(full2)
ss <- rep(0, 100)
sf <- rep(0, 100)
AUC <- 0
for(i in 1:100){
c <- i/100
pred <- (sign(mu - c)+1)/2
ss[i] <- sum(pred[y==1])/418
sf[i] <- sum(pred[y==0])/226
}
plot(sf,ss,xlab = "Specificity", ylab = "Sensitivity")
sfit <- loess( ss ~ sf )
idx <- order(sfit$x)
a <- (1:1000)/1000
b <- a
lines( sfit$x[idx], fitted( sfit )[idx], col="red", lwd=2 )
lines( b, a, col="blue", lty=2, lwd=3)
















