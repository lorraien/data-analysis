library(tidyverse)
library(nlme)
library(ggplot2)
#library(tidyverse)
efw<-read.csv("/Users/jing/Desktop/2020 winter/Stat 275/FEV.csv",sep=',',header=T)
attach(efw)
length(unique(fev$subid))   # 100 subjects
# How many measurements per patient?
sort(tapply(efw$EFW, efw$subid, length))
n.obs<-unlist(lapply(split(efw$subid,efw$subid), length))
table(n.obs)
efw$Race<-as.factor(efw$Race)
#efw$EFWs<- scale(efw$EFW)

# select the measurement (ObsNum=5) of EFW 
efw5<-rbind(efw[which(efw$ObsNum=="5"),])
efw1<-rbind(efw[which(efw$ObsNum=="1"),])
efwd<-rbind(efw[which(efw$ObsNum=="5"),]-efw[which(efw$ObsNum=="1"),])
names(efwd)[2]<-"Obsd"
names(efwd)[3]<-"GAd"
names(efwd)[8]<-"Scortd"
names(efwd)[9]<-"EFWd"
efwd<-cbind(efw1,efwd)
efwd<-efwd[,-c(2,10,13:16)]
# efwd<-efwd[,-c(2,11,14:17)]
attach(efwd)
names(efwd)[14]<-"EFWds"


# change the data into wide forms
#efw2<-reshape(efw, idvar="subid", v.names="EFW", timevar="ObsNum", direction="wide")


#summary of the groupdata
efwgroup<-groupedData(EFW~ObsNum|subid,data = efw)
gsummary(efwgroup, mean)
summary(gsummary(efwgroup, mean))

EFW.grp = groupedData(EFW ~ GA | subid, data = efw,
                      labels=list(x = "GA", y = "Estimated Fetal Weight"),
                      units = list(x = "(subject)"))


## R has a default plot option for a grouped data object:
plot(EFW.grp)

EFW.grp= groupedData(Scort ~ GA | subid, data = efw,
                      labels=list(x = "GA", y = "Estimated Fetal Weight"),
                      units = list(x = "(subject)"))


## R has a default plot option for a grouped data object:
plot(EFW.grp)



#explorary data analysis

#explore the relationship btwn predictors

boxplot(efwd$EFWd~efwd$Race)
boxplot(efwd$EFWd~efwd$RiskOB)
boxplot(efwd$EFWd~efwd$Gender)

boxplot(efw5$EFW~efw5$Race,xlab="Mother's Race ",ylab="Estimated Fetal Weight (EFW in grams)",
        main="Estimated Fetal Weight Across Mother's Race")
boxplot(efw5$EFW~efw5$RiskOB,xlab="RiskOB",ylab="Estimated Fetal Weight (EFW in grams)",
        main="Estimated Fetal Weight Across RiskOB")
boxplot(efw5$EFW~efw5$Gender,xlab="Child's gender ",ylab="Estimated Fetal Weight (EFW in grams)",
        main="Estimated Fetal Weight Child's gender")

ggplot() +
  geom_point(data=efw5, mapping=aes(x=Scort, y=EFW)) + 
  xlab("Scort") +
  ylab("EFW")+ geom_smooth(method = lm)
lines(efw5$Scort, efw5$EFW, lwd=2)

ggplot(efw5,aes(x=Scort,y=EFW)) + geom_point(shape=19) +
  xlab("Scort in last measurement") + ylab("EFW in last measurement") + 
  geom_smooth(method = lm)
                           

ggplot(efwd,aes(x=Scortd,y=EFWd)) + geom_point(shape=19) +
  xlab("Change in Scort from first to last measurement") + ylab("Change in EFW from first to last measurement")+geom_smooth(method = lm)

ggplot() +
  geom_point(data=efw5, mapping=aes(x=GA, y=EFW)) + 
  xlab("GA") +
  ylab("EFW") + geom_smooth(method = lm)



ggplot() +
  geom_point(data=efwd, mapping=aes(x=Scortd, y=EFWd)) + 
  xlab("Change in Scort") +
  ylab("Change in EFW") 

ggplot(efwd,aes(x=Scortd,y=EFWd)) + geom_point(shape=19) +
  xlab("Scortd") + ylab("EFWd") + 
  geom_smooth(method = lm)




#explore the distribution of EFW
library(ggplot2)
library(gcookbook) 
ga_mean<-tapply(efwgroup$GA,efwgroup$ObsNum, count)


library(PerformanceAnalytics)
chart.Correlation(cbind(efw0,scort0,ga0,bmi0), method = "pearson")

efw0 <- unlist( lapply( split( efw$EFW, efw$subid), mean ) )       ## unique  efw
summary( efw0 )
hist( efw0, xlab = "EFW", nclass = 10,probability = T, ylim=c(0,0.004) )
lines(density(efw0))

with(efw, table(GA, ObsNum))
hist( efw$GA, xlab = "GA", nclass = "obs" )


scort0 <- unlist( lapply( split( efw$Scort, efw$subid), mean ))       ## unique  scort
summary( scort0 )
hist( scort0, xlab = "Scort", nclass = 10, probability = T )
lines(density(scort0))

ga0 <- unlist( lapply( split( efw$GA, efw$subid), mean ) )       ## unique  ga
summary( ga0  )
hist( ga0 , xlab = "GA", nclass = 10, probability = T )
lines(density(ga0))

bmi0 <- unlist( lapply( split( efw$BMI, efw$subid), mean ) )       ## unique  efw
summary( bmi0  )
hist( bmi0 , xlab = "EFW", nclass = 10,probability = T )



plot(efw$EFW ~ efw$Scort, xlab="Scort ",ylab="EFW", main="Estimated Fetal Weight Across Gestational Age", 
     ylim=c(0,4000), xlim=c(0,2), cex=.5)
# cex argument specifies size of points (relative to 1)
# Add vertical dotted line at time 0:
abline(v=27, lty=1)
# Add lowess smoothed curve:
lines(lowess(efw$Scort, efw$EFW , f=0.5), lwd=2)

bnplot(efwgroup$EFW ~ efwgroup$GA, xlab="Gestational Age ",ylab="Estimated Fetal Weight (EFW in grams)", main="Estimated Fetal Weight Across Gestational Age", 
     ylim=c(0,4000), xlim=c(15,40), cex=.5)
# cex argument specifies size of points (relative to 1)
# Add vertical dotted line at time 0:
abline(v=27, lty=1)
# Add lowess smoothed curve:
lines(lowess(efwgroup$GA, efwgroup$EFW, f=0.5), lwd=2)

plot(log(efwgroup$EFW) ~ efwgroup$GA, xlab="Gestational Age ",ylab="Estimated Fetal Weight (EFW in grams)", main="Estimated Fetal Weight Across Gestational Age", 
     ylim=c(2,10), xlim=c(15,40), cex=.5)
# cex argument specifies size of points (relative to 1)
# Add vertical dotted line at time 0:
abline(v=27, lty=1)
# Add lowess smoothed curve:
lines(lowess(efwgroup$GA, log(efwgroup$EFW), f=0.5), lwd=2)

plot(efw$EFWs ~ efw$GA, xlab="Gestational Age ",ylab="Estimated Fetal Weight (EFW in grams)", main="Estimated Fetal Weight Across Gestational Age", 
    ylim=c(-2,3), xlim=c(15,40), cex=.5)
# cex argument specifies size of points (relative to 1)
# Add vertical dotted line at time 0:
abline(v=27, lty=1)
# Add lowess smoothed curve:
lines(lowess(efw$GA, efw$EFWs, f=0.5), lwd=2)

plot(efw$Scort ~ efw$GA, xlab="Gestational Age ",ylab="Estimated Fetal Weight (EFW in grams)", main="Estimated Fetal Weight Across Gestational Age", 
     ylim=c(0,2), xlim=c(15,40), cex=.5)
# cex argument specifies size of points (relative to 1)
# Add vertical dotted line at time 0:
abline(v=27, lty=1)
# Add lowess smoothed curve:
lines(lowess(efw$GA, efw$Scort, f=0.5), lwd=2)


efwg<-tapply(efwgroup$EFW,efwgroup$GA,mean)
plot(efwg)
efwg1<-tapply(log(efwgroup$EFW),efwgroup$GA,mean)
plot(efwg1)
scortg<-tapply(efwgroup$Scort,efwgroup$GA,mean)
plot(scortg)

#spaghetti plot





efwgroup<-groupedData(EFW~Scort|subid,data = efwgroup)
plot(efwgroup,outer=~as.factor(GA),aspect = 1,key = FALSE)
#plot(efwgroup,aspect = 1,key = FALSE)

efwgroup<-groupedData(EFW~Scort|subid,data = efwgroup)
plot(efwgroup,outer=~as.factor(ObsNum),aspect = 1,key = FALSE)

efwgroup<-groupedData(EFW~GA|subid,data = efwgroup)
plot(efwgroup,outer=~1,aspect = 1,key = FALSE,xlab="Gestational Age(Weeks) ",
     ylab="Estimated Fetal Weight (EFW in grams)", 
     main="Individuals Estimated Fetal Weight Across Gestational Age", ylim=c(0,4000), xlim=c(15,40))



ecortgroup<-groupedData(EFW~Scort|subid,data = efw)
plot(ecortgroup,outer=~1,aspect = 1,key = FALSE)


scortgroup<-groupedData(Scort~efw$GA|subid,data = efw)
plot(scortgroup,aspect = 1,key = FALSE)

plot(efw$EFW ~ efw$GA, xlab="Gestational Age(Weeks) ",
     ylab="Estimated Fetal Weight (EFW in grams)", 
     main="Individuals Estimated Fetal Weight Across Gestational Age", ylim=c(0,4000), xlim=c(15,40))
for(i in unique(efw$subid) ){
  lines(efw$EFW[subid==i] ~ efw$GA[subid==i])
}

##### sample 25 subjects
#
subid <- unique( efw$subid )
subset <- sample( subid, 25 )
for( j in 1:25 ){
  lines( efw$EFW[ efw$subid==subset[j] ], efw$GA[ efw$subid==subset[j] ] )
}
#title("EFW -- 25 subjects")


# fit regression
efw51<-efw5[-c(8,17,54,85),]
efw5_reg <- lm(EFW ~ GA+Scort+BMI+Gender+RiskOB+Race, data=efw5)
summary(efw5_reg)
source( "http://www.ics.uci.edu/~dgillen/STAT212/Handouts/Stat212Functions.R" )

efw5_reg <- robust.se.lm( efw5_reg )
round(efw5_reg, 3)
library(xtable)
xtable(efw5_reg)

#library(stargazer)

#stargazer(round(robust.se.lm( efw5_reg ),3))

efw5_reg1 <- lm(EFW ~ GA+BMI+Gender+RiskOB+Race, data=efw5)
summary(efw5_reg1)
anova(efw5_reg1,efw5_reg)

# check assumptions - normality
qqnorm(efw5_reg$residuals)
qqnorm(efw5_reg1$residuals)
qqline(efw5_reg1$residuals)
plot(efw5$Scort,efw5_reg1$residuals)
hatvalues(efw5_reg)
plot(efw5_reg)

efwd_reg <- lm(EFWd ~ BMI+Gender+RiskOB+Race+GAd+Scortd, data=efwd)
summary(efwd_reg)
plot(efwd_reg)
efwd_reg1 <- lm(EFWd ~ BMI+Gender+RiskOB+Race+GAd, data=efwd)
summary(efwd_reg1 )
anova(efwd_reg1,efwd_reg)
efwd_reg <- robust.se.lm( efwd_reg )
round(efwd_reg, 3)
library(xtable)
xtable(efwd_reg)

# check assumptions - normality
qqnorm(efwd_reg$residuals)
qqnorm(efwd_reg1$residuals)
qqline(efwd_reg$residuals)


mod1= lme(EFW ~ 1+Scort*GA+BMI+Gender+Race+RiskOB, data = efwgroup, 
          method = "ML",random = reStruct( ~ 1  | subid, pdClass="pdSymm"),
          correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(mod1)
plot(mod1)

modlme= lme(EFW ~ 1+Scort*GA+BMI+Gender+Race+RiskOB, data = efwgroup, 
          method = "ML",random = reStruct( ~ 1  | subid, pdClass="pdSymm"),
          correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(modlme)
plot(modlme)

modlme1= lme(EFW ~ 1+Scort*GA+BMI+Gender+Race+RiskOB+I(GA^2), data = efwgroup, 
          method = "ML",random = reStruct( ~ 1  | subid, pdClass="pdSymm"),
          correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(modlme1)
plot(modlme1)

modlme2= lme(EFW ~ 1+Scort+BMI+Gender+Race+RiskOB+GA+I(GA^2), data = efwgroup, 
             method = "ML",random = reStruct( ~ 1  | subid, pdClass="pdSymm"),
             correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(modlme2)
anova(modlme2,modlme1)
xtable(anova(modlme2,modlme1))

anova(modlme,modlme1)
xtable(anova(modlme,modlme1))
library(stargazer)
stargazer(modlme1)




mod111= lme(EFW ~ 1+Scort*as.factor(ObsNum)+BMI+Gender+Race+RiskOB, data = efwgroup, 
          method = "ML",random = reStruct( ~ 1 | subid, pdClass="pdSymm"),
          correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(mod111)
plot(mod111)

mod1111= lme(EFW ~ 1+Scort*GA+BMI+Gender+Race+RiskOB+I(GA^2)*Scort, data = efwgroup, 
            method = "ML",random = reStruct( ~ 1 | subid, pdClass="pdSymm"),
            correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(mod1111)
plot(mod1111)


mod11= lme(log(EFW) ~ 1+Scort*I(GA^2)+BMI+Gender+Race+RiskOB, data = efwgroup, 
          method = "ML",random = reStruct( ~ 1 | subid, pdClass="pdSymm"),
          correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(mod11)
plot(mod11)

mod2= lme(EFW ~ 1+Scort+as.factor(ObsNum)+BMI+Gender+Race+RiskOB, data = efwgroup, 
          method = "ML",random = reStruct( ~ 1 | subid, pdClass="pdSymm"),
          correlation = corExp( form = ~ 1|subid, nugget=TRUE))
summary(mod2)

anova(mod2,mod1)


#mod3= lme(EFW ~ 1+Scort+GA+I(GA^2)+BMI+Gender+as.factor(Race)+RiskOB, data = efwgroup, 
#          method = "ML",random = reStruct( ~ 1 | subid, pdClass="pdSymm"),
 #         correlation = corExp( form = ~ 1|subid, nugget=TRUE))
#summary(mod3)
#mod4= lme(EFW ~ 1+Scort*GA+Scort*I(GA^2)+BMI+Gender+as.factor(Race)+RiskOB, data = efwgroup, 
#          method = "ML",random = reStruct( ~ 1 | subid, pdClass="pdSymm"),
#          correlation = corExp( form = ~ 1|subid, nugget=TRUE))
#summary(mod4)

#anova(mod4,mod3)

source( "http://www.ics.uci.edu/~dgillen/STAT212/Handouts/Stat212Functions.R"  )
lmCI(efw5_reg) 
source( "http://www.ics.uci.edu/~dgillen/STAT212/Handouts/Stat212Functions.R" )

robustfit <- robust.se.lm( efw5_reg )
round(robustfit, 3)


install.packages("rgl")   #安装rgl程序包
library('rgl')
attach(efw)
plot3d(EFW,Scort,GA,col=rainbow(12),size=10)   
