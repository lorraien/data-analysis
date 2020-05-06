Stroke<-read.table("/Users/jing/Desktop/qualify/DA/DA2019.csv",sep=",",header=T)
summary(Stroke)
Strokec<-Stroke[,c(2:5,106)]
library(PerformanceAnalytics)
sex<-as.numeric(Strokec$Gender)-1
chart.Correlation(cbind(sex,Strokec[,2:5]), method = "pearson")
chart.Correlation(cbind(sex,Strokec[,2:5]), method = "pearson")
chart.Correlation(Stroke1[,5:105], method = "pearson")


library(pastecs)
stat.desc(Stroke)
list <-which(rowSums(is.na(Stroke)) > 0) # hafu数据集中有缺失值的行。
hafu_NA <- Stroke[list,]#提取有缺失值的行。
hafu_A <- Stroke[-list,]# 产生无缺失值的行。
attach(Stroke)

library(mice)
md.pattern(Stroke) 

library(Hmisc)
Stroke$Age<-impute(Stroke$Age, mean)
Stroke$LKW<-impute(Stroke$LKW, mean)
Stroke$RACE<-impute(Stroke$RACE, 1)

#knn missing value
library(DMwR)
knnOutput <- knnImputation(Stroke)  # 使用KNN插值.
anyNA(knnOutput)
Stroke1<-cbind(knnOutput)

#recode gender
Stroke1$sex[Stroke1$Gender=="F"]<-0
Stroke1$sex[Stroke1$Gender=="M"]<-1

hist(Stroke1$RACE)
hist(Stroke1$Age)
hist(Stroke1$LKW)




Stroke$age = cut(Stroke$Age, breaks=c(55,65,77), na.rm = TRUE,
                        include.lowest=TRUE)


pa<-table(Stroke$Stroke,Stroke$Age)[2,]/table(Stroke$Age)


#pg<-table(Stroke$Stroke,Stroke$Gender)[2,]/table(Stroke$Gender)

#plkw<-table(Stroke$Stroke,Stroke$LKW)[2,]/table(Stroke$LKW)

pra1<-table(Stroke$Stroke[Stroke$age=="[55,65]"],Stroke$RACE[Stroke$age=="[55,65]"])[2,]/table(Stroke$RACE[Stroke$age=="[55,65]"])
pra2<-table(Stroke$Stroke[Stroke$age=="(65,77]"],Stroke$RACE[Stroke$age=="(65,77]"])[2,]/table(Stroke$RACE[Stroke$age=="(65,77]"])



Stroke1$old[Stroke1$Age>65]<-1
Stroke1$old[Stroke1$Age<=65]<-0




xx<-c(seq(55,76,1))

plot(Stroke$Age,Stroke$Stroke,pch=16, col="blue",ylim=c(0,1),xlab="Age",ylab="Proportion Stroke Outcomes",
     main="Proportion  Stroke Outcomest by Age")
points(c(seq(55,76,1)), pa, type="o",col="red")

plot(Stroke$RACE,Stroke$Stroke, col="green",ylim=c(0,1),xlab="Race",ylab="Proportion Stroke Outcomes",
     main="Proportion  Stroke Outcomest by Race")
points(pra1, type="o",col="red")

points(pra2, type="o",col="blue")
legend("bottomright", c("55-65","65-77"), col=c("red","blue"), pch=c(1,1))





race.glm = glm(Stroke ~ Gender+LKW+Age+RACE, family=binomial(link="logit"), data=Stroke1)
r1<-glm(Stroke ~ Gender+LKW+RACE+old, family=binomial(link="logit"), data=Stroke1)
anova(r1,race.glm,test="Chisq")
      
summary(race.glm)
glmCI(race.glm)
robust.se.glm(race.glm)
library(xtable)
xtable(robust.se.glm(race.glm))

library(stargazer)

stargazer(robust.se.glm(race.glm))


ra.glm = glm(Stroke ~ Gender+LKW+RACE*old, family=binomial(link="logit"), data=Stroke1)
summary(ra.glm)

xtable(glmCI(ra.glm,transform = T,robust = T))

robust.se.glm(ra.glm)
xtable(robust.se.glm(ra.glm))
anova(r1,ra.glm,test="Chisq")
xtable(anova(r1,ra.glm,test="Chisq"))


## prediction

means<-apply(Stroke1[,6:105],2,mean)
mean1 <- scale(means)

EEG1 = Stroke1[Stroke1$Stroke=="0",6:105]
EEG2 = Stroke1[Stroke1$Stroke=="1",6:105]

Mean.1 = apply(EEG1,2,mean)
Mean.2 = apply(EEG2,2,mean)

plot(c(1:100),Mean.2,type = "p",col="red",xlab =" E Signal",ylab = " Mean Value",pch=16)
lines(c(1:100),Mean.1,type = "p",col="blue",pch=17)
legend(72,0.8,c("Stroke","Not-Stroke"), col=c("red","blue"),pch = c(16,17))

fit.glm = glm(Stroke ~ LKW+RACE+Age+z+sex, family=binomial(link="logit"), data=Stroke1)
summary(fit.glm)
robust.se.glm(fit.glm)
anova(fit.glm,race.glm,test="Chisq")

fit0 = glm(Stroke ~ RACE+Age+sex+LKW, family=binomial(link="logit"), data=tr)

fit1 = glm(Stroke ~ RACE+Age, family=binomial(link="logit"), data=tr)
anova(fit1,fit0)
#prediction:
predictedML <- predict(fit1, te,type = "response")
predictedML1 <- predict(fit1, tr,type = "response")

fitted.results <- ifelse(predictedML > 0.5,1,0)
# confusion matrix
caret::confusionMatrix(as.factor(fitted.results),as.factor(te$Stroke))

plot(auc, print.auc=TRUE,auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

library(pROC)
auc <- roc(te$Stroke, fitted.results,plot=TRUE)
legend('bottomright', legend=paste('AUC =', round(auc$auc*100, 1)), bty = 'n')

library("pROC")
auc0<- roc(te$Stroke,predictedML,xlim=c(1,0))
auc3<- roc(tr$Stroke,predictedML1)
plot(auc0,col="red",xlim=c(1,0))  
plot.roc(auc3, add=TRUE, col="blue",xlim=c(1,0)) 
legend('bottom', legend=paste('AUCte =', round(auc0$auc*100, 1),'AUCtr =',round(auc3$auc*100, 1)),bty = 'n')

pred1 <- prediction(predictedML, te$Stroke) 
plot(performance(pred1, "tpr", "fpr"), colorize = T, lwd = 3, main = "ROC Curves")

pROC_obj <- roc(df$labels,df$predictions,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


fit2 = glm(Stroke ~ LKW+RACE+Age+sex+z, family=binomial(link="logit"), data=tr)
summary(fit2)
#prediction:
predicted1 <- predict(fit2, te,type = "response")
p3<- predict(fit2, tr,type = "response")

fitted.result <- ifelse(predicted1 > 0.5,1,0)
f3<-ifelse(p3> 0.5,1,0)
# confusion matrix
caret::confusionMatrix(as.factor(fitted.result),as.factor(te$Stroke))
caret::confusionMatrix(as.factor(f3),as.factor(tr$Stroke))

caret::confusionMatrix(as.factor(predicted_classML),as.factor(te$Score))

library(pROC)
auc<- roc(te$Stroke,predicted1)
auc1<- roc(tr$Stroke,p3)
legend('bottomright', legend=paste('AUC =', round(auc$auc*100, 1)), bty = 'n')
plot(auc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

  


fit3 = glm(Stroke ~ RACE+Age+E6+E25+E36+E53+E54+E86, family=binomial(link="logit"), data=tr)
summary(fit3)
fit31 = glm(Stroke ~Age+RACE+E6+E25+E36+E53, family=binomial(link="logit"), data=tr)
summary(fit31)
fit32 = glm(Stroke ~ RACE*E53+E36+E25+E6+Age, family=binomial(link="logit"), data=tr)
summary(fit32)
anova(fit31,fit32,test="Chisq")

xtable(glmCI(fit31,transform = T,robust = T))

xtable(binary.gof(fit31, print.table=TRUE))

xtable(anova(fit1,fit31,test="Chisq"))
#prediction:
predicted2 <- predict(fit3, te,type = "response")
predicted22 <- predict(fit31, tr,type = "response")

fitted <- ifelse(predicted2 > 0.5,1,0)
# confusion matrix
caret::confusionMatrix(as.factor(fitted),as.factor(te$Stroke))

library(pROC)
auc<- roc(te$Stroke,predicted2)
auc1<- roc(tr$Stroke,predicted22)
plot(auc, col="red")  
plot.roc(auc1, add=TRUE, col="blue") 
legend('bottom', legend=paste('AUCte =', round(auc$auc*100, 1),'AUCtr =',round(auc1$auc*100, 1)), bty = 'n')

plot(auc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

modelroc <- roc(te$Stroke,predicted2, plot=TRUE)


net <- neuralnet(Stroke ~LKW+RACE+Age+sex ,tr, hidden=20, 
                 threshold=0.005, learningrate = 0.1, algorithm = "rprop+", err.fct = "sse",
                 act.fct = "logistic")

net.results <- compute(net, te)   #利用训练好的模型进行预测
ls(net.results)   
pp1<- ifelse(net.results$net.result>0.5, "Stroke", "Non")
table(te$Stroke, pp1)
print(net.results$net.result)  #输出
auc<- roc(te$Stroke,net.results$net.result, plot=TRUE)
legend('bottomright', legend=paste('AUC =', round(auc$auc*100, 1)), bty = 'n')

library(nnet) 
data.train.nnet = nnet(
  formula =Stroke ~LKW+RACE+Age+sex+z, 
  data = tr,
  size = 3,
  decay = 0.1, 
  linout = T, 
  trace = F
)

data.test.predict <- predict(
  data.train.nnet, 
  newdata = te
)

library(pROC)
auc<- roc(te$Stroke,data.test.predict, plot=TRUE)
legend('bottomright', legend=paste('AUC =', round(auc$auc*100, 1)), bty = 'n')


pp<- ifelse(data.test.predict>0.5, "Stroke", "Non")

table(te$Stroke, pp)


step(object, scope, scale=0, direction=c("both","backward", "forward"), trace=1,...)

EGG<-Stroke1[,6:106]
fita = glm(Stroke ~ ., family=binomial(link="logit"), data=EGG)
a<-step(fita)
a

muhat <- fitted( fit3 )
pr <- (tr$Stroke - muhat) / (muhat*(1-muhat)) + fit3$coef[2]*tr$Stroke 
plot( tr$Stroke , pr, xlab="mMMSE", ylab="Partial Residual" )
sfit <- loess( pr ~ tr$Stroke  )
idx <- order(sfit$x)
lines( sfit$x[idx], fitted( sfit )[idx], col="red", lwd=2 )
lines( tr$Stroke, fitted( lm( pr ~ tr$Stroke ) ), col="blue", lwd=2 )

plot( hatvalues(fit31), cooks.distance(fit31), xlab="Leverage",
      ylab="Cooks Distance", ylim=c(0,.3) )
abline( h=qf(.05,length(fit31$coef), fit3$df.residual), col="blue", lwd=2 )
abline( v=2*mean(hatvalues(fit31), col="red", lwd=2 ))

        