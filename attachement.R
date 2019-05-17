setwd("~/Desktop/2019/NU/2019-spring/MSIT423")
bike<-read.csv("bike.csv")
com <- bike[,c(1:13,45)]
com1.2<- com[,c(3:8,10:14)]
cor(com1.2)
#plot(com1.2)
library(MASS)
tran1=cbind(log(com1.2[,c(1:6,9)]),com1.2[,7:8],com1.2[,10:11])
library(glmnet) 
lam = seq(0,300,length=101)/nrow(com1.2)
x = model.matrix(trips~., com1.2)
fitcom1.1=glmnet(x,com1.2$trips,alpha=1, lambda = lam)
cv.lasso=cv.glmnet(x, com1.2$trips, alpha=1, lambda = lam)
coef(cv.lasso)
#matplot(fitcom1.1$lambda*nrow(com1.2), t(fitcom1.1$beta), type="l"); abline(h=0)
fit2.1= lm(trips~ CTA_TRAIN_STATIONS + BIKE_ROUTES + Retail_Food_Establishment + CAPACITY +I(MINORITY^2) + EDU +CBD, data = com1.2)
summary(fit2.1)
fit2.2= lm(trips~ CTA_TRAIN_STATIONS + Retail_Food_Establishment + CAPACITY +I(MINORITY^2) , data = com1.2)
summary(fit2.2)crime.lasso
par(mfrow=c(2,2))
plot(fit2.2)
##cross validation
library(car)
vif(fit2.2)

####crime
bike2< bike[,c(15,16,17,22,24,31,40,43,45)]
plot(bike2)
x2 = model.matrix(trips~., bike2)
crime.lasso=cv.glmnet(x2, bike2$trips, alpha=1, lambda = lam)
coef(crime.lasso)
fitcrime1=lm(trips ~ ASSAULT+BURGLARY+DECEPTIVE_PRACTICE+HOMICIDE+THEFT+CAPACITY +I(MINORITY^2), data=bike)
fitcrime2=lm(trips ~ ASSAULT+BURGLARY+THEFT+CAPACITY +I(MINORITY^2), data=bike)
summary(fitcrime2)
par(mfrow=c(2,2))
plot(fitcrime2)