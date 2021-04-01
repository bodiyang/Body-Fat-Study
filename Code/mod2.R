##Data pre-processing
#Import the data
fat=read.csv('BodyFat.csv')
dim(fat)
colnames(fat)

#Explore the data
head(fat)
tail(fat)
fat=fat[,-c(1,3)] #Remove the first and third column (useless)
summary(fat)
index=which(fat[,1]==0) #Find the person with 0 fat (impossible)
fat=fat[-index,] #Remove the line since the number must be wrongly recorded
hist(fat$BODYFAT,breaks=30)

set.seed(123)
samp=sample(251,200) #Split the data for training and testing
train=fat[samp,]
test=fat[-samp,]

##Data modeling (without interaction)

lm1<-lm(BODYFAT~.,data=train)
model1=step(lm(BODYFAT~1,data=train),scope=formula(lm1),direction='both',trace=F)
summary(model1)
qqnorm(model1$residuals)
qqline(model1$residuals)
shapiro.test(model1$residuals) #Residuals are normal

anova(model1)

model1=lm(formula = BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, data = train)
summary(model1) #Some variables are not significant, so we try removing it.

shapiro.test(model1$residuals) #Residuals are normal

#Check for multicollinearity
library(car)
vif(model1) #The VIF values are acceptable (less than 10)
cor(train[,c(3,6,8,14,15)])
pairs(train[,c(3,6,8,14,15)]) #There exists multicollinearity

#Model evaluation: RMSE (root mean square error)
(rmse1<-sqrt(mean((test[,1]-predict(model1,test[,2:15]))^2)))

##Modeling with interaction

lm4<-lm(BODYFAT~.*.,data=train)
model4=step(lm(BODYFAT~1,data=train),scope=formula(lm4),direction='both',trace=F)
summary(model4)
qqnorm(model4$residuals)
qqline(model4$residuals)
shapiro.test(model4$residuals) #Residuals are normal

anova(model4)

model4=lm(formula = BODYFAT ~ ABDOMEN + WEIGHT + WRIST + ADIPOSITY + 
            NECK + AGE + ABDOMEN:NECK + WRIST:AGE + ABDOMEN:AGE + 
            WEIGHT:ADIPOSITY, data = train)
summary(model4) #Some variables are not significant, so we try removing it.

shapiro.test(model1$residuals) #Residuals are normal

#Model evaluation: RMSE (root mean square error)
(rmse4<-sqrt(mean((test[,1]-predict(model4,test[,2:15]))^2)))

##Trying modeling after log transformation

lm1<-lm(log(BODYFAT)~.,data=train)
model1=step(lm(log(BODYFAT)~1,data=train),scope=formula(lm1),direction='both',trace=F)
summary(model1)
qqnorm(model1$residuals)
qqline(model1$residuals)
shapiro.test(model1$residuals) #Residuals are not normal

stu.res=studres(model1) #Find outliers using studentized residuals
which(abs(stu.res)>3)
cook.dist=cooks.distance(model1) #Find influential points by Cook's distance
which(cook.dist>1)

train1=train[-c(108,140),]

lm2<-lm(log(BODYFAT)~.,data=train1)
model2=step(lm(log(BODYFAT)~1,data=train1),scope=formula(lm2),direction='both',trace=F)
summary(model2)
qqnorm(model2$residuals)
qqline(model2$residuals)
shapiro.test(model2$residuals) #Residuals are approximately normal

anova(model2)

model2=lm(formula = log(BODYFAT) ~ ABDOMEN + NECK + FOREARM +
            HIP + WRIST, data = train1) #Some variables are not significant, so we try removing it.

(log_rmse2<-sqrt(mean((test[,1]-exp(predict(model2,test[,2:15])))^2)))

#Including interaction
lm1<-lm(log(BODYFAT)~.*.,data=train)
model1=step(lm(log(BODYFAT)~1,data=train),scope=formula(lm1),direction='both',trace=F)
summary(model1)
qqnorm(model1$residuals)
qqline(model1$residuals)
shapiro.test(model1$residuals) #Residuals are not normal

stu.res=studres(model1) #Find outliers using studentized residuals
which(abs(stu.res)>3)
cook.dist=cooks.distance(model1) #Find influential points by Cook's distance
which(cook.dist>1)

train1=train[-c(108,135,140),]

lm2<-lm(log(BODYFAT)~.*.,data=train1)
model2=step(lm(log(BODYFAT)~1,data=train1),scope=formula(lm2),direction='both',trace=F)
summary(model2)
qqnorm(model2$residuals)
qqline(model2$residuals)
shapiro.test(model2$residuals) #Residuals are not normal

stu.res=studres(model2) #Find outliers using studentized residuals
which(abs(stu.res)>3)
cook.dist=cooks.distance(model2) #Find influential points by Cook's distance
which(cook.dist>1)

train2=train1[-c(24,152),]

lm3<-lm(log(BODYFAT)~.*.,data=train2)
model3=step(lm(log(BODYFAT)~1,data=train2),scope=formula(lm2),direction='both',trace=F)
summary(model3)
qqnorm(model3$residuals)
qqline(model3$residuals)
shapiro.test(model3$residuals) #Residuals are normal

anova(model3)

model3=lm(formula = log(BODYFAT) ~ ABDOMEN + WEIGHT + WRIST + NECK + 
            AGE + FOREARM + THIGH + HIP + ABDOMEN:WEIGHT + 
            WEIGHT:NECK + ABDOMEN:WRIST + ABDOMEN:NECK + NECK:THIGH + 
            AGE:FOREARM + WRIST:FOREARM, data = train2) #Some variables are not significant, so we try removing it.

(log_rmse3<-sqrt(mean((test[,1]-exp(predict(model3,test[,2:15])))^2)))

##Compare the models
cat('RMSE without interaction:',rmse1,'\nRMSE with interaction:',rmse4)
cat('RMSE (log) without interaction:',log_rmse2,'\nRMSE (log) with interaction:',log_rmse3)

#The first model performs best on test data

##Try another seed number
set.seed(99)
samp=sample(251,200) #Split the data for training and testing
train0=fat[samp,]
test0=fat[-samp,]

##Data modeling (without interaction)
lm0<-lm(BODYFAT~.,data=train0)
model0=step(lm(BODYFAT~1,data=train0),scope=formula(lm0),direction='both',trace=F)
summary(model0)
qqnorm(model0$residuals)
qqline(model0$residuals)
shapiro.test(model0$residuals) #Residuals are normal

anova(model0)

model0=lm(formula = BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, data = train0)
summary(model0) #Some variables are not significant, so we try removing it.

shapiro.test(model0$residuals) #Residuals are normal

#Check for multicollinearity
vif(model0) #The VIF values are acceptable (less than 10)
cor(train[,c(3,8,14,15)])
pairs(train[,c(3,8,14,15)]) #There exists multicollinearity

#Model evaluation: RMSE (root mean square error)
(rmse0<-sqrt(mean((test0[,1]-predict(model0,test0[,2:15]))^2)))

#We still find the four variables for prediction
##Final model
model_final=lm(formula = BODYFAT ~ ABDOMEN + WEIGHT + WRIST + FOREARM, data = fat)
summary(model_final)
anova(model_final)
par(mfrow=c(2,2))
plot(model_final)
par(mfrow=c(1,1))
plot(fat[,1]-predict(model_final,fat),main="Residual plot",xlab = "index",ylab = "residual",ylim=c(-15,15))
abline(h=0,lty=2)


##Alternative model
model_1=lm(formula = BODYFAT ~ ABDOMEN + WEIGHT, data = fat)
summary(model_1)
model_2=lm(formula = BODYFAT ~ ABDOMEN, data = fat)
summary(model_2)

