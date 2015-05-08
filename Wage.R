library(caret)
library(Hmisc)
library(gridExtra)
library(splines)
library(ISLR)
data(Wage)
View(Wage)

inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
dim(training)
dim(testing)

#**********************************************************

#plot predictors

#feature plot (caret)
featurePlot(x=training[,c("age","education","jobclass")],
             y=training$wage,plot="pairs")

#Qplot (ggplot2)
qplot(age,wage,data=training)
qplot(age,wage,col=jobclass,data=training)
qq<-qplot(age,wage,col=education,data=training)
#add regression smoothers
qq + geom_smooth(method='lm',formula=y~x)  

#cut into factors
cutWage<-cut2(training$wage,g=3) #g=groups
table(cutWage)

#plots with cut2

#boxplot
p1<-qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))
p1
p2<-qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot","jitter"))
p2
grid.arrange(p1,p2,ncol=2)
#table
t1<-table(cutWage,training$jobclass)
t1
prop.table(t1,1) #show fraction
#density plot
qplot(wage,col=education,data=training,geom="density")

#**********************************************************

#create covariates (features)

#dummy variables
table(training$jobclass)
dummies<-dummyVars(wage~jobclass,data=training)
head(predict(dummies,newdata=training))

#remove zero covariates
nsv<-nearZeroVar(training,saveMetrics=T)
nsv #find freqRatio=0 items

#spline basis
bsBasis<-bs(training$age,df=3)
bsBasis

#fitting curves with splines
lm1<-lm(wage~bsBasis,data=training)
#training set
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
#test set
predict(bsBasis,age=testing$age)

#**********************************************************

#multi-covariates linear regression

#fit a linear model
modFit<-train(wage~age+jobclass+education,method="lm",data=training)
finMod<-modFit$finalModel
print(modFit)

#diagnostics
plot(finMod,1,pch=19,cex=0.5,col="00000010")
qplot(finMod$fitted,finMod$residuals,col=race,data=training)
plot(finMod$residuals,pch=19)

#precition vs. truth on test set
pred<-predict(modFit,testing)
qplot(wage,pred,col=year,data=testing)

#use all covariates to fit a linear model
modFitAll<-train(wage~.,data=training,method="lm")
pred<-predict(modFitAll,testing)
qplot(wage,pred,data=testing)

#**********************************************************

#boosting

data(Wage)
Wage<-subset(Wage,select=-c(logwage))
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

#fit a model
modFit<-train(wage~.,method="gbm",data=training,verbose=F)
print(modFit)

#predict on test set
qplot(predict(modFit,testing),wage,data=testing)

#**********************************************************

#combining predictors

data(Wage)
Wage<-subset(Wage,select=-c(logwage))
inBuild<-createDataPartition(y=Wage$wage,p=0.7,list=F)
validation<-Wage[inBuild,]
buildData<-Wage[-inBuild,]
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

#fit models
mod1<-train(wage~.,method="glm",data=training)
mod2<-train(wage~.,method="rf",data=training,
            trControl=trainControl(method="cv",number=3))

#predict on test set
pred1<-predict(mod1,testing)
pred2<-predict(mod2,testing)
qplot(pred1,pred2,col=wage,data=testing)

#combine model and predict
predDF<-data.frame(pred1,pred2,wage=testing$wage)
combModFit<-train(wage~.,method="gam",data=predDF)
combPred<-predict(combModFit,predDF)

#testing errors
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

#predict on validation set
pred1V<-predict(mod1,validation)
pred2V<-predict(mod2,validation)
predVDF<-data.frame(pred1==pred1V,pred2==pred2V)
combPredV<-predict(combModFit,predVDF)

#validation errors
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))