library(caret)
data(faithful)
View(faithful)

#data partition
set.seed(1)
inTrain<-createDataPartition(y=faithful$waiting,p=0.8,list=F)
train<-faithful[inTrain,]
test<-faithful[-inTrain,]
#inTrain<-createDataPartition(y=faithful$waiting,p=0.6,list=F)
#train<-faithful[inTrain,]
#nottrain<-faithful[-inTrain,]
#inTest<-createDataPartition(y=nottrain$waiting,p=0.5,list=F)
#test<-nottrain[inTest,]
#validation<-nottrain[-inTest,]

#regression model
lm<-lm(eruptions~waiting,data=train)
modFit<-train(eruptions~waiting,data=train,method="lm") #using caret
lm$coefficients
all.equal(lm$coefficients,modFit$finalModel$coefficients)
#prediction
pred<-predict(lm,test)
pred1<-predict(lm,test,interval="prediction") #with interval

par(mfrow=c(1,3))
#plot train and test
plot(train$waiting, train$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(train$waiting,lm$fitted,lwd=3)
plot(test$waiting, test$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(test$waiting,pred,lwd=3)
#plot interval
ord<-order(test$waiting) #return test set No. ordered by waiting
plot(test$waiting,test$eruptions,pch=19,col="blue")
matlines(test$waiting[ord],pred1[ord,],type="l",col=c(1,2,2),lty=c(1,1,1),lwd=3)

#RMSE
sqrt(sum((lm$fitted-train$eruptions)^2))
sqrt(sum((pred-test$eruptions)^2))