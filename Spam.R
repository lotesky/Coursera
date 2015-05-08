library(caret)
library(kernlab)
data(spam)

#**********************************************************

#density plot of "your"
plot(density(spam$your[spam$type=="nonspam"]),main="")
lines(density(spam$your[spam$type=="spam"]))
prediction<-ifelse(spam$your<=0.5,"nonspam","spam")
table(prediction,spam$type)/length(spam$type)

#data splitting
inTrain<-createDataPartition(y=spam$type,p=0.75,list=F)
#75% for testing
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)
dim(testing)

#fit a model
set.seed(32323)
modelFit<-train(type~.,data=training,method="glm")
#generalized linear model
modelFit

#final model
modelFit$finalModel

#prediction
predictions<-predict(modelFit,newdata=testing)
predictions

#confusion matrix
confusionMatrix(predictions,testing$type)

#**********************************************************

#data slicing

#K-fold
set.seed(32323)
folds<-createFolds(y=spam$type,k=10,list=T,returnTrain=T)
sapply(folds,length)
folds[[1]][1:10]

#K-fold return test
set.seed(32323)
folds<-createFolds(y=spam$type,k=10,list=T,returnTrain=F)
sapply(folds,length)
folds[[1]][1:10]

#K-fold resampling
set.seed(32323)
folds<-createResample(y=spam$type,times=10,list=T)
sapply(folds,length)
folds[[1]][1:10]

#time slices
set.seed(32323)
tme<-1:1000
folds<-createTimeSlices(y=tme,initialWindow=20,horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

#**********************************************************

#train and trainControl options
args(train.default)
args(trainControl)

#**********************************************************

#pre-processing

#necessity: mean and sd are large
hist(training$capitalAve,xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

#standardizing on training set
trainCapAve<-training$capitalAve
trainCapAveS<-(trainCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

#standardizing on test set
testCapAve<-testing$capitalAve
#training and test must be processed in the same way
testCapAveS<-(testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)
#not ideal but accpetable

#standardizing - preProcess
preObj<-preProcess(training[,-58])
#training set
trainCapAveS<-predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
#test set
testCapAveS<-predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)
#fit a model
set.seed(32323)
modelFit<-train(type~.,data=training,
                preProcess=c("center","scale"),method="glm")
modelFit

#standardizing - BoxCox
preObj<-preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS<-predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

#standardizing - Imputing data
set.seed(13343)
#make some values NA
training$capAve<-training$capitalAve
selectNA<-rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA]<-NA
#impute and standardize
preObj<-preProcess(training[,-58],method="knnImpute")
capAve<-predict(preObj,training[,-58])$capAve
#standardize true values
capAveTruth<-training$capitalAve
capAveTruth<-(capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve-capAveTruth)
quantile((capAve-capAveTruth)[selectNA])
quantile((capAve-capAveTruth)[!selectNA])

#**********************************************************

#correlated predictors
M<-abs(cor(training[,-58]))
diag(M)<-0
which(M>0.8,arr.ind=T)
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])
#rotate the plot
X<-0.71*training$num415+0.71*training$num857
Y<-0.71*training$num415-0.71*training$num857
plot(X,Y)

#Princple Components Analysis

#prcomp with small data
smallSpam<-spam[,c(34,32)]
prComp<-prcomp(smallSpam)
prComp
plot(prComp$x[,1],prComp$x[,2])

#prcomp with large data
typeColor<-((spam$type=="spam")*1+1)
prComp<-prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

#PCA with caret
preProc<-preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC<-predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

#fit a PCA model
preProc<-preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC<-predict(preProc,log10(training[,-58]+1))
testPC<-predict(preProc,log10(testing[,-58]+1))
modelFit<-train(training$type~.,method="glm",data=trainPC)
confusionMatrix(testing$type,predict(modelFit,testPC))

#alternative
modelFit<-train(training$type~.,method="glm",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))