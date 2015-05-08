library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)
data(iris)
table(iris$Species)

inTrain<-createDataPartition(y=iris$Species,p=0.7,list=F)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training)
dim(testing)

#**********************************************************

#predict with trees

#fit a model
modFit<-train(Species~.,method="rpart",data=training)
print(modFit$finalModel)
predict(modFit,newdata=testing)

#plot tree
plot(modFit$finalModel,uniform=T,main="Classification Tree")
text(modFit$finalModel,use.n=T,all=T,cex=0.8)
#plot tree use rattle
fancyRpartPlot(modFit$finalModel)

#**********************************************************

#random forests

#fit a model
modFit<-train(Species~.,data=training,method="rf",prox=T)
modFit

#get a single tree
getTree(modFit$finalModel,k=2)

#class "centers"
irisP<-classCenter(training[,c(3,4)],training$Species,modFit$finalModel$prox)
irisP<-as.data.frame(irisP)
irisP$Species<-rownames(irisP)
p<-qplot(Petal.Width,Petal.Length,col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),
               size=5,shape=4,data=irisP)

#predict new values
pred<-predict(modFit,testing)
testing$predRight<-pred==testing$Species
table(pred,testing$Species)
qplot(Petal.Width,Petal.Length,col=predRight,data=testing,
      main="newdata Predictions")

#**********************************************************

#model based prediction

#fit models
modlda<-train(Species~.,data=training,method="lda")
modnb<-train(Species~.,data=training,method="nb")

#build predictors
plda=predict(modlda,testing)
pnb=predict(modnb,testing)
table(plda,pnb)

#compare results
equalPredictions=(plda==pnb)
qplot(Petal.Width,Sepal.Width,col=equalPredictions,data=testing)

#**********************************************************

#unsupervised prediction

#cluster with k-means
kMeans1<-kmeans(subset(training,select=-c(Species)),centers=3)
training$clusters<-as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,col=clusters,data=training)
table(kMeans1$cluster,training$Species)

#build predictor
modFit<-train(clusters~.,data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species)

#apply on test
testClusterPred<-predict(modFit,testing)
table(testClusterPred,testing$Species)