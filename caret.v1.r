#练习caret包
require(caret)
require(kernlab)

##exampl1
data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75,list=FALSE)  #matrix
inTrain.2 <- createDataPartition(y=spam$type,p=0.75,list=TRUE) #list
trainning <- spam[inTrain,]
testing <- spam[-inTrain,]

# data("iris")#把多分类变成两分类问题
# iris2<-head(iris,n=100)
# 
# index1<-sample(1:nrow(iris)*2/3,nrow(iris)*0.75*2/3)
# tr <- iris[index1,]
# te <- iris[-index1,]
# model.iris <- train(Species ~.,data=tr,method="glm")

set.seed(32343)
modelFit <- train(type ~.,data=trainning,method="glm")
modelFit$finalModel

predictions<-predict(modelFit,newdata=testing)
confusionMatrix(predictions,testing$type)

#K-folds
folds<-createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE)
sapply(folds,length)
#return test
folds.list<-createFolds(y=spam$type,k=10,list=TRUE,returnTrain=FALSE)
sapply(folds.list,length)
#return resample
folds.resample<-createResample(y=spam$type,times=10,list=TRUE)
sapply(folds.resample,length)
folds.resample[[1]][1:10]
#time slice
tme <- 1:1000
folds.time <- createTimeSlices(y=tme,initialWindow=20,horizon=10)
sapply(folds.time,length)
names(folds.time)


###example 2
require(ISLR)
require(ggplot2)
require(caret)
data(Wage)
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
trainning <-Wage[inTrain,]
testing <- Wage[-inTrain,]

featurePlot(x=trainning[,c("age","education","jobclass")],
            y=trainning$wage,
            plot="pairs")

#-----------example3 :preprocess (L5)----------------
require(caret)
require(kernlab)
data(spam)
require(DT)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
#训练集的均值和方差
mean(training$capitalAve)
sd(training$capitalAve)
#测试集的均值、方差
mean(testing$capitalAve)
sd(testing$capitalAve)
#标准化
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS);sd(trainCapAveS)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAve);sd(testCapAveS)

#Preprocess 函数
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS);sd(trainCapAveS)

testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS);sd(testCapAveS)

##标准化， preProcess 参数
set.seed(32343)
modelFit <- train(type ~.,data=training,method="glm",
                  preProcess=c("center","scale"))
modelFit

###Box-Cox transforms
preObj2 <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS2 <- predict(preObj2,training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS2)
qqnorm(trainCapAveS2)

##Standardizing-Imputing data
set.seed(13343)
#生成NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA
#输入与标准化
require(RANN)
preObj3 <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj3,training[,-58])$capAve
#标准化
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)
quantile(capAve - capAveTruth)

#----------example4: covariates 协变量 (L6)-----------
require(ISLR)
require(ggplot2)
require(caret)
data(Wage)
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
trainning <-Wage[inTrain,]
testing <- Wage[-inTrain,]
table(trainning$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=trainning)
nsv <- nearZeroVar(trainning,saveMetrics = TRUE)
library(splines)
bsBasis <- bs(trainning$age,df=3)
lm1 <- lm(wage ~ bsBasis,data=trainning)
plot(trainning$age,trainning$wage,pch=19,cex=0.5)
points(trainning$age,predict(lm1,newdata=trainning),col="red",pch=19,cex=0.5)
predict(bsBasis,age=testing$age)


#------------------example5:(L7) PCA---------------
require(caret)
require(kernlab)
data(spam)
require(DT)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M<-abs(cor(training[,-58]))
diag(M)<-0
which(M>0.8,arr.ind=TRUE)
require(dplyr)
names(spam)[c(32,34)]
plot(spam[,32],spam[,34])
smallSpam<-select(spam,num415,num857)
prComp<-prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

typeColor <- ((spam$type=="spam")*1+1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

#用caret包实现上面PCA的过程
#方法1
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)
#用PCA降维之后的数据集做训练
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit.pca <- train(training$type ~.,method="glm",data=trainPC)
#用pca降维所训练的模型去预测test
testPC<- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit.pca,testPC))
#方法2
modelFit.pca2 <- train(training$type ~.,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit.pca2,testing))


#------------------example6:(L8)  Logistic Regression------
library(caret);data(faithful);set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,p=0.5,list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
plot(faithful$eruptions,faithful$waiting,col='blue',pch=19)
lm2 <-lm(eruptions ~ waiting,data=trainFaith)
plot(trainFaith$waiting,trainFaith$eruptions,col="blue",pch=19)
lines(trainFaith$waiting,lm2$fitted,lwd=3)
#预测
newdata <-data.frame(waiting=80)
predict(lm2,newdata)
#模型评估
sqrt(sum((lm2$fitted-trainFaith$eruptions)^2))
sqrt(sum(predict(lm2,newdata=testFaith)-testFaith$eruptions)^2)
#预测间隔
pred2 <- predict(lm2,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred2[ord,],type="l",col=c(1,2,2),lty=c(1,1,1),lwd=3)
###以上过程用caret包一步实现
modFit4 <- train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit4$finalModel)
