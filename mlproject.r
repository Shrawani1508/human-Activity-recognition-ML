library(ggplot2)
library(readr) 
library("e1071") 
list.files("D:/user data/Desktop")
traindata <- read.csv("D:/user data/Desktop/train.csv", header=T)
testdata <- read.csv("D:/user data/Desktop/test.csv", header=T)
data<-rbind(traindata,testdata)
nameVec <- make.names(names(data),unique=TRUE)
names(data) <- nameVec
traindata<-data[1:7352,]
testdata<-data[-c(1:7352),]
dim(data)
pc <- prcomp(traindata[,-563], center=TRUE, scale=TRUE)
pc.var <- pc$sdev^2
pc.pvar <- pc.var/sum(pc.var)
plot(cumsum(pc.pvar),xlab="Principal component", ylab="Proportion of variance ",type='b',main="Components proportions",col="red")
abline(h=0.95)
abline(v=100)
train.data<-data.frame(activity=traindata$Activity,pc$x)
train.data<-train.data[,1:100]

svm_model <- svm(activity ~ ., data=train.data)


test.data<-predict(pc,newdata=testdata)
test.data<-as.data.frame(test.data)

test.data<-test.data[,1:100]
result<-predict(svm_model,test.data,type="class")
test.data$Activity=testdata$Activity
references<-test.data$Activity
t<-table(references,result)
t
Accuracy <- (t[1,1]+t[2,2]+t[3,3]+t[4,4]+t[5,5]+t[6,6])/sum(t)
AccuracyRate <- Accuracy*100
c("Accuracy",AccuracyRate)
library(randomForest)
library(caret)
modRFfit <- randomForest(activity ~ ., data = train.data)
predRFfit <- predict(modRFfit,train.data)
cmRF <- confusionMatrix(predRFfit,train.data$ activity)
print(cmRF)
predQuizRF <- predict(modRFfit,test.data)
predQuizRF
