library(caret)
library(rpart)
library(knitr)
library(randomForest)
library(corrplot)
set.seed(888) 
trainUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainFile <- "./data/pml-training.csv"
testFile  <- "./data/pml-testing.csv"
if (!file.exists("./data")) {
  dir.create("./data")
}
if (!file.exists(trainFile)) {
  download.file(trainUrl, destfile=trainFile)
}
if (!file.exists(testFile)) {
  download.file(testUrl, destfile=testFile)
}
trainRaw <- read.csv("./data/pml-training.csv",header=T,sep=",",na.strings=c("NA",""))
testRaw <- read.csv("./data/pml-testing.csv",header=T,sep=",",na.strings=c("NA",""))
dim(trainRaw)
dim(testRaw)
trainRaw <- trainRaw[,-1] # Remove the first column that represents a ID Row
inTrain = createDataPartition(trainRaw$classe, p=0.60, list=F)
training = trainRaw[inTrain,]
validating = trainRaw[-inTrain,]
sum((colSums(!is.na(training[,-ncol(training)])) < 0.6*nrow(training))) # Number of columns with less than 60% of data
## [1] 100
Keep <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.6*nrow(training)))
training   <-  training[,Keep]
validating <- validating[,Keep]
model <- randomForest(classe~.,data=training)
model
importance(model)
confusionMatrix(predict(model,newdata=validating[,-ncol(validating)]),validating$classe)
acrcy<-c(as.numeric(predict(model,newdata=validating[,-ncol(validating)])==validating$classe))
acrcy<-sum(acrcy)*100/nrow(validating)
testRaw <- testRaw[,-1]
testRaw <- testRaw[ , Keep]
testRaw <- testRaw[,-ncol(testRaw)]
testing <- rbind(training[100, -59] , testRaw) 
row.names(testing) <- c(100, 1:20)
predictions <- predict(model,newdata=testing[-1,])
predictions
