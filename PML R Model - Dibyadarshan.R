setwd('C:/PML Project')
library(caret)
training = read.csv("pml-training.csv",na.strings = c("NA","#DIV/0!"))
testing = read.csv("pml-testing.csv",na.strings = c("NA","#DIV/0!"))
dim(training)
table(training$classe)
training = training[,apply(training,2,function(x)!any(is.na(x)))]
training = training[,-c(1:7)]
testing = testing[,apply(testing,2,function(x)!any(is.na(x)))]
testing = testing[,-c(1:7)]
dim(training)
set.seed(500000)
inTrain = createDataPartition(y=training$classe, p=0.75, list=FALSE)
trainset1 = training[inTrain,]
trainset2 = training[-inTrain,]
dim(trainset1)
dim(trainset2)
library(randomForest)
modFitRF = randomForest(classe~., data = trainset1, method="rf", importance=T, trControl = trainControl(method = "cv", classProbs = TRUE, savePredictions = TRUE, allowParallel = TRUE, number = 10))
plot(modFitRF)
predict1 = predict(modFitRF, trainset2, type = "class")
confusionMatrix(predict1, trainset2$classe)
predict2 = predict(modFitRF, testing)
predict2
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predict2)
