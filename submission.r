# Submission Code.

library(caret)
library(randomForest)
library(stringr)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

test.raw <- read.csv("pml-testing.csv", na.strings=c("#DIV/0!","NA",""))
train.raw <-read.csv("pml-training.csv", na.strings=c("#DIV/0!","NA",""))
predictors <- colnames(train.raw[colSums(is.na(train.raw)) == 0])[-(1:7)]
data <- train.raw[predictors]

set.seed(1021)
inTrain <- createDataPartition(y=data$classe, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
model <- train(classe ~ ., data=training, method="rf",
               trControl=trainControl(method="cv", 5))
model$finalModel
hat.predict <- predict(model,test.raw)
hat.predict
setwd(str_join(getwd(),"/output"))
pml_write_files(hat.predict)
