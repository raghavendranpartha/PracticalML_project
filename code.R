require(data.table)
require(caret)
require(dplyr)
require(ggplot2)
require(randomForest)
training_full <- data.frame(fread("pml-training.csv"))
testing <- data.frame(fread("pml-testing.csv"))

training_full <- mutate(training_full, classe=factor(classe))
training_full <- select(training_full,-c(1:7))
inds <- character(0)
for(ii in 1:ncol(training_full)){
  inds[ii]<-class(training_full[,ii])
}

training_fulls <- select(training_full,which(inds!='character'))

fNas <- apply(training_fulls,2,function(x){
  mean(is.na(x))
})

training_fulls <- select(training_fulls,which(fNas==0))
training_fulls <- select(training_fulls,-findCorrelation(cor(as.matrix(training_fulls[,-53]))))

set.seed(1)
inTrain <- createDataPartition(training_fulls$classe, p = 0.7, list = FALSE)
training <- training_fulls[inTrain,]  
valid <- training_fulls[-inTrain,]

fitControl    <- trainControl(method = 'cv',number = 10)
tgrid           <- expand.grid(mtry=c(6)) 
Sys.time()
model  <- train(classe ~ ., data = training, method = "rf", trControl = fitControl, tuneGrid = tgrid)
Sys.time()
validpred <- predict(model, newdata = valid)
confusionMatrix(valid$classe,validpred)
trainpred <- predict(model, newdata=training)
confusionMatrix(training$classe,trainpred)

testPred <- predict(model, newdata = testing)

answers <- as.character(testPred)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

fitControl    <- trainControl(method = 'oob')
#fitControl    <- trainControl(method = 'none')
tgrid           <- expand.grid(mtry=c(6)) 
model_oob  <- train(classe ~ ., data = training, method = "rf", trControl = fitControl, tuneGrid = tgrid)
Sys.time()
validpred <- predict(model_oob, newdata = valid)
confusionMatrix(valid$classe,validpred)
trainpred <- predict(model_oob, newdata=training)
confusionMatrix(training$classe,trainpred)
testPredoob <- predict(model_oob, newdata = testing)
answersoob <- as.character(testPredoob)


fitControl    <- trainControl(method = 'boot632')
#fitControl    <- trainControl(method = 'none')
tgrid           <- expand.grid(mtry=c(6)) 
model_boo6  <- train(classe ~ ., data = training, method = "rf", trControl = fitControl, tuneGrid = tgrid)
Sys.time()
validpred <- predict(model_boo6, newdata = valid)
confusionMatrix(valid$classe,validpred)
trainpred <- predict(model_boo6, newdata=training)
confusionMatrix(training$classe,trainpred)
model_boo6full  <- train(classe ~ ., data = training_fulls, method = "rf", trControl = fitControl, tuneGrid = tgrid)
testPredboo6 <- predict(model_boo6, newdata = testing)
testPredboo6full <- predict(model_boo6full, newdata = testing)
answersboo6 <- as.character(testPredboo6)
answersboo6full <- as.character(testPredboo6full)

fitControl    <- trainControl(method = 'boot')
tgrid           <- expand.grid(mtry=c(6)) 
Sys.time()
model_boo  <- train(classe ~ ., data = training, method = "rf", trControl = fitControl, tuneGrid = tgrid)
Sys.time()
validpred <- predict(model_boo, newdata = valid)
confusionMatrix(valid$classe,validpred)
testPredboo <- predict(model_boo, newdata = testing)
answersboo <- as.character(testPredboo)

pml_write_files(answersboo6full)


fitControl    <- trainControl(method = 'none')
tgrid           <- expand.grid(mtry=c(6)) 
Sys.time()
modelnaive  <- train(classe ~ ., data = training, method = "rf", trControl = fitControl, tuneGrid = tgrid)


