library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
modelFit
modelFit$finalModel

predictions <- predict(modelFit, newdata = testing)
predictions

confusionMatrix(predictions, testing$type)

folds <- createFolds(y=spam$type, k=10, 
                     list = TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

#return only the test set samples:

folds <- createFolds(y=spam$type, k=10,
                     list=TRUE,returnTrain = FALSE)
sapply(folds, length)

#resampling with replacement. You might get the observation repeated multiple times.

folds <- createResample(y=spam$type, times = 10, list=TRUE)
sapply(folds, length)
folds[[1]][1:10]

# You can set lots of parameters when training...

modelFit <- train(type ~., data=training, method = "glm")
args(train.default) #this doesn't work



