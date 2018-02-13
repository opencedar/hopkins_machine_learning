#1
require(AppliedPredictiveModeling)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y <- factor(vowel.test$y)
vowel.train$y <- factor(vowel.train$y)

set.seed(33833)

require(caret)
rf.Fit <- train(y ~., data=vowel.train, method="rf", prox=TRUE)
boost.Fit <- train(y ~., data=vowel.train, method="gbm", verbose=FALSE)

pred.RF <- predict(rf.Fit, vowel.test)
pred.Boost <- predict(boost.Fit, vowel.test)

rf.table = table(pred.RF, vowel.test$y) 

accuracy <- function(predict, actual) {
  sum(predict == actual) / length(actual)}

accuracy(pred.RF, vowel.test$y)
#RF acccuracy is .60389
accuracy(pred.Boost, vowel.test$y)
#Boosting accuracy is 0.512987

agreement = pred.RF == pred.Boost

accuracy(pred.RF[agreement], vowel.test$y[agreement])
#Agreement accuracy is 0.635514

#2
#library(caret)


library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

require(caret)
rf.Fit <- train(diagnosis ~., data=training, method="rf", prox=TRUE)
boost.Fit <- train(diagnosis ~., data=training, method="gbm", verbose=FALSE)
discr.Fit <- train(diagnosis ~., data=training, method="lda", verbose=FALSE)

predRF <- predict(rf.Fit, training)
predBoost <- predict(boost.Fit, training)
predDiscr <- predict(discr.Fit, training)

predDF <- data.frame(predRF, predBoost, predDiscr, diagnosis = training$diagnosis)

combModFit <- train(diagnosis ~., method="gam", data=predDF)

predRFTest <- predict(rf.Fit, testing)
predBoostTest <- predict(boost.Fit, testing)
predDiscrTest <- predict(discr.Fit, testing)

predDFTest <- data.frame(predRFTest, predBoostTest, predDiscrTest, diagnosis = testing$diagnosis)

predCombMod <- predict(combModFit, predDFTest)


accuracy(predDFTest$predRFTest, predDFTest$diagnosis)
accuracy(predDFTest$predBoostTest, predDFTest$diagnosis)
accuracy(predDFTest$predDiscrTest, predDFTest$diagnosis)
accuracy(predCombMod, predDFTest$diagnosis)

#Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.`

