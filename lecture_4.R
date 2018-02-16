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

#3
#Load the concrete data with the commands:
set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]


#Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
#Which variable is the last coefficient to be set to zero as the penalty 
#increases? (Hint: it may be useful to look up ?plot.enet).

set.seed(233)
lasso.Fit <- train(CompressiveStrength ~., data=training, method="lasso")
require(elasticnet)

plot(lasso.Fit$finalModel, "penalty")
#Cement

#4
#Load the data on the number of visitors to the instructors blog from here:
  
#https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv

library(RCurl)
dat <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"))

#Using the commands:

library(lubridate) # For year() function below

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tsTest <- ts(testing$visitsTumblr)

#Fit a model using the bats() function in the forecast package to 
#the training time series. Then forecast this model for the remaining time 
#points. For how many of the testing points is the true value within the 95% 
#prediction interval bounds?

library(forecast)
tsModel <- bats(tstrain)
forecast <- predict(tsModel, nrow(testing), level=c(80,95))
upperBounds <- forecast$upper
insideBounds <- testing$visitsTumblr < upperBounds[,2]
sum(as.numeric(insideBounds)) / nrow(testing)

#226, 96%

#5

#Load the concrete data with the commands:

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Set the seed to 325 and fit a support vector machine using the e1071 package 
#to predict Compressive Strength using the default settings. 
#Predict on the testing set. What is the RMSE?

set.seed(325)
library(e1071)
model <- svm(CompressiveStrength ~ ., data = training)
prediction <- predict(model, testing)
E <- prediction-testing$CompressiveStrength
SE <- sum(E^2)/length(E)
RMSE <- (SE)^0.5

#RMSE = 6.715009
