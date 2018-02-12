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
