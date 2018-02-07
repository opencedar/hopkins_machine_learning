##RANDOM FORESTS

data(iris); library(ggplot2)
library(caret)
inTrain <- createDataPartition(y=iris$Species, p = 0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modFit <- train(Species~ ., data=training, method="rf", prox=TRUE)
modFit

require(randomForest)

getTree(modFit$finalModel, k=2)

irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)

pred <- predict(modFit, testing); testing$predRight <- pred == testing$Species
table(pred, testing$Species)

#BOOSTING
library(ISLR); data(Wage); library(ggplot2); library(caret)
Wage <- subset(Wage, select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

modFit <- train(wage~.,method="gbm", data=training, verbose=FALSE)
print(modFit)

qplot(predict(modFit, testing), wage, data=testing)


#Model-Based Prediction

inTrain <- createDataPartition(y=iris$Species, p = 0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modlda <- train(Species ~., data = training,method='lda')
modnb <- train(Species ~., data=training, method='nb')
plda <- predict(modlda, testing); pnb <- predict(modnb, testing)
table(plda, pnb)
equalPredictions <- (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)

#Quiz
#1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- segmentationOriginal[segmentationOriginal$Case=="Train",]
testing <- segmentationOriginal[segmentationOriginal$Case=="Test",]
set.seed(125)
model <- train(Class ~., data=training, method="rpart")


plot(model$finalModel)
text(model$finalModel)

#PS, WS, PS, Not possible to predict

#2
#If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy
#smaller or bigger? 
#If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger? 
#Is K large or small in leave one out cross validation?


#The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

#3

load("olive")
olive = olive[,-1]

qplot(olive$Area, olive$Palmitic)

library(caret)
modFit <- train(Area ~., method = "rpart", data=olive)
print(modFit$finalModel)

newdata = as.data.frame(t(colMeans(olive)))

prediction <- predict(modFit, newdata)

#2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area
#as a numeric variable in the leaf predicted for newdata

#4
library(ElemStatLearn)
data(SAheart)
str(SAheart$chd)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

modFit <- train(chd ~ age + alcohol + tobacco + ldl + obesity + typea, method = "glm", family = "binomial", data = trainSA)


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

trainPredict <- predict(modFit, trainSA)
testPredict <- predict(modFit, testSA)

missTrain <- missClass(trainSA$chd, trainPredict)

missTest <- missClass(testSA$chd, testPredict)

#Test 31, Train 27

#5 
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.test$y <- factor(vowel.test$y)
vowel.train$y <- factor(vowel.train$y)

set.seed(33833)

?randomForest
vowelModel <- randomForest(y ~ ., data = vowel.train)
?varImp
sort(unlist(varImp(vowelModel)))

#2, 1,5,6,8,4,9,3,7,10