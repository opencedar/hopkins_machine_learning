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

require(caret)
args(trainControl)
#boot = bootstrapping
#cv = cross-validation
#LOOCV = leave one out cross-validation

#setting the seed--so one can reproduce one's results

set.seed(1235)
modelFit2 <- train(type ~., data=training, method="glm")


#Plotting predictors
#Example: Wage Data
library(ISLR); library(ggplot2); library(caret); 
data(Wage)
summary(Wage)

#Build training / testing sets
inTrain <- createDataPartition(y=Wage$wage, p = 0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")

qplot(age,wage,data=training)

qplot(age,wage,colour=jobclass, data=training)

qq <- qplot(age,wage,colour=education, data=training)
qq + geom_smooth(method = 'lm', formula=y~x)

#making factors
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

p1 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom = c("boxplot"))

p2 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom=c("boxplot", "jitter"))
require(gridExtra)
grid.arrange(p1, p2, ncol=2)

t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

#density plots
qplot(wage, colour = education, data=training, geom="density")
#maybe most useful.

#preprocessing
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main='',xlab="ave. capital run length")

mean(training$capitalAve)

sd(training$capitalAve)

testCapAve <- training$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)

preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)


# you can send preprocessing from training to testing using caret using arguments

#Standardizing Box-Cox
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qnorm(trainCapAveS)

#imputing using k-nearest neighbor imputation
#finds k nearest data vectors and avereges the values of what's missing.
#preprocessing with caret


#Load the Alzheimer's disease data using the commands:

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

#Quiz
#1
#Which of the following commands will create non-overlapping 
#training and test sets with about 50% of the observations assigned to each?

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#2
# Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
#Make a plot of the outcome (CompressiveStrength) versus the index of the 
#samples. Color by each of the variables in the data set (you may find the 
#cut2() function in the Hmisc package useful for turning continuous covariates
#into factors). What do you notice in these plots?

require(Hmisc)
#Cement
p <- ggplot(training, aes(seq_along(CompressiveStrength), CompressiveStrength))
p + geom_point(aes(colour=(cut2(Cement))))

p + geom_point(aes(colour=(cut2(BlastFurnaceSlag))))
p + geom_point(aes(colour=(cut2(FlyAsh))))
#First 100 or so were no fly ash, as well as 400-600
p + geom_point(aes(colour=(cut2(Water))))
#Lots of Water in those trials as well
p + geom_point(aes(colour=(cut2(Superplasticizer))))
#No superplasticizer in 0-100 or 400-600
p + geom_point(aes(colour=(cut2(CoarseAggregate))))
#Same kind of pattern
p + geom_point(aes(colour=(cut2(FineAggregate))))
p + geom_point(aes(colour=(cut2(Age))))
#Lots of 14-56 age in 600-800, 0-100 started old

#3. Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. 
#Why would that be a poor choice for this variable?

hist(training$Superplasticizer)
#-Inf variables.

hist(log(training$Superplasticizer + 1))


#4. Load the Alzheimer's disease data using the commands:

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess() function from the caret package. Calculate the number of principal components 
#needed to capture 90% of the variance. How many are there?

IL_vars <- substr(colnames(training),1,2) == "IL"

preProcess(training[,IL_vars], method = "pca", thresh=0.90)

#PCA needed 9 components to capture 90 percent of the variance

#4
#Create a training data set consisting of only the predictors with variable 
#names beginning with IL and the diagnosis. Build two predictive models, 
#one using the predictors as they are and one using PCA with principal 
#components explaining 80% of the variance in the predictors. 
#Use method="glm" in the train function.

#What is the accuracy of each method in the test set? Which is more accurate?

IL_vars <- substr(colnames(training),1,2) == "IL"
training2 <- training[,c(1, which(IL_vars))]


PCAvars <- preProcess(training[,IL_vars], method = "pca", thresh=0.80)
training3 <- predict(PCAvars, training2)
testing <- predict(PCAvars, testing)

modelFitRaw <- train(diagnosis ~ ., data=training2, method = "glm")
modelFitPCA <- train(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, data=training3, method = "glm")

predictRaw <- predict(modelFitRaw, newdata = testing)
predictPCA <- predict(modelFitPCA, newdata = testing)

require(descr)
crosstab(predictRaw, testing$diagnosis, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE)
crosstab(predictPCA, testing$diagnosis, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE)

#PCA 72, non-PCA 65


