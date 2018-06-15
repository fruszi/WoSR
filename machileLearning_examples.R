

# Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess() function 
#from the caret package. Calculate the number of principal components needed to 
#capture 90% of the variance. How many are there?

require(caret)
require(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], 
                     method=c("center", "scale", "pca"), 
                     thresh=0.9)
preObj


#Create a training data set consisting of only the predictors with variable names
#beginning with IL and the diagnosis. Build two predictive models, one using the 
#predictors as they are and one using PCA with principal components 
#explaining 80% of the variance in the predictors. Use method="glm" in the 
#train function. 

require(caret)
require(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# The model using all the predictors (Non-PCA)
ILpredictor <- names(training)[IL_col_idx]
trainingIL <- training[, c(ILpredictor, "diagnosis")]
testingIL <- testing[, c(ILpredictor, "diagnosis")]
ModelAll <- train(diagnosis ~ ., data = trainingIL, method = "glm")
confusionMatrix(testingIL$diagnosis, predict(ModelAll, testingIL))

# The model using PCA with principal components explaining 80% of the variance 
# in the predictors
preProc <- preProcess(training[, ILpredictor], method = "pca", thresh = .8)
trainPC <- predict(preProc, training[, ILpredictor])
trainPC$diagnosis <- trainingIL$diagnosis
ModelPCA <- train(diagnosis ~ ., method = "glm", data = trainPC)
testPC <- predict(preProc, testing[, ILpredictor])
confusionMatrix(testingIL$diagnosis, predict(ModelPCA, testPC))



#Load the cell segmentation data from the AppliedPredictiveModeling package using
#the commands: library(AppliedPredictiveModeling) data(segmentationOriginal) 
#library(caret) 1. Subset the data to a training set and testing set based on 
#the Case variable in the data set. 2. Set the seed to 125 and fit a CART model 
#with the rpart method using all predictor variables and default caret settings.
#3. In the final model what would be the final model prediction for cases with 
#the following variable values: a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; 
#PerimStatusCh1=2 b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
#c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
#d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

library(rpart)
library(ggplot2)
library(rattle)
library(rpart.plot)
library(dplyr)

# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 0.6, 
                               list = FALSE) # 60% training
training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain, ]
# 2. Set the seed to 125 and fit a CART model with the rpart method using all 
#predictor variables and default caret settings. (The outcome class is 
#contained in a factor variable called Class with levels 
#"PS" for poorly segmented and "WS" for well segmented.)
set.seed(125)
# this is for fancy plot - the other is for the prediction. Don't yet know how
modFit <- train(Class ~ ., method = "rpart", data = training)

modFit$finalModel

fancyRpartPlot(modFit$finalModel)

# PREDICTIONS
# this formula for the predictions, and not train one (not sure why)
modFit <- rpart(Class ~ .,  data = training)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
testA <- segmentationOriginal[0,]
#testA <- select(testA,TotalIntenCh2,FiberWidthCh1,PerimStatusCh1)
testA[1,c("TotalIntenCh2", "FiberWidthCh1", "PerimStatusCh1")] <- c(23000, 10, 2)
predict(modFit, testA, type="prob")

# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
testB <- segmentationOriginal[0,]
testB[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(50000, 10, 100)
predict(modFit, testB, type="prob")

# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
testC <- segmentationOriginal[0,]
testC[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(57000, 8, 100)
predict(modFit, testC, type="prob")

# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
testD <- segmentationOriginal[0,]
testD[1,c("FiberWidthCh1", "VarIntenCh4","PerimStatusCh1")] <- c(8, 100, 2)
predict(modFit, testD, type="prob")



#...........................................................................
#Then set the seed to 13234 and fit a logistic regression model 
#(method="glm", be sure to specify family="binomial") 
#with Coronary Heart Disease (chd) as the outcome and 
#age at onset, current alcohol consumption, obesity levels, 
#cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol 
#as predictors. Calculate the misclassification rate for your model using this
#function and a prediction on the "response" scale:

missClass = function(values,prediction){
    sum(((prediction > 0.5)*1) != values)/length(values)
    }    
    

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
             data=trainSA, method="glm", family="binomial")
# model misclassification
predictTrainSA <- predict(fit)
missClass(trainSA$chd,predictTrainSA)
# test misclassification
predictTestSA <- predict(fit, testSA)
missClass(testSA$chd,predictTestSA)

#...........................................................................
#Set the variable y to be a factor variable in both the training and test set. 
#Then set the seed to 33833. Fit a random forest predictor relating the factor 
#variable y to the remaining variables. Read about variable importance in random
#forests here: 
#http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
#The caret package uses by default the Gini importance.

#Calculate the variable importance using the varImp function in the caret 
#package. What is the order of variable importance?

require(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modRF <- train(y ~ ., data=vowel.train, method="rf")
res <- predict(modRF,vowel.test)
varImp(modRF)


#...........................................................................
#Set the variable y to be a factor variable in both the training and test set. 
#Then set the seed to 33833. Fit (1) a random forest predictor relating the 
#factor variable y to the remaining variables and (2) a boosted predictor using 
#the "gbm" method. Fit these both with the train() command in the caret package. 

require(caret)
require(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
mod_rf <- train(y ~ ., data = vowel.train, method = "rf")
mod_gbm <- train(y ~ ., data = vowel.train, method = "gbm", verbose=F)
pred_rf <- predict(mod_rf, vowel.test)
pred_gbm <- predict(mod_gbm, vowel.test)

# accuracy for random forest and for boosting model
confusionMatrix(pred_rf, vowel.test$y)
confusionMatrix(pred_rf, vowel.test$y)$overall[1]
confusionMatrix(pred_gbm, vowel.test$y)
confusionMatrix(pred_gbm, vowel.test$y)$overall[1]

# Accuracy among the test set samples where the two methods agree
predDF <- data.frame(pred_rf, pred_gbm, y = vowel.test$y)
# Accuracy among the test set samples where the two methods agree
sum(pred_rf[predDF$pred_rf == predDF$pred_gbm] == 
        predDF$y[predDF$pred_rf == predDF$pred_gbm]) / 
    sum(predDF$pred_rf == predDF$pred_gbm)


#...........................................................................
#Set the seed to 62433 and predict diagnosis with all the other variables using 
#a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis 
#("lda") model. Stack the predictions together using random forests ("rf"). 
#What is the resulting accuracy on the test set? Is it better or worse than 
#each of the individual predictions? 

require(caret)
require(gbm)
require(AppliedPredictiveModeling)
set.seed(62433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
mod_rf <- train(diagnosis ~ ., data = training, method = "rf")
set.seed(62433)
mod_gbm <- train(diagnosis ~ ., data = training, method = "gbm", verbose=F)
set.seed(62433)
mod_lda <- train(diagnosis ~ ., data = training, method = "lda")
pred_rf <- predict(mod_rf, testing)
pred_gbm <- predict(mod_gbm, testing)
pred_lda <- predict(mod_lda, testing)

# stacked model
predDF <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)

# Accuracy using random forests
confusionMatrix(pred_rf, testing$diagnosis)$overall[1]

# Accuracy using boosting
confusionMatrix(pred_gbm, testing$diagnosis)$overall[1]

# Accuracy using linear discriminant analysis
confusionMatrix(pred_lda, testing$diagnosis)$overall[1]

# Stacked Accuracy
confusionMatrix(combPred, testing$diagnosis)$overall[1]



#...........................................................................
#Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
#Which variable is the last coefficient to be set to zero as the penalty 
#increases? (Hint: it may be useful to look up ?plot.enet). 

require(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
mod_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)
plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)



#...........................................................................
#Fit a model using the bats() function in the forecast package to the training 
#time series. Then forecast this model for the remaining time points. For how 
#many of the testing points is the true value within the 95% prediction interval
#bounds? 

library(lubridate) # For year() function below
library(forecast)
dat <- read.csv(
    'C:\\Users\\fishie\\Desktop\\coursera\\repoClone\\datasciencecoursera\\practical_ML\\gaData.csv')
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, nrow(testing))


#...........................................................................
#Set the seed to 325 and fit a support vector machine using the e1071 package 
#to predict Compressive Strength using the default settings. Predict on the 
#testing set. What is the RMSE?

library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)

