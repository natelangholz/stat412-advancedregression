
#example taken from Applied Predictive Modeling book by Max Kuhn and Kjell Johnson

################################################################################
### Choosing Tuning Parameters

library(AppliedPredictiveModeling)
data(GermanCredit)

library(caret)


## First, remove near-zero variance predictors then get rid of a few predictors 
## that duplicate values. For example, there are two possible values for the 
## housing variable: "Rent", "Own" and "ForFree". So that we don't have linear
## dependencies, we get rid of one of the levels (e.g. "ForFree")

GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

## The model fitting code shown in the computing section is fairly
## simplistic.  For the text we estimate the tuning parameter grid
## up-front and pass it in explicitly. This generally is not needed,
## but was used here so that we could trim the cost values to a
## presentable range and to re-use later with different resampling
## methods.



library(kernlab)
set.seed(231)
sigDist <- sigest(Class ~ ., data = GermanCreditTrain, frac = 1)
svmTuneGrid <- data.frame(sigma = as.vector(sigDist)[1], C = 2^(-2:7))

### Optional: parallel processing can be used via the 'do' packages,
### such as doMC, doMPI etc. We used doMC (not on Windows) to speed
### up the computations.

### WARNING: Be aware of how much memory is needed to parallel
### process. It can very quickly overwhelm the available hardware. We
### estimate the memory usage (VSIZE = total memory size) to be 
### 2566M/core.

library(doMC)
registerDoMC(4)

set.seed(1056)
svmFit <- train(Class ~ .,
                data = GermanCreditTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneGrid = svmTuneGrid,
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5,
                                         classProbs = TRUE))


## Print the results
svmFit

## A line plot of the average performance. The 'scales' argument is actually an 
## argument to xyplot that converts the x-axis to log-2 units.

plot(svmFit, scales = list(x = list(log = 2)))

## Test set predictions

predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)

## Use the "type" option to get class probabilities

predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(predictedProbs)


## Fit the same model using different resampling methods. The main syntax change
## is the control object.

## I've commented out everything here as some of them take ~5 minutes each to complete (or more)
## so feel free to run them but know that they might take awhile

#10 fold cross validation
# set.seed(1056)
# svmFit10CV <- train(Class ~ .,
#                     data = GermanCreditTrain,
#                     method = "svmRadial",
#                     preProc = c("center", "scale"),
#                     tuneGrid = svmTuneGrid,
#                     trControl = trainControl(method = "cv", number = 10))
# svmFit10CV

#leave one out cross validation
# set.seed(1056)
# svmFitLOO <- train(Class ~ .,
#                    data = GermanCreditTrain,
#                    method = "svmRadial",
#                    preProc = c("center", "scale"),
#                    tuneGrid = svmTuneGrid,
#                    trControl = trainControl(method = "LOOCV"))
# svmFitLOO

#repeated training/test set splitting with 80% training set
# set.seed(1056)
# svmFitLGO <- train(Class ~ .,
#                    data = GermanCreditTrain,
#                    method = "svmRadial",
#                    preProc = c("center", "scale"),
#                    tuneGrid = svmTuneGrid,
#                    trControl = trainControl(method = "LGOCV", 
#                                             number = 50, 
#                                             p = .8))
# svmFitLGO 

#bootstrapping
# set.seed(1056)
# svmFitBoot <- train(Class ~ .,
#                     data = GermanCreditTrain,
#                     method = "svmRadial",
#                     preProc = c("center", "scale"),
#                     tuneGrid = svmTuneGrid,
#                     trControl = trainControl(method = "boot", number = 50))
# svmFitBoot

#632 bootstrap method
# set.seed(1056)
# svmFitBoot632 <- train(Class ~ .,
#                        data = GermanCreditTrain,
#                        method = "svmRadial",
#                        preProc = c("center", "scale"),
#                        tuneGrid = svmTuneGrid,
#                        trControl = trainControl(method = "boot632", 
#                                                 number = 50))
# svmFitBoot632

################################################################################
# choosing Between Models

#fit a logistic regression model with repeated cross validation
set.seed(1056)
glmProfile <- train(Class ~ .,
                    data = GermanCreditTrain,
                    method = "glm",
                    trControl = trainControl(method = "repeatedcv", 
                                             repeats = 5))
glmProfile

#predict for both models on the same resampled values
resamp <- resamples(list(SVM = svmFit, Logistic = glmProfile))
summary(resamp)

#Statistical method for comparing methodologies based on resampling results 
#Since the accuracies were measured using identically resampled data sets, 
#statistical methods for paired comparisons can be used to determine if the 
#differences between models are statistically significant


modelDifferences <- diff(resamp)
summary(modelDifferences)

## The actual paired t-test:
modelDifferences$statistics$Accuracy

#no statistical significant difference on the prediction from the SVM vs the logistic regression
#as a result I would liely choose the logistic regression as my 'final' model
#bc it being the less complex model and the ability to maintain interperability


