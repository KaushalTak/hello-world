###############################################################################

## Logistic Regression.
## Data : Customer Data - Whether the customer churn or not.
##                        3209 Observations, 14 variables.

## Steps to carry out the regression.

## 1. Import and summarize data.

## 2. Build a simple logistic regression model - using 1 independent variable.

## 3. Split the data into training and testing datasets.

## 4. Fit a multiple variable logistic regression model - Using all significant variables.

## 5. Predict values on training and testing datasets.

## 6. Use confusion matrix and ROC to check the model performance.

###############################################################################
rm(list = ls(all=TRUE))
setwd("E:/rahul/Cognizant_Bangalore_Training/day_8/Logistic_Regression_Activity")
#install.packages("MASS")
library(MASS)

## 1. Import Data.
customer_Data <- read.csv(file = "CustomerData.csv", header = T)

## attach the dataset "customer_Data".
attach(customer_Data)

## Understand the structure of the data.
str(customer_Data)

## Look for missing values in the CustomerData dataset. No missing values found.
sapply(customer_Data, function(x) sum(is.na(x)))


## Pre-Process Data.
## Drop the attribute "CustomerID".
customer_Data <- customer_Data[,-c(1)]

## Convert "City" as factor variable.
customer_Data$City <- as.factor(as.character(customer_Data$City))

## Target attribute is : Churned. Convert this as factor variable.
customer_Data$Churned <- as.factor(as.character(customer_Data$Churned))

## 2. Build a simple model using "FrequencyOFPlay" attribute.
## The only variable frequency of play has very small estimate. 
## So this variable doesn't help us in building the model.
simple.model <- glm(Churned~FrequencyOFPlay, data = customer_Data, family = binomial())
summary(simple.model)

# model is not very good as Null deviance is not much different
# from the Residual deviance, also variable is not
# significant

## We stored our output in simple.model
## By specifying type = response, We instruct R to response with the probabilities 
## using the specified model.
simple.model.pred.prob <- predict(simple.model, type = "response")


## Build Comprehensive model.
## 3. Split data into training and validation datasets.
set.seed(123)
## list of random row-ids from the customer_Data dataset.  
sub_Seq         <- sample(nrow(customer_Data), floor(nrow(customer_Data) * 0.7))
training_data   <- customer_Data[sub_Seq,]
validation_data <- customer_Data[-sub_Seq,]

## Verify training and validation datasets.
## Churned Yes - 1141 : No - 1105
summary(training_data)
## Churned Yes - 498  : No - 465
summary(validation_data)

## 4. Build a more comprehensive model
model.new <- glm(Churned~., data = training_data, family = binomial())
summary(model.new)

## Explore the model by dropping the insgificant variables with-out hurting the model.
## Perform step wise regressing using stepAIC function.
model.step <- stepAIC(model.new)
summary(model.step)

## Use model.step to predict the churned or not for training and validation datasets.
training_data$pred.prob     <- predict(model.step, type = "response")
validation_data$pred.prob   <- predict(model.step, type = "response", newdata = validation_data)

## Confusion Matrix
## Convert the probabilty values to 1 and 0 -
## 0.5 - Arbitrary choice guided by chart - 
training_data$class   <- as.factor(ifelse(training_data$pred.prob > 0.5, 1, 0))
validation_data$class <- as.factor(ifelse(validation_data$pred.prob > 0.5, 1, 0))


conf.train <- table(training_data$Churned, training_data$class)
conf.valid <- table(validation_data$Churned, validation_data$class)

conf.train
conf.valid

## Plot ROC curve
## The ROC curve is used to visualize the performance of the model.
## The area under this curve - represents how good is the model is, 
## the closer the curve to 100% true positive rate. Area under curve is ~52%.
library(ROCR)
train_pred <- prediction(training_data$pred.prob, training_data$Churned)
train_perf <- performance(train_pred, measure = "tpr", x.measure = "fpr")
plot(train_perf)
     
validation_pred <- prediction(validation_data$pred.prob, validation_data$Churned)
validation_pref <- performance(validation_pred, measure = "tpr", x.measure = "fpr")
plot(validation_pref) 
          
# AUC for training data
auc.tmp <- performance(train_pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
auc       

# AUC for testing data
auc.tmp <- performance(validation_pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
auc  
          
## training accuracy
training_accuracy <- sum(diag(conf.train))/nrow(training_data) * 100
training_accuracy

## validation accuracy
validation_accuracy <- sum(diag(conf.valid))/nrow(validation_data) * 100
validation_accuracy

