##########################################

##### Logistic Regression.
##### Data: Credit card default data - inbuilt in R in ISLR package, 
#####       10000 observations, 4 variables.

##### Steps to carry out the regression.

# 1. Import and summarize data.

# 2. Build a simple logistic regression model - using 1 independent variable.

# 3. Split the data int training and testing datasets.

# 4. Fit a multiple variable logistic regression model - Using all significant variables.

# 5. Predict values on training and testing datasets.

# 6. Use confusion matrix and ROC to check the model performance.


rm(list=ls(all=TRUE))
setwd("E:/rahul/Cognizant_Bangalore_Training/day_8/Logistic_Regression_Activity")

# 1. Import data
##install.packages("ISLR")
## Load ISLR package - This package contains the "default" dataset we are looking for.
library(ISLR)
library(MASS)

## attach the dataset "Default"
attach(Default)

## Understand the structure of the data.
?Default
summary(Default)
str(Default)

## Look for missing values in the  dataset. No missing values found.
# if missing values found, either delete the rows or do imputation
# using functions from DMwR library

sapply(Default,function(x) sum(is.na(x)))

# Standardize the numerical variables 

num_data <- Default[, names(Default) %in% c("balance", "income") ]

cat_data <- Default[, names(Default) %in% c("default", "student") ]

library(vegan)

num_data <- decostand(num_data , method = "range")

my_data <- cbind(cat_data , num_data)

rm(cat_data, num_data)

detach(Default)

## 2. Build a simple model using "income" attribute.
## The only variable income has very small estimate. 
## The siginificance of this variable also indicates that very small 0.471
## So this variable doesn't help us in building the model.
simple.model <- glm(default~income, data = my_data, family = binomial())
summary(simple.model)

## We stored our output in simple.model
## By specifying type = response, we instruct R to response with the probabilities 
## using the specified model.
simple.model.pred.prob <- predict(simple.model, type = "response")

# 3. Split Data
set.seed(123)
# list of random row-ids from the Default dataset.  
sub_seq <- sample(nrow(my_data), floor(nrow(my_data) * 0.7))
training_data <- my_data[sub_seq,]
validation_data <- my_data[-sub_seq,]

## Verify training and validation datasets.
## Default No : 6754  : Yes - 246
summary(training_data)
## Default No : 2913  : Yes - 87
summary(validation_data)

# 4. Build a more comprehensive model
## student and balance are significant.
## income is in-significant.
model.new <- glm(default~., data = training_data, family = binomial())
summary(model.new)


## Explore the model by dropping the insignificant variables with-out hurting the model.
## Perform step wise regressing using stepAIC function.
model.step <- stepAIC(model.new)
summary(model.step)


## Use model.step to predict the default or not for training and validation datasets.
training_data$pred.prob <- predict(model.step, type = "response")
validation_data$pred.prob <- predict(model.step, type = "response", newdata = validation_data)

## Confusion Matrix
## Convert the probabilty values to 1 and 0 -
## Take a cut-off point as 0.5 to get a quick estimate of how the model is performing -

training_data$class <- ifelse(training_data$pred.prob > 0.5, 1, 0)
validation_data$class <- ifelse(validation_data$pred.prob > 0.5, 1, 0)

conf.train <- table(training_data$default, training_data$class)
conf.valid <- table(validation_data$default, validation_data$class)

conf.train

conf.valid

## Plot ROC curve
## The ROC curve is used to visualize the performance of the model.
## The area under this curve - represents how good is the model is, 
## the closer the curve to 100% true positive rate.
library(ROCR)
train_pred <- prediction(training_data$pred.prob, training_data$default)
train_perf <- performance(train_pred, measure = "tpr", x.measure = "fpr")
plot(train_perf)

validation_pred <- prediction(validation_data$pred.prob, validation_data$default)
validation_pref <- performance(validation_pred, measure = "tpr", x.measure = "fpr")
plot(validation_pref) 

# AUC for train data
auc.tmp <- performance(train_pred,"auc")

auc <- as.numeric(auc.tmp@y.values)

auc

# AUC for validation data

auc.tmp <- performance(validation_pred,"auc")

auc <- as.numeric(auc.tmp@y.values)

auc
## TN FP
## FN TP
## Compute error metrics precision, accuracy and recall
## For training data.
## Calculate Accuracy Percentage
## What percent of your predictions were correct? TN + TP / TOTAL
accuracy <- (sum(diag(conf.train))/sum(conf.train))*100
cat("accuracy is --->", accuracy)

## Calculate Recall Percentage
## What percent of the positive cases did you catch? TP / FN + TP
recall=(conf.train[2,2]/(conf.train[2,1]+conf.train[2,2]))*100
cat("recall is --->", recall)

## Calculate Precission Percentage
## What percent of positive predictions were correct? TP / TP + FP
precision=(conf.train[2,2])/(conf.train[1,2]+conf.train[2,2])*100
cat("precision is --->", precision)

## Compute error metrics precision, accuracy and recall
## For validation data.
## Calculate Accuracy Percentage
## What percent of your predictions were correct? TN + TP / TOTAL
accuracy <- (sum(diag(conf.valid))/sum(conf.valid))*100
cat("accuracy is --->", accuracy)

## Calculate Recall Percentage
## What percent of the positive cases did you catch? TP / FN + TP
recall=(conf.valid[2,2]/(conf.valid[2,1]+conf.valid[2,2]))*100
cat("recall is --->", recall)

## Calculate Precission Percentage
## What percent of positive predictions were correct? TP / TP + FP
precision=(conf.valid[2,2])/(conf.valid[1,2]+conf.valid[2,2])*100
cat("precision is --->", precision)
