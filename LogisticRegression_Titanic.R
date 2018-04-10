##########################################

##### Logistic Regression.
##### Data: Titanic dataset - Downloaded from Kaggle. 
#####       Train       891  - 12 

##### Steps to carry out the regression.

# 1. Import and summarize data.

# 2. Build a simple logistic regression model - using 1 independent variable.

# 3. Split the data int training and testing datasets.

# 4. Fit a multiple variable logistic regression model - Using all significant variables.

# 5. Predict values on training and testing datasets.

# 6. Use confusion matrix and ROC to check the model performance.
  

rm(list=ls(all=TRUE))
setwd("E:/rahul/Cognizant_Bangalore_Training/day_8/Logistic_Regression_Activity")
library(MASS)

# 1. Import data
## When working with a real dataset we need to take into account the fact that some data might be 
## missing or corrupted, therefore we need to prepare the dataset for our analysis. 
## As a first step we load the csv data using the read.csv() function.
## Make sure that the parameter na.strings is equal to c("") so that each missing value is coded 
## as a NA. This will help us in the next steps.
my_Data <- read.csv('train.csv', header=T, na.strings=c(""))

## Understand the structure of the data.
summary(my_Data)
str(my_Data)

##  check for missing values and look how many unique values there are for each variable 
## using the sapply() function which applies the function passed as argument to each column 
## of the dataframe.
sapply(my_Data,function(x) sum(is.na(x)))


## A visualization on the missing values might be helpful: 
## the Amelia package has a special plotting function missmap() that will plot your dataset and 
## highlight missing values:
#install.packages("Amelia")
library(Amelia)
missmap(my_Data, main = "Missing values vs observed")

## The variable cabin has too many missing values, better not to use it. 
## We will also drop PassengerId since it is only an index and Ticket. 
## Using the subset() function we subset the original dataset selecting the relevant columns only.
my_Data <-   subset(my_Data,select=c(2,3,5,6,7,8,10,12))


# Missing value imputation
library(DMwR)

my_Data <- centralImputation(my_Data)

sum(is.na(my_Data))

## As far as categorical variables are concerned, using the read.table() or read.csv() by default 
## will encode the categorical variables as factors. A factor is how R deals categorical variables.
## We can check the encoding using the following lines of code
is.factor(my_Data$Sex)
is.factor(my_Data$Embarked)
is.factor(my_Data$Survived)
my_Data$Survived <- as.factor(as.character(my_Data$Survived))
 
# remove the rownames.. 
rownames(my_Data) <- NULL

## Model Building
## We split the data into two chunks: training and testing set. 
## The training set will be used to fit our model which we will be testing over the testing set.
## 3. Split data into training and validation datasets.
set.seed(123)
## list of random row-ids from the customer_Data dataset.  
sub_Seq         <- sample(nrow(my_Data), floor(nrow(my_Data) * 0.7))
training_data   <- my_Data[sub_Seq,]
validation_data <- my_Data[-sub_Seq,]

## Verify training and validation datasets.
## Survived Yes - 237 : No - 385
summary(training_data)
## Survived Yes - 103 : No - 164
summary(validation_data)


## Build the model
model <- glm(Survived~., data = training_data, family = binomial())
summary(model)


## Explore the model by dropping the insignificant variables with-out hurting the model.
## Perform step wise regressing using stepAIC function.
model.step <- stepAIC(model)
summary(model.step)


## Use model.step to predict the default or not for training and validation datasets.
training_data$pred.prob <- predict(model.step, type = "response")
validation_data$pred.prob <- predict(model.step, type = "response", newdata = validation_data)

## Confusion Matrix
## Convert the probabilty values to 1 and 0 -
## 0.5 - Arbitrary choice guided by chart - 
## Take a cut-off point as 0.5 to get a quick estimate of how the model is performing -

training_data$class <- ifelse(training_data$pred.prob > 0.5, 1, 0)
validation_data$class <- ifelse(validation_data$pred.prob > 0.5, 1, 0)

conf.train <- table(training_data$Survived, training_data$class)
conf.valid <- table(validation_data$Survived, validation_data$class)

conf.train
conf.valid

## Plot ROC curve
## The ROC curve is used to visualize the performance of the model.
## The area under this curve - represents how good is the model is, 
## the closer the curve to 100% true positive rate.
library(ROCR)
train_pred <- prediction(training_data$pred.prob, training_data$Survived)
train_perf <- performance(train_pred, measure = "tpr", x.measure = "fpr")
plot(train_perf)

validation_pred <- prediction(validation_data$pred.prob, validation_data$Survived)
validation_pref <- performance(validation_pred, measure = "tpr", x.measure = "fpr")
plot(validation_pref) 

# AUC for training data
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


