



# 1) Clear workspace. remove all objects

rm(list=ls(all=TRUE))

# 2) Set working directory. You should have "Data" directory
# under this working directory

setwd("F:/insofe_official/Batch 19/CSE_7112_new")

# 3) Read file Grade2.csv from "Data" directory. Call this object
# mydata

mydata <- read.csv("./Data/Grade2.csv")

# 4) check head and tail and structure of the mydata

head(mydata)

tail(mydata)

str(mydata)

# 5) Create a new variable which is sum of Math2 and Science2 
# scores. Call it math_science

mydata$math_science <- mydata$Math2 + mydata$Science2

# 6) check missing values and number of rows

nrow(mydata)

sum(is.na(mydata))

# 7) store this data frame in anoter variable. call it mydata2

mydata2 <- mydata

# 8) Student ID is not useful for analysis, so remove it from
# mydata2

mydata2$Student.id <- NULL

# 9) take subset of mydata2 so that only those students having
# Math2 score of greater than 50 will be listed

subset(mydata2, Math2 > 50)

# 10) give math_science scores of those students who have
# less than 60 marks in English2

subset(mydata2, English2 < 60 , select = c(math_science))

# 11) List rows where English2 marks are less than 60 or
# OverallPct2 marks are greater than 70

subset(mydata2, English2 < 60 | OverallPct2 > 70)

# 12) Drop variable math_science from mydata2

subset(mydata2, select = -c(math_science))

# mydata2$math_science <- NULL

###################################
# Working with vectors
###################################

# 13) Store vector c(12,0,-1,49,22,15,18) in a variable x and
# get its length

x <- c(12,0,-1,49,22,15,18)
length(x)

# 14) Give the elements in x which are greater than 20

x[ x > 20]

# 15) Give elements in x which are greater than 15 and
# less than 30

x[x > 15 & x < 30]

# 16) Find the index of the maximum and minimum element

which.max(x)
which.min(x)

# 17) Consider the vector c(NA, 1, 34, 0, NA, 15, 19). Input 
# this vector in variable x

x <- c(NA, 1, 34, 0, NA, 15, 19)

# 18) Check how many missing values are there

sum(is.na(x))

# 19) Repalce NA values with number 999

x[ is.na(x)] <- 999


