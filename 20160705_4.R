
rm(list=ls(all=TRUE))

# 1) Create a function which takes vector as an input and
# return sine of those values

myfun <- function(x){
        return(sin(x))
}

# 2) Take data set CustomerData1 and remove NA values and store that
# into variable mydata. Check structure of the data

data <- read.csv("CustomerData1.csv")

sum(is.na(data))

mydata <- data[complete.cases(data), ]

# 3) First column is not useful, so remove and store result
# back into variable mydata

mydata <- mydata[,-1]

# 4) Convert the variable City into factor

mydata$City <- as.factor(mydata$City)

# 5) For mtcars dataset, find mean of hp for each group of
# gear

tapply(mtcars$hp, mtcars$gear, mean)

# 6) Plot histogram of variable Revenue in mydata

hist(mydata$Revenue)

# 7) Create dummy variables for City which has been converted
# into a factor. Put the dummy variables and other variables
# in datafram mydata2

library(dummies)

mycity <- dummy(mydata$City)

mydata2 <- data.frame(mydata, mycity)

# 8 ) Remove original City variable from the mydata2

mydata2$City <- NULL

# 9) Use discretize function from infotheo library and
# use it to discretize Revenue variable in 5 bins. Use
# equalfreq as value for the paramter disc

library(infotheo)

RevenueBin <- discretize(mydata2$Revenue, disc="equalfreq",nbins=5)

# 10) Use tapply function to get the median of each of the 5 bins
# of Revenue

tapply(mydata2$Revenue,RevenueBin,median)
