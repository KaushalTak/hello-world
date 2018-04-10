rm(list=ls(all=TRUE))

setwd("C:/INSOFE (WORK-MAY 2016)/COGNIZANT ACTUAL/DATA PP/20160602_Cog_Bangalore_CSE9098_Day04_DP_Class")

setwd(choose.dir())
# Reading the csv files into R environment
data <- read.csv(file="dataMerged.csv",
                 header=TRUE,sep=",")

# Reading other formats into R environment
read<-read.table("greek.txt",sep="\t",header=T)

# Reading excel file into R environment (NEED TO RESOLVE) 
#install.packages("XLConnect")
library(XLConnect) 
#library(rJava)
wb<-loadWorkbook("datampg.xlsx")
abc <- readWorksheet(wb,"Sheet1",header=T)
library(xlsx)
write.xlsx(abc,"excel_mpg.xlsx",row.names=F)

# Study the dataset "dataMerged.csv"
str(data)
names(data) #also colnames(data)
dim(data) # also nrow(data), ncol(data)
summary(data)
data_ext <-data #to be used later
head(data)

# To view the first 'n' records in the data frame 
head(data,2)
tail(data,10)

# To view records 30 to 50 and columns 1 to 10 in the data frame 
data[30:50,1:10]

# Taking random 10 records
data[sample(1:nrow(data),10),]

##Saving the work space as image##
#a.  Saving workspace
save.image() / save.image("Save_20160602.RData")
#b.	How do save only a few variables from environment- 
save(data,read,file="amit.RData")

load("amit.RData")
# Identify categorical attributes and convert them into factors
# family,edu,securities,cd,online,cc,infoReq,loan

# Method1 - convert each attribute independently
data$family<- as.factor(data$family)
data$edu <- as.factor(data$edu)
data$securities <- as.factor(data$securities)
data$cd <- as.factor(data$cd)
data$online <- as.factor(data$online)
data$cc <- as.factor(data$cc)
data$infoReq <- as.factor(data$infoReq)
data$loan <- as.factor(data$loan)
str(data)


# Method2 - seperate numeric and categorical attribute and convert it all at once
# subsetting data
data_num <- subset(data_ext,select=c(age,exp,inc,ccAvg,mortgage))
data_cat <-subset(data_ext,select=-c(age,exp,inc,mortgage,ccAvg))

# Convert the seperated categorical attributes into factors
data_cat <- data.frame(apply(X = data_cat, MARGIN = 2, FUN = function(x){as.factor(x)}))

# Merge the converted categorical attributes and numerical attributes  
data_combined <-cbind(data_num,data_cat)

# MISSING VALUES HANDLING (IMPUTATION ETC.)

# a) Total numbers of cells that have missing values or nulls 
sum(is.na(data_combined))

# b) Check if the rows with missing values (1 row can have many NAs)
s <- data_combined[!complete.cases(data_combined),]

# c) Dropping the records with missing values
data_comb_omitted <- na.omit(data_combined)
dim(data_comb_omitted)
sum(is.na(data_comb_omitted))

# d) Identify & remove rows where more than 10% attributes are missing
# only for demonstration (we will not be using this)
library(DMwR)
length(manyNAs(data_combined, 0.25))
data_refined <- data_combined[-manyNAs(data_combined, 0.1),]
dim(data_refined) 

# e) check columns with high % of NAs 
apply(data_combined,2,function(x)sum(is.na(x))/length(x)) #(nothing to remove)

# f) Impute using Central Imputation (try on a subset of original data)
#install.packages("DMwR")
#library(DMwR)

# create a subset of the original dataset to try the central imputation
data_subset <- data_combined[1:100,1:6]
sum(is.na(data_subset))

# apply central imputation
data_Cimp <- centralImputation(data = data_subset)
# For numeric variable , the replacement is done using "mean",
# For categorical ,the replacement done using "mode"
sum(is.na(data_Cimp))

# apply knn imputation on the original dataset
data_knnimputed <-knnImputation(data_combined,k=5,scale=T)
sum(is.na(data_combined))
sum(is.na(data_knnimputed))

# WRITE FUNCTION
write.csv(data_knnimputed, "imputed_file.csv", row.names=FALSE)

### DESCRETIZE  (convert from numeric to categorical)
#select numerical attributes
#install.packages("infotheo")
library(infotheo)

age_binned <- discretize(data_knnimputed$age, 
                     disc="equalfreq",nbins=4)
table(age_binned) #shows the no. of records bucketed in each bin
str(age_binned)
class(age_binned)

age_binned <- as.factor(age_binned$X)
str(age_binned)
age_binned<-as.data.frame(age_binned)
str(age_binned)

#combine binned age attr with the main dataset & remove the original age
data_binned <- cbind(data_knnimputed,age_binned)

# delete "age" and rename "age_binned" to "Age"
data_binned <- data_binned[,-1]
colnames(data_binned)[13] <- "Age"
table(data_binned$Age)

# alternate way 1 of descretizing - manual coding 
# (example taken for another attribute) 
summary(data_knnimputed$age)
data_knnimputed$ageNew<-0
for (i in 1:nrow(data_knnimputed)){
  if (data_knnimputed$age[i]>=45){ 
    data_knnimputed$ageNew[i]=2
  }
  else {
    data_knnimputed$ageNew[i]=1
  }
}
table(data_knnimputed$ageNew)
abc <- data_knnimputed[data_knnimputed$ageNew==1,]
abc
head(abc)
tapply(data_knnimputed$inc,data_knnimputed$ageNew,min) 
tapply(data_knnimputed$inc,data_knnimputed$ageNew,max) 

# alternate way 2 of descretizing by avoiding for-loops
xyz <- sapply(data_knnimputed$age,function(x)ifelse(x>=45,2,1))
table(xyz)

# alternate way 3 of descretizing by using ifelse
rst <-ifelse(data_knnimputed$age<45,"1","2")
# rst <-ifelse(data_knnimputed$age<25,"young",ifelse(data_knnimputed$age<50,"middle-age","old"))
table(rst)

# alternate way 4 of descretizing by using cut
range(data_knnimputed$age)
def <- cut(x =data_knnimputed$age,breaks = c(23,35,45,70))
table(def)

### DUMMIFY (convert from categorical to numeric)
# dummify select categorical attributes 
library(dummies)
data_binned$edu
table(data_binned$edu) #check similarly for infoReq and family
# dummify "edu", "family" and "infoReq" - all at once
data_dummified <-apply(data_binned[,c(5,6,11)],2,FUN = function(x){dummy(x)})
data_dummified <-data.frame(data_dummified)
colnames(data_dummified) <-c("Family1","Family2","Family3","Family4",
                             "edu1","edu2","edu3","inforeq1","inforeq2")
names(data_dummified)
#combine dummified attributes with main data & remove original undummified attributes
dummified_final <-cbind(data_binned[,-c(5,6,11)],data_dummified)
head(dummified_final)

# dummify "edu", "family" and "infoReq" individually
# data1<-dummy(data_binned$edu)
# data2<-dummy(data_binned$family)
# data3<-dummy(data_binned$infoReq)
# dummified_final1<-cbind(data_binned[,-c(5,6,11)],data1,data2,data3)
# or dummified_final2 <-data.frame(data_binned[,-c(5,6,11)],data1,data2,data3)

### STANDARDIZE THE DATASET
#install.packages("vegan")
library(vegan)
str(dummified_final)

# extract numerical attributes that need to be standardize
data_for_std <-dummified_final[,c(1,3,4)]

# standardize using the range method
data_std <-decostand(x=data_for_std,method="range")

# seperate the rest of the attributes from the main data set (no standardization)
# "inc" which can be considered as a target variable is not standardized
data_rest <-dummified_final[,-c(1,3,4)]

# combining the standardized attributes with the rest of the attributes
data_final <-cbind(data_std, data_rest)

# Using z-score method
#"inc" is selected as it is the only attribute left to demonstrate
data_std_inc <- decostand(x = data_final$inc,method = "standardize") 
data_std_inc <- as.data.frame(data_std_inc)
colnames(data_std_inc) <- ("income")
data_final1 <-cbind(data_final[,c(-4)],data_std_inc)

# can use this data for models like linear regression 
#summary(lm(inc~.,data=data_final))
#summary(model)$coefficients[1,1] #need to assign to "model" first
#summary(model)$coefficients[1,2]
##################################################################

# Merging 2 datasets
rm(list=ls(all=TRUE))

Grade1<-read.csv("Grade1.csv",header=T,sep=",")
Grade2<-read.csv("Grade2.csv",header=T,sep=",")
str(Grade1$Student.id)
# we observed above that the structure for student.id is numeric.
# does it make sense to have statistical summary of it when data it is loaded in R,
# by default R assigns a data type to it. If we find it inappropriate we manually change it
Grade1$Student.id<-as.character(Grade1$Student.id) # converts into a character variable

## we have two data sets of Grades for students. We observe that student.id 
## is common in both we want to merge these two data sets into one.
merged_grades <-merge(Grade1,Grade2) ##There are other forms of merging like left join/outer join,inner join etc. 
## Explore the possible joins on other data sets

#################
#APPLY FUNCTIONS
################
attach(mtcars)
data<-mtcars

## find max value for each column
## we can also find the stats for each of the variable separately
apply(data[,2:11],2,min) #This generates the min values for each numeric attribute

## write this to a data frame
A<-apply(data[,2:11],2,min) #outputs a list
A<-data.frame(minimum=apply(data[,2:11],2,min)) #try max, mean

# if we want to have all stats in a data frame we can write a customize function
stat<-function(x){
  "Mean"=mean(x)
  "Min"=min(x)
  "Max"=max(x)
  A<-data.frame(Min,Mean,Max)
  return(A)
}
statistics <-apply(data[,2:11],2,FUN=stat) # observe O/p...it is a list

lappy<-lapply(data[,2:11],mean);lappy #outputs a list, nor margin needed  
sappy<-sapply(data[,2:11],mean);sappy #simplified output

# find mean mileage for each cylinder types
tappy<-tapply(mtcars$mpg,mtcars$cyl,mean);tappy # outputs values and not dataframe
# tapply gives a table wrt to a categorical attribute as well,i.e. (num,cat,function)

# Train and Test 
rows <-seq(1,nrow(data),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(data))
Train<-data[trainrows,]
Test<-data[-trainrows,]
##By using a package caTools
require(caTools)
set.seed(123) 
sample = sample.split(data$gear, SplitRatio = .75)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

#####RESHAPE#####
library(reshape2)
library(reshape)

# read the dataset
Cust <-read.csv("CustTransDat.csv",header=T,sep=",")
dim(Cust)
str(Cust)
table(Cust$Quarter)

# reshape is used for better representation of data. For example we
# have an a transaction data of customers for a store. We would like
# to know the revenue generated by year and by quarter. 

data2 <-dcast(Cust,Quarter~Year,fun.aggregate = sum,value.var="Cost");data2

# using the melt function for anotherdataset
Grade <-read.csv("Grade1.csv",header=T,sep=",")
meltdata<-melt(data=Grade,id="Student.id")
head(meltdata)

# aggregating the data based on subject and gender. This is also called the wide format.
data2<-dcast(data=meltdata,Student.id~variable,value="value") 
data3<-dcast(data=meltdata,variable~Student.id,value="value",fill=0) 

###Resources for Reshape- http://seananderson.ca/2013/10/19/reshape.html




