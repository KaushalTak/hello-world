# Clear Environment Variables
rm(list=ls(all=TRUE))

# Set Woring Directory
setwd("C:/Users/Ashish/Desktop/Corporate Training/Cognizant Batch 05/20160719_AR")

# Load Data from csv
binnedData =read.csv("annealingbinnnedData.csv")

# Load required library
require(arules)

# rules with rhs containing Target only
Target="class"

# getting the target attribute levels
classlevels = names(table(binnedData[,Target]))

#Appending the levels to the target attribute "class"
rhslevels = as.character()
for ( i in 1:length(classlevels)){
  rhslevels=c(rhslevels,paste(Target,classlevels[i],sep="="))
    
} 
# rhslevels = paste("class",classlevels,sep="=")

# Rules generation
require(arules)
Assocrules = data.frame()

# Generate the rules fo each level of target attribute
for ( i in 1:length(classlevels)){
  # appearance decides how the patterns look like
  # default = "lhs" means generate the lhs rules with other than the attributes mentioned in rhs="",
 # minlen is 1. This means that rules with only one item 
  #(i.e., an empty antecedent/LHS) like {} => {class} will be created
  # If you want to avoid these rules then use the argument parameter=list(minlen=2)
  
    rules <- apriori(binnedData,
                   parameter = list(minlen=2, maxlen=4,supp=0.02, conf=0.5,target="rules"),
                   appearance = list(rhs=rhslevels[i],default="lhs"),
                   control = list(verbose=T))
  
  rules1 = as(rules,"data.frame")
  rules1 = subset(rules1,confidence>0.5)
  Assocrules = rbind(Assocrules,rules1)
}

# rearranging the rules as lhs an rhs as separate columns by splitting these rules at =>

# install.packages("stringr")
require(stringr)

# Split rule to LHS and RHS
m=str_split(Assocrules$rules,"=>")
Class = data.frame(Class = unlist(lapply(m,function(x){str_trim(x[2])})))
  
Rule = data.frame(Rule = unlist(lapply(m,function(x){str_trim(x[1])})))
rules2= data.frame(Assocrules,Rule,Class)

rules2 = rules2[,-(which(colnames(rules2)=="rules"))]
Rulesset = unique(rules2)

write.csv(Rulesset,"Rulessetannealingdata.csv")

  
