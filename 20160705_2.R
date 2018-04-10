# Its good practice to first clear the workspace

rm(list=ls(all=TRUE))

# 1) 1)	Create an object which is a  sequence of numbers 
  #  from 100 to 200 in the steps of 5 and check its class and mode

x <- seq(100,200,5)

# 2 ) 	Use the vector created above to create a 
# matrix which has 7 rows and 3 columns. Check the dimensions of the matrix

my_matrix <- matrix(x, nrow = 7, ncol = 3)

# 3)	Create a dataframe where both the columns are vector
#    from part 1. Use cbind function.

my_data <- cbind(x1 = x, x2 = x)

# 4)	Save the dataframe object created in part 3 in the file "test.RData". 

save(my_data, file = "test.RData")

# 5)	Get your current working directory

getwd()

# 6)	Remove the object created in part 2

rm(my_matrix)

