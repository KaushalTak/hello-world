rm(list=ls(all=TRUE))

# To specify seeds
set.seed(8953)

# Taking iris data
data <- iris
head(data)

# Drop class labels (Species)
data$Species <- NULL
head(data)

# Running kmeans 
clus <- kmeans(data, 3)
clus

clus$cluster

table(clus$cluster)
clus$size




# Calculation of betweenss

clus$betweenss

# Calculation of withinss

clus$withinss

# Calculation of total withinss

clus$tot.withinss

# Calculation of totalss
clus$tot.withinss +  clus$betweenss
clus$totss

# Check clustering result against class labels (Species)
table(clus$clus, iris$Species)

# Observation
#   Class "setosa" can be easily separated from the other clusters
#   Classes "versicolor" and "virginica" are to a small degree overlapped with each other.

plot(data[c("Sepal.Length", "Sepal.Width")], col = clus$cluster)
points(clus$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3, pch = 8, cex = 2) # plot cluster centers
rm(clus)


# Identifying right number of clusters
tot.wss <- 0
for (i in 1:15) {
  tot.wss[i] <- kmeans(data,centers=i)$tot.withinss
}

plot(1:15, tot.wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Total within groups sum of squares") 
rm(i, tot.wss)
# Cluster may vary based on the where inition cluster center are picked 
# So, re-run the above code multiple time with different set.seed and find the right K


# From the above analysis k = 4 looks better 
# K-Means Cluster Analysis

set.seed(1234)
clus <- kmeans(data, 4) # 4 cluster solution

# get cluster means

# append cluster numbers
data <- data.frame(data, "Cluster" = clus$cluster) 
head(data)

rm(data, clus, x)
 