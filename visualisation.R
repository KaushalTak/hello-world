rm(list=ls())

setwd("E:\\rahul\\Cognizant_Bangalore_Training\\day_5")
library(ggplot2)

# Plots are made up of aesthetics (size, shape, color) 
#  and geoms(points, lines)

str(mpg)

# qplot is similar to plot from Base R

qplot(displ, hwy, data = mpg)

# we can highlight subgroups by color

qplot(displ, hwy, data = mpg, color = drv)

# Adding a geom

qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

# Histogram

qplot(hwy, data = mpg, fill = drv)

# Facets ( kinds of panel plots)

qplot(displ, hwy, data = mpg, facets = . ~ drv)

qplot(hwy, data = mpg, facets = drv ~ . , binwidth = 2)

# ggplot
str(diamonds)
head(diamonds$cut)
head(diamonds)

# p is a ggplot object
p <- ggplot(diamonds, aes(cut))

# creating bar chart for categorical variable
p + geom_bar( )

p <- ggplot(diamonds, aes(x = clarity))
p + geom_bar()

# creating stack bar chart
p <- ggplot(diamonds, aes(x = clarity, fill = cut))
p + geom_bar()


# http://www.ats.ucla.edu/stat/data/binary.csv

admit <- read.csv("binary.csv")

admit$testscores <- admit$gre

admit$gre <- NULL

plot <-  ggplot(data = admit, aes(x=gpa, y=testscores, col=rank))

plot <- plot + geom_point(size = 3)

plot <- plot + xlab("GPA") +ylab("Test Scores") + ggtitle("GPA vs Test Scores")

plot

# Above we have continuous color scheme, we can make it discrete color scheme

plot <-  ggplot(data = admit, aes(x=gpa, y=testscores, col=factor(rank)))

plot <- plot + geom_point(size = 3) +
  scale_color_discrete(name = "Rank")

plot <- plot + xlab("GPA") +ylab("Test Scores") + ggtitle("GPA vs Test Scores")

plot

# Adding another dimension of the data in the form of shape

plot <-  ggplot(data = admit, aes(x=gpa, y=testscores, col=factor(rank)))

plot <- plot + geom_point(aes(shape = factor(admit)), size = 3) +
  scale_color_discrete(name = "Rank") +
  scale_shape_discrete(name = "Admit", labels = c("No", "Yes"))

plot <- plot + xlab("GPA") +ylab("Test Scores") + ggtitle("GPA vs Test Scores")

plot

a = requests.get('http://d.yimg.com/aq/autoc?query=vodafone%20idea&region=US&lang=en-US')
