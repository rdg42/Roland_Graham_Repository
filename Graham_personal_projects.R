# Author: Roland Graham
# MQM Data Competition

# Project 1: Using Neural Networks to Identify Banknote Forgeries

# Loads the neuralnet library
library(neuralnet)

# Reads in the dataset and gives the columns names
data = read.delim("data_banknote_authentication.txt", header = FALSE, sep = ",")
colnames(data)[1] = "variance"
colnames(data)[2] = "skewness"
colnames(data)[3] = "kurtosis"
colnames(data)[4] = "entropy"
colnames(data)[5] = "class"

# Standardizes the explanatory variables to assure they are measured on the 
# same scale
data$variance = (data$variance - min(data$variance)) / (max(data$variance) - min(data$variance))
data$skewness = (data$skewness - min(data$skewness)) / (max(data$skewness) - min(data$skewness))
data$kurtosis = (data$kurtosis - min(data$kurtosis)) / (max(data$kurtosis) - min(data$kurtosis))
data$entropy = (data$entropy - min(data$entropy)) / (max(data$entropy) - min(data$entropy))

# Shuffles the dataset so the training and testing segments get a mix of 
# positive and negative responses for forgeries
set.seed(35)
shuffled.data = data[sample(1:nrow(data)), ]

# Divides the dataset into training and testing segments
set.seed(35)
sample <- sample(c(TRUE, FALSE), nrow(shuffled.data), replace=TRUE, prob=c(0.7,0.3))
train  <- shuffled.data[sample, ]
test   <- shuffled.data[!sample, ]

# Creates and plots the neural network with two hidden layers and three 
# hidden nodes
set.seed(35)
net = neuralnet(class ~ ., train, hidden = c(3,2))
plot(net)

# Cross-validates the neural net on the testing data and creates a confusion 
# matrix
pred = compute(net, test)
cross.val = data.frame(actual = test$class, prediction = pred$net.result)
cross.val.round = sapply(cross.val, round, digits = 1)
cross.val.round.df = data.frame(cross.val.round)
attach(cross.val.round.df)

# Prints the confusion matrix
table(actual, prediction)

################################################################################

# Project 2: Using Hierarchical Clustering to Identify Mall Customer Segments

# Loads the factoextra library
library(factoextra)

#Runs the data and adds simpler column names to the Income and Spending Score 
# variables
data = read.csv("Mall_Customers.csv", header = TRUE)
colnames(data)[4] = "Income"
colnames(data)[5] = "Score"

# Removes the CustomerID and Gender variables
# Note: We have to remove Gender because hierarchical clustering only works with
# continuous variable types
data = data[,3:5]

# Standardizes the variables
data$Age = (data$Age - min(data$Age)) / (max(data$Age) - min(data$Age))
data$Income = (data$Income - min(data$Income)) / (max(data$Income) - min(data$Income))
data$Score = (data$Score - min(data$Score)) / (max(data$Score) - min(data$Score))

# Uses the gap stat method to determine the ideal number of clusters
# The plot shows the optimal number is k = 5 clusters
set.seed(35)
fviz_nbclust(data, hcut, method = "gap_stat")

# Calculates the average distance for each hierarchical cluster
distance = dist(data, method = "euclidean")
hier.average = hclust(distance, method = "average")

# Creates and plots a dendrogram to show the two clusters
plot(hier.average)
rect.hclust(hier.average, k = 5)
abline(h = 5, col = "red")

# Identifies which variables are in which cluster
cluster.cut = cutree(hier.average, k = 5)

# Adds the cluster identifier to the dataset
data$Cluster = cluster.cut

# Note: In order to un-standardize the data, I reran the code from lines 
# 131-138, then ran line 163 again

# Shows the summary statistics for the first cluster
summary(data$Age[data$Cluster == 1])
summary(data$Income[data$Cluster == 1])
summary(data$Score[data$Cluster == 1])

# Shows the summary statistics for the second cluster
summary(data$Age[data$Cluster == 2])
summary(data$Income[data$Cluster == 2])
summary(data$Score[data$Cluster == 2])

# Shows the summary statistics for the third cluster
summary(data$Age[data$Cluster == 3])
summary(data$Income[data$Cluster == 3])
summary(data$Score[data$Cluster == 3])

# Shows the summary statistics for the fourth cluster
summary(data$Age[data$Cluster == 4])
summary(data$Income[data$Cluster == 4])
summary(data$Score[data$Cluster == 4])

# Shows the summary statistics for the fifth cluster
summary(data$Age[data$Cluster == 5])
summary(data$Income[data$Cluster == 5])
summary(data$Score[data$Cluster == 5])