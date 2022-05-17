# 과제 3번
# Importing data
data = read.csv("c:/data/datamining/assignment3.csv", header=TRUE)

# Factorize for classification
data$Y = factor(data$Y)
data$X1 = factor(data$X1)
data$X2 = factor(data$X2)

# Classification Tree
library(rpart)
my.control = rpart.control(maxdepth=1, minsplit = 5, xval=10, cp=0.01)
tree.data = rpart(Y~., data=data, method="class", control=my.control)
print(tree.data)
# Display tree
library(rpart.plot)
prp(tree.data, type=4, extra=1, box.palette="Grays")