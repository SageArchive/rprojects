# 과제 3번
# Importing data
data = read.csv("c:/data/datamining/3-2.csv", header=TRUE)
head(data)

# Factorize for classification
data$Y = factor(data$Y)
data$X1 = factor(data$X1)
data$X2 = factor(data$X2)

# Classification Tree
library(rpart)
set.seed(1234)
my.control = rpart.control(xval=10, cp=0.1, minsplit=5)
tree.data = rpart(Y~., data=data, method="class", control=my.control)
print(tree.data) # 의사결정나무 결과 출력

# Display tree
library(rpart.plot)
prp(tree.data, type=4, extra=1, digits=2, box.palette="Grays") #의사결정나무의 그래프 출력

# Pruning with c-s.e. 가지치기 수행
cps = printcp(tree.data)
gini_value = 1-(18/39)^2-(21/39)^2
gini_value
k = which.min(cps[,"xerror"])
err = cps[k,"xerror"]; se = cps[k,"xstd"]
c = 1 # 1-s.e.
k1 = which(cps[,"xerror"] <= err+c*se)[1]
cp.chosen = cps[k1,"CP"]
tree.pruned.data = prune(tree.data, cp=cp.chosen)
print(tree.pruned.data)
# Display tree
prp(tree.pruned.data, type=4, extra=1, digits=2, box.palette="Grays")

# Making predictions - probability prediction
prob.tree.data = predict(tree.pruned.data, newdata=data, type="prob")
head(prob.tree.data, 5)
cutoff = 0.5 #cutoff
yhat.tree.data = ifelse(prob.tree.data[,2] > cutoff, 1, 0)

# Evaluation
tab = table(data$Y, yhat.tree.data, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
1-sum(diag(tab))/sum(tab) # 오분류율
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity
