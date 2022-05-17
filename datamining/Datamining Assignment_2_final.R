# 과제 2번
# Importing data
wine = read.csv("c:/data/datamining/winequalityCLASS.csv", header=TRUE)
head(wine)

# 2-(1) two variables
# Fitting a logistic regression model 
fit.two = glm(quality ~ alcohol+sulphates, family = binomial, data = wine)
summary(fit.two)
# Making predictions
p1 = predict(fit.two, newdata=wine, type="response")
cutoff = 0.5
yhat1 = ifelse(p1 > cutoff, 1, 0)
# Evaluation
tab1 = table(wine$quality, yhat1, dnn=c("Observed","Predicted"))
print(tab1)                 # 정오분류표(confusion matrix)
sum(diag(tab1))/sum(tab1)   # 예측 정확도(prediction accuracy)
1-sum(diag(tab1))/sum(tab1) # 오분류율
tab1[2,2]/sum(tab1[2,])     # 민감도(sensitivity) 
tab1[1,1]/sum(tab1[1,])     # 특이도(specificity)

# 2-(2) all variables
# Fitting a logistic regression model
fit.all = glm(quality ~ ., family = binomial, data = wine)
summary(fit.all)
# Making predictions
p2 = predict(fit.all, newdata=wine, type="response")
cutoff = 0.5 
yhat2 = ifelse(p2 > cutoff, 1, 0)
# Evaluation
tab2 = table(wine$quality, yhat2, dnn=c("Observed","Predicted"))
print(tab2)                 # 정오분류표(confusion matrix)
sum(diag(tab2))/sum(tab2)   # 예측 정확도(prediction accuracy)
1-sum(diag(tab2))/sum(tab2) # 오분류율
tab2[2,2]/sum(tab2[2,])     # 민감도(sensitivity) 
tab2[1,1]/sum(tab2[1,])     # 특이도(specificity)

# 2-(3)  stepwise vaiable selection
fit.step = step(fit.all, direction="both")
fit.step$anova
summary(fit.step)
# Making predictions
p3 = predict(fit.step, newdata=wine, type="response")
cutoff = 0.5 
yhat3 = ifelse(p3 > cutoff, 1, 0)
# Evaluation
tab3 = table(wine$quality, yhat3, dnn=c("Observed","Predicted"))
print(tab3)                 # 정오분류표(confusion matrix)
sum(diag(tab3))/sum(tab3)   # 예측 정확도(prediction accuracy)
1-sum(diag(tab3))/sum(tab3) # 오분류율
tab3[2,2]/sum(tab3[2,])     # 민감도(sensitivity) 
tab3[1,1]/sum(tab3[1,])     # 특이도(specificity)