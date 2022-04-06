# 과제 2번
# Importing data
wine = read.csv("c:/data/datamining/winequalityCLASS.csv", header=TRUE)
head(wine)
summary(wine)
wine_data = wine[, c(10:12)]

# Fitting a logistic regression model
fit.all = glm(quality ~ ., family = binomial, data = wine_data)
fit.step = step(fit.all, direction="both") # stepwise vaiable selection
fit.step$anova
summary(fit.step)

# Making predictions
p = predict(fit.step, newdata=wine_data, type="response")
cutoff = 0.5 
yhat = ifelse(p > cutoff, 1, 0)
yhat

# Evaluation
tab = table(wine$quality, yhat, dnn=c("Observed","Predicted"))
print(tab)                # confusion matrix
sum(diag(tab))/sum(tab)   # 예측 정확도(prediction accuracy)
1-sum(diag(tab))/sum(tab) # 오분류율
tab[2,2]/sum(tab[2,])     # 민감도(sensitivity) 
tab[1,1]/sum(tab[1,])     # 특이도specificity)
