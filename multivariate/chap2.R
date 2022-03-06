# chap2. Principle Component Analysis

# R 2.2
install.packages("HSAUR2")
library(HSAUR2)
data(heptathlon)
head(heptathlon)

summary(heptathlon)
write.csv(heptathlon, file="c:/data/mva/heptathlon.csv")

# R 2.3
heptathlon$hurdles = max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m = max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m = max(heptathlon$run800m) - heptathlon$run800m
head(heptathlon)
     
# R 2.4
hep_data = heptathlon[, -8]
# Principal component analysis using principal ( using eigen)
hep_pca = princomp(hep_data, cor=T, scores=T)
names(hep_pca)
hep_pca
     
# Principal component analysis using prcomp (using SVD)
hep_pca2 =  prcomp(hep_data, scale=TRUE)
     
# R 2.5
summary(hep_pca)
# summary(hep_pca2)
eig_val = hep_pca$sdev^2
eig_val
     
# R 2.6
screeplot(hep_pca, type="lines", pch=19, main="Scree plot")
hep_var = hep_pca$sdev^2
hep_var
hep_var_ratio = hep_var/sum(hep_var)
round(hep_var_ratio, 3)

# screeplot(hep_pca2, type="lines", pch=19, main="Scree plot")
plot(cumsum(hep_var_ratio), type='b', pch=19, xlab='Component', ylab='Cumulative Proportion')
title('Variance Explained')

# PCA coefficient
hep_pca$loadings[, c(1:2)]
# hep_pca2$rotation[, c(1:2)]

# R 2.7
hep_pca$scores[, c(1:2)]
biplot(hep_pca, cex=0.7, col=c("Red", "Blue"))
title("Biplot")
     
# hep_pca2$x[c(1:5),c(1:2)]
# biplot(hep_pca2)
     
# R 2.8
beer = read.csv("c:/data/mva/beer.csv")
head(beer)
summary(beer)
     
# R 2.9
round(cor(beer), 2)
plot(beer, pch=19)
     
# R 2.10
beer_pca = princomp(beer, cor=F, scores=T)
beer_pca
     
# R 2.11
summary(beer_pca)
     
# R 2.12
screeplot(beer_pca, type="lines", pch=19)
beer_pca$loadings[, c(1:3)]