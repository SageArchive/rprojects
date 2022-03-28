# Chap.2 주성분 분석(Principle Component Analysis)

install.packages("HSAUR2")
library(HSAUR2)
data(heptathlon)
# heptathlon: 1988년 서울 올릭픽 육상 여성 7종 경기 결과
# 변수: hurdles(110m 허들), highjump(높이뛰기), shot(포환던지기), run200m(200m 달리기), longjump(멀리뛰기),
# javelin(창던지기), run800m(800m 달리기), score(점수) 등 8개의 변수, 케이스 수: 25개
head(heptathlon)
summary(heptathlon)
write.csv(heptathlon, file="c:/data/mva/heptathlon.csv")

# 자료 변형하기: hurdles, run200m, run800m은 작은 값일수록 좋은 점수이기 때문에 높은 수가 좋은 점수가 되도록 최대값에서 빼줌으로써 자료를 역변환.
heptathlon$hurdles = max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m = max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m = max(heptathlon$run800m) - heptathlon$run800m
head(heptathlon)

# 분석 전 상자그림 확인
library(MVA)
x = heptathlon[, c(1,2)]
bvbox(x, xlab="hurdles", ylab="highjump", pch=19)
identify(x) # 그래프에서 아웃라이어 표시
rownames(x)[c(25)]
     
# 주성분 분석 실행
hep_data = heptathlon[, -8] # 8열의 scores 변수를 데이터에서 제외

# 1) Principal component analysis using principal(using eigen)
hep_pca = princomp(hep_data, cor=T, scores=T) # cor:상관계수행렬, scores: 주성분점수
names(hep_pca)
hep_pca
     
# 주성분 분석 결과
summary(hep_pca) 
eig_val = hep_pca$sdev^2 # 고유값: 각 주성분의 표준편차를 제곱
round(eig_val,3) 
     
# 스크리 그림
screeplot(hep_pca, type="lines", pch=19, main="Scree plot")

# 누적분산 그림
hep_var = hep_pca$sdev^2
hep_var_ratio = hep_var/sum(hep_var)
round(hep_var_ratio, 3)
plot(cumsum(hep_var_ratio), type='b', pch=19, xlab='Component', ylab='Cumulative Proportion')
title('Variance Explained')

# 주성분 계수 PCA coefficient
round(hep_pca$loadings[, c(1:2)],3) 
# Comp.1은 javelin을 제외한 모든 변수의 절대값이 큰 값을 가지므로 전반적인 체력정도를 나타내는 성분, Comp.2는 javelin과 밀접한 관련이 있는 성분

# 주성분 점수 및 행렬도(biplot)
hep_pca$scores[, c(1:2)]
# 행렬도(biplot)는 각 개체의 관찰값은 주성분 점수로, 각 변수와 주성분과의 관계를 나타내는 주성분계수를 동시에 표시
biplot(hep_pca, cex=0.7, col=c("Red", "Blue"))
title("Biplot")

# 2) Principal component analysis using prcomp (using SVD)
hep_pca2 =  prcomp(hep_data, scale=TRUE)
names(hep_pca2)

# 주성분 분석 결과
summary(hep_pca2)
eig_val2 = hep_pca2$sdev^2
round(eig_val2,3)

# 스크리 그림
screeplot(hep_pca2, type="lines", pch=19, main="Scree plot")

# 누적분산 그림
hep_var2 = hep_pca2$sdev^2
hep_var_ratio2 = hep_var2/sum(hep_var2)
round(hep_var_ratio2, 3)
plot(cumsum(hep_var_ratio2), type='b', pch=19, xlab='Component', ylab='Cumulative Proportion')
title('Variance Explained')

# 주성분 계수 PCA coefficient
round(hep_pca2$rotation[, c(1:2)],3)

# 주성분 점수 및 행렬도(biplot)
hep_pca2$x[c(1:5),c(1:2)]
biplot(hep_pca2, cex=0.7, col=c("Red", "Blue"))
title("Biplot")