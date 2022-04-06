# 2장 주성분 분석 4번 p.78
# 자료 읽기 및 요약통계량
crime = read.csv("c:/data/mva/ex2-4.csv")
head(crime)
summary(crime)

# 상관계수행렬 및 산점도행렬 보기
round(cor(crime[,2:5]), 2)
plot(crime[,2:5], pch=19)
par(mfrow=c(1,4))
boxplot(crime$Murder, xlab="Murder")
boxplot(crime$Assault, xlab="Assault")
boxplot(crime$UrbanPop, xlab="UrbanPop")
boxplot(crime$Rape, xlab="Rape")
a <- crime$Rape
which(a > fivenum(a)[4] + 1.5*IQR(a))
crime[2,1]
crime[28,1]

# 이상치 제거

# 주성분분석 실행
rownames(crime) = crime[,1]
crime_data = crime[,-1]
crime_pca = princomp(crime_data, cor = T, scores = T)
names(crime_pca)
crime_pca

# 주성분분석 결과
summary(crime_pca)
eig_val = crime_pca$sdev^2 # 고유값: 각 주성분의 표준편차를 제곱
round(eig_val,3)

# 스크리 그림과 누적분산
par(mfrow=c(1,2))
screeplot(crime_pca, type="lines", pch=19, main="Scree plot")
crime_var = crime_pca$sdev^2
crime_var_ratio = crime_var/sum(crime_var)
round(crime_var_ratio, 3)
plot(cumsum(crime_var_ratio), type='b', pch=19, xlab='Component', ylab='Cumulative Proportion')
title("Varience Explained")

# 주성분 계수
round(crime_pca$loadings[, c(1:2)], 3)
#PC1 = 0.536*Murder + 0.583*Assault + 0.278*UrbanPop + 0.543*Rape 
#PC2 = 0.418*Murder + 0.188*Assault - 0.873*UrbanPop - 0.167*Rape
# 제1주성분은 UrbanPop을 제외한 모든 변수의 절대값이 큰 값을 가지는 것으로 볼 때, 전반적인 범죄와 관련이 있는 성분이라고 할 수 있음.
# 제2주성분은 UrbanPop의 계수가 다른 변수에 비해 절대값이 큰 것으로 볼 때, 도시화 수준과 밀접한 관련이 있는 성분으로 파악할 수 있음.

#주성분 점수 밒 행렬도
par(mfrow=c(1,1))
crime_pca$scores[, c(1:2)]
biplot(crime_pca, cex=0.7, col=c("RED", "BLUE"))
title("Biplot")
