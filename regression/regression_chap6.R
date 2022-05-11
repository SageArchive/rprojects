# 6장. 모형의 진단

# 오차의 등분산성 검정
# 1) 잔차 산점도 이용
goose = read.table("c:/data/reg/goose.txt", header=T)
head(goose,3)
goose.lm = lm(photo ~ obsA, data=goose)
plot(goose.lm$fitted, goose.lm$resid, pch=19)
# X가 증가함에 따라 잔차의 흩어짐이 커져 이분산성이 의심됨
# 2) 스코어 검정
library(car)
ncvTest(goose.lm)
# Chisquare = 81.41이고, 유의확률 p-값이 매우 작으므로 등분산 가정 기각

# 오차의 선형성 검정
tree = read.table("c:/data/reg/tree.txt", header=T)
head(tree, 3) # D는 나무의 지름, H는 높이, V는 부피
tree.lm = lm(V ~ D+H, data=tree) # v: 반응변수 D, H: 설명변수
# 잔차 산점도 이용
plot(tree$D, tree.lm$resid, pch=19) # 2차 함수 형태의 비선형형이 나타남
plot(tree$H, tree.lm$resid, pch=19)

# 오차의 정규성 검정
# 1) 정규확률그림(normal probability plot)
goose.lm = lm(photo ~ obsA, data=goose)
qqPlot(goose.lm)
# 잔차가 곡선의 형태로 직선에서 벗어나고 있으므로 정규성 가정에 위배된다고 판단
# 2) Shapiro-Wilk의 W통계량
install.packages("mvnormtest")
library(mvnormtest)
goose.rstudent = rstudent(goose.lm) # 스튜던트화 잔차 구하기
shapiro.test(goose.rstudent)
# W 통계량 값이 0.7192이고, 유의확률 p-값이 매우 작으므로 정규성 가정을 기각

# Box-Cox 변환
# 53명의 주거지역 고객에 대한 수요(Y)와 에너지 사용량(X)
energy = read.table("c:/data/reg/energy.txt", header=T)
head(energy,3)
energy.lm = lm(Y ~ X, data=energy)
plot(energy.lm$fitted, energy.lm$resid, pch=19)
# X가 증가함에 따라 잔차의 흩어짐이 커져 이분산성이 의심됨
library(MASS)
boxcox(Y~X, data=energy, lambda=seq(-2,2, 1/2), plotit=TRUE) 
# lamda -2에서 2 사이에 1/2 간격으로 그리면 log-likelihood 값이  최대가  되는 lamda=0.5이므로 Y대신 sqrt(Y)를 반응변수로 하여 회귀모형 적합
