# 과제1
# 자료파일 읽기 및 변수 변환
forbes = read.table("c:/data/reg/forbes.txt", header = T)
forbes$Lpress = 100*log10(forbes$press)
head(forbes)

# 산점도 그리기
attach(forbes)
plot(temp, Lpress, pch=19)
title("temp(온도)와 Lpress(대기압력)의 산점도")

# 회귀모형 적합하기
forbes.lm = lm(Lpress ~ temp, data = forbes)
summary(forbes.lm)

# 분산분석표 구하기
anova(forbes.lm)

# 잔차 및 추정값 보기
names(forbes.lm)
cbind(forbes, forbes.lm$resid, forbes.lm$fitted)

# 잔차그림 그리기
plot(temp, forbes.lm$resid)
abline(h=0, lty=2)
identify(temp, forbes.lm$resid)

# 추정값의 신뢰대 그리기
p.x=data.frame(temp=c(190:215))
pc=predict(forbes.lm, int="c", newdata=p.x)
pred.x=p.x$temp
plot(temp, Lpress, ylim=range(Lpress, pc))
matlines(pred.x, pc, lty=c(1, 2, 2), col="BLUE")

# 과제2
# 자료파일 읽기
health = read.csv("c:/data/reg/p162.csv", header=T)
head(health)

# 기술통계량 미 상관계수 보기
summary(health[,-1])
cor(health[,-1])

# 회귀모형 적합하기
health.lm=lm(Y ~ X1+X2+X3+X4, data=health)
summary(health.lm)

names(health.lm)
yhat = health.lm$fitted.values
yhat

# 추가 변수 그림
library(car)
avPlots(health.lm)

# 분산분석표
anova(health.lm)
# SSR: 회귀 제곱합
89117+4680+3165+22399
# SST: 회귀 제곱합+잔차 제곱합
89117+4680+3165+22399+20551
# MSR: SSR/k
119361/4
# MSE: 822
# F-값: MSR/MSE
29840.25/822
# 유의성 검정 F-value = 36.3 > F(4,25,0.05) 이므로 귀무가설을 기각함.
# p-값 구하기
1-pf(36.3, 4,25)


# 잔차산점도
par(mfrow=c(2,2))
plot(health$X1, health.lm$resid)
abline(h=0, lty=2)
identify(health$X1, health.lm$resid)
plot(health$X2, health.lm$resid)
abline(h=0, lty=2)
identify(health$X2, health.lm$resid)
plot(health$X3, health.lm$resid)
abline(h=0, lty=2)
identify(health$X3, health.lm$resid)
plot(health$X4, health.lm$resid)
abline(h=0, lty=2)
identify(health$X4, health.lm$resid)

#추정값과 잔차 산점도
par(mfrow=c(1,1))
plot(health.lm$fitted, health.lm$resid)
abline(h=0, lty=2)
identify(health.lm$fitted, health.lm$resid)

# 결정계수: 반응변수 Y와 추정값 yhat의 상관계수의 제곱
cor(health$Y, y2hat)
cor(health$Y, y2hat)^2
cor(health$Y, y3hat)
cor(health$Y, y3hat)^2

# 표준화된 회귀모형
install.packages("lm.beta")
library(lm.beta)
h2.beta=lm.beta(h2.lm)
print(h2.beta)
summary(h2.beta)
anova(h2.beta)
names(h2.beta)
h3.beta=lm.beta(h3.lm)
print(h3.beta)
summary(h3.beta)
anova(h3.beta)

# 잔차 산점도: (독립변수, 잔차)
par(mfrow=c(2,2), pty="s")
plot(health$X1, h3.beta$residuals, pch=19)
abline(h=0, lty=2)
plot(health$X2, h3.beta$residuals, pch=19)
abline(h=0, lty=2)
plot(health$X3, h3.beta$residuals, pch=19)
abline(h=0, lty=2)
plot(health$X4, h3.beta$residuals, pch=19)
abline(h=0, lty=2)

# 잔차 산점도: (독립변수, 잔차)
par(mfrow=c(1,1))
plot(h3.beta$fitted.values, h3.beta$residuals, pch=19)
abline(h=0, lty=2)
identify(h3.beta$fitted.values, h3.beta$residuals)
