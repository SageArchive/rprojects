#3강.단순회귀모형(2)

#산점도 scatter plot
market = read.table("c:/data/reg/market1.txt", header=T)
head(market)
plot(market$X, market$Y, xlab = "광고료", ylab = "총판매액", pch=19)
title("광고료와 판매액의 산점도")

#회귀선 추정
market.lm = lm(Y ~ X, data=market) #lm: linear model
market.lm

summary(market.lm)
market.lm.summary <- summary(market.lm) #Residuals, Coefficients 변수 호출 가능
market.lm.summary
market.lm.summary$coefficients

# 추정된 회귀식 Ŷ = 0.3282 + 2.1497X
β0 <- market.lm.summary$coefficients[1,1]
β1 <- market.lm.summary$coefficients[1,2]
cat('β0 :', β0, '\n')
cat('β1 :', β1)

#산점도 위에 회귀직선 그리기
plot(market$X, market$Y, xlab = "광고료", ylab = "총판매액", pch=19)
title("광고료와 판매액의 산점도")
abline(market.lm)
identify(market$X, market$Y)

#잔차 residual
plot(market$X, market$Y, xlab = "광고료", ylab = "총판매액", pch=19)
title("광고료와 판매액의 산점도")
abline(market.lm)
xbar = mean(market$X)
ybar = mean(market$Y)
xbar
ybar
points(xbar, ybar, pch=17, cex=2.0, col="RED")
text(xbar, ybar, "(8.85, 19.36)")
fx <- "Y-hat = 0.328+2.14*X"
text(locator(1), fx)

#분산분석표
market.lm = lm(Y ~ X, data=market)
anova(market.lm)
market.lm.anova <- anova(market.lm)
market.lm.anova
#p-value가 매우 작은 값이므로 귀무가설 기각
#유의수준 0.05에서 F-기각역
qf(0.95, 1, 13) #F-value=192.9 > F(1,13,0.05)이므로 귀무가설 기각
#p-value 직접 구하기
1-pf(192.9, 1, 13)

#결정계수, 추정값 표준오차
market.lm = lm(Y ~ X, data=market)
summary(market.lm) #결정계수는 Multiple R-squarred 값으로 0.9369
anova(market.lm) #결정계수는 SSR/(SSR+SSE)
485.57/(485.57+32.72) #위의 0.9369 값과 동일

#단순회귀의 추정과 검정
#beta1와 beta0의 신뢰구간 구하기
market.lm = lm(Y ~ X, data=market)
summary(market.lm)
#beta1의 95% 신뢰구간
q.val = qt(0.975, 13)
2.1497 - q.val*0.1548
2.1497 + q.val*0.1548
#beta0의 95% 신뢰구간
0.3282 - q.val*1.4302
0.3282 + q.val*1.4302

#추정값의 신뢰구간
#X의 주어진 값에서 신뢰대 그리기
pred.frame = data.frame(X=seq(3.5, 14.5, 0.2))
pc = predict(market.lm, int="c", newdata=pred.frame) #기댓값 신뢰구간간
pp = predict(market.lm, int="p", newdata=pred.frame) #새로운 값 신뢰구간
head(pc, 3)
head(pp, 3)
t(pc)
t(pp)
pred.X = pred.frame$X
pred.X
plot(market$X, market$Y, ylim=range(market$Y, pp))
matlines(pred.X, pc, lty=c(1,2,2), col="BLUE")
matlines(pred.X, pp, lty=c(1,3,3), col="RED")

#beta1의 검정
market.lm = lm(Y ~ X, data=market)
summary(market.lm)
#이 결과에서 기울기 beta1의 추정값 b1=2.1497
#t-value는 t0=2.1497/0.1545=13.889

#기각역 및 p-값 구하기
#유의수준 0.05 기각역
qt(0.975, 13)
#유의확률 p-value
2*(1-pt(13.889, 13))

#가중회귀: 오차항마다 분산이 다른 경우
x = c(1,2,3,4,5)
y = c(2,3,5,8,7)
w = 1/x
w.lm = lm(y ~ x, weight=w)
summary(w.lm) #가중회귀직선 y-hat=0.3784+1.5405*x

#분석사례
#자료를 읽어 산점도 그리기
super = read.table("c:/data/reg/supermarket.txt", header=T)
head(super, 3)
attach(super)
plot(price, time, pch=19)
#회귀모형 적합하기
super.lm = lm(time ~ price, data=super)
summary(super.lm)
#단순회귀방정식: time-hat=0.396+0.116*price
#기울기 검정: t-value=12.92, p-value=1.22e-06로 매우 작으므로 귀무가설 기각
#결정계수 R^2=0.9542로, 총 변동 중에서 95.42%가 회귀방정식으로 설명됨
#F-statistic=166.9이고, 이에 대한 p-value=1.22e-06으로 적합된 회귀직선이 유의함

#분산분석표 구하기
anova(super.lm)
#검정통계량 F0=166.85이고, 이에 대한 유의확률 p-value=1.221e-06이 매우 작으므로 적합된 회귀직선이 유의함

#잔차 및 추정값 보기
names(super.lm)
cbind(super, super.lm$resid, super.lm$fitted)
#잔차 그림 그리기
plot(super$price, super.lm$resid, pch=19)
abline(h=0, lty=2) #잔차가 0을 중심으로 일정한 범위내에 있으므로 회귀에 대한 기본 가정 만족

#추정값의 신뢰대 그리기
p.x = data.frame(price=c(1,45))
pc = predict(super.lm, int="c", newdata=p.x)
pred.x=p.x$price
plot(super$price, super$time, ylim=range(super$time, pc), pch=19)
matlines(pred.x, pc, lty=c(1,2,2), col="BLUE")