#chap2.단순회귀모형(1)

#산점도 scatter plot
market = read.table("c:/data/reg/market1.txt", header=T)
head(market)
plot(market$X, market$Y, xlab="광고료", ylab="총판매액", pch=19)
title("광고료와 판매액의 산점도")
# attach(market) 함수를 사용하면 아래와 같이 plot함수에서 market$X, market$Y 대신 X, Y 사용 가능
# plot(X, Y, xlab="광고료", ylab="총판매액", pch=19)

#회귀선의 추정
market.lm = lm(Y ~ X, data=market)
summary(market.lm)
abline(market.lm)
identify(market$X, market$Y)

#잔차 residual
names(market.lm) # market.lm 변수 이름 확인
resid = market.lm$residuals
fitted = market.lm$fitted.values
sum(resid) #매우 작은 수로 0과 같음
sum(fitted)
sum(market$Y)
sum(market$X*resid) #매우 작은 값으로 0과 같음
sum(fitted*resid) #매우 작은 값으로 0과 같음

#점(Xbar, Ybar)는 회귀선 상에 있음
xbar=mean(market$X)
ybar=mean(market$Y)
xbar
ybar
points(xbar, ybar, pch=17, cex=2.0, col="RED")
text(xbar, ybar, "(8.85, 19.36")
fx <- "Y-hat = 0.328+2.14X"
text(locator(1), fx) #클릭한 위치에 텍스트 fx 출력

#분산분석표 analysis of variance table
anova(market.lm) #p값이 매우 작은 값이므로 귀무가설 기각
#결정계수 coefficient of determination
485.57/(485.57+32.72) #총 변동 중 회귀선에 의해 설명되는 부분이 94%라는 의미
#추정값의 표준오차 standard error of estimate=MSE(잔차제곱평균)의 제곱근
sqrt(2.52)