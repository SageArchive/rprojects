#chap2.단순회귀모형(2)

#산점도 scatter plot
market = read.table("c:/data/reg/market1.txt", header=T)
head(market)
plot(market$X, market$Y, xlab = "광고료", ylab = "총판매액", pch=19)
title("광고료와 판매액의 산점도")

#회귀선 추정
market.lm = lm(Y ~ X, data=market)
summary(market.lm)

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
