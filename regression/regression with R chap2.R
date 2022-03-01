market = read.table("c:/data/reg/market-1.txt", header=T)
head(market)
plot(market$X, market$Y, xlab="광고료", ylab="총판매액", pch=19)
title("광고료와 판매액의 산점도")
# attach(market)
# plot(X, Y, xlab="광고료", ylab="총판매액", pch=19)
market.lm = lm(Y ~ X, data=market)
summary(market.lm)
abline(market.lm)
identify(market$X, market$Y)
names(market.lm) # market.lm 변수 이름 확인
resid = market.lm$residuals
fitted = market.lm$fitted.values
sum(resid)
sum(fitted)
sum(market$Y)
sum(market$X*resid)
sum(fitted*resid)

xbar=mean(market$X)
ybar=mean(market$Y)
xbar
ybar
points(xbar, ybar, pch=17, cex=2.0, col="RED")
text(xbar, ybar, "(8, 18.6")
fx <- "Y-hat = -2.2696+2.61X"
text(locator(1), fx)

sum((market$Y-fitted)*(fitted-mean(market$Y)))
anova(market.lm)
313.043/(313.043+55.357)
sqrt(6.92)
