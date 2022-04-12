# 제3장 예측데이터의 기초분석

# 경제성장률 요약
gdp <- read.csv("c:/data/forecast/data/gdpq.csv", header = TRUE)
gdp_o  <- ts(gdp[,1]/1000, start=1970, frequency=4) # GDP 원계열
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4) # GDP 계절조정계열
gdp_gr <- ts((gdp_sa-lag(gdp_sa,-1))/lag(gdp_sa,-1)*100,  start=c(1970,2), frequency=4) # 전기대비 GDP 증감률
plot(gdp_gr, ylab="경제성장률(전기대비)", xlab="연도", col="steelblue", main="")
abline(h=0, lty=2, col="gray") # GCP가 마이너스 성장한 시점 확인 가능
summary(gdp_gr)
mean(gdp_gr);var(gdp_gr);sd(gdp_gr)
 
# 경제성장률 분포 작성
hist(gdp_gr, breaks = 12, col = "lightblue", border = "black", freq=FALSE, main="", xlab="", xlim=c(-10, 10))
lines(density(gdp_gr)) # density 함수를 이용해 히스토그램을 평활화하여 확률밀도함수 추정하고 그 결과를 lines 함수를 통해 그래프로 표현
shapiro.test(gdp_gr) #샤피로-윌크 검정: p-value가 매우 작아 5% 유의 수준에서 경제성장률은 정규분포를 따른다는 귀무가설을 기각

# Figure 3-3
library(quantmod)
# Yahoo! Finance로터 종합주가지수 종가 데이터 가져오기 
kospi <- getSymbols("^KS11", auto.assign = FALSE)[4]
kospi_r <- dailyReturn(kospi)
# 그래프
hist(kospi_r, breaks = 30, col = "lightblue", border = "black", freq=FALSE, main="", xlab="", xlim=c(-0.12, 0.12), ylim=c(0,50))
lines(density(kospi_r))
shapiro.test(kospi_r)

# 백색작음계열 상관도표
set.seed(1) # 난수 생성
nn=length(gdp_o)
wn=ts(rnorm(nn), start=1970, frequency=4)
par(mfrow=c(3,1))
plot(wn, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
acf(wn, main="", col="steelblue")
pacf(wn, main="", col="steelblue")

# GDP 계정조정계열과 상관도표
nn=length(gdp_sa)
sin = ts(sin(1:nn/nn*12*pi), start=1970, frequency=4)
par(mfrow=c(2,1))
plot(sin, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
acf(sin, main="", col="steelblue")

# GDP 원계열: 상관도표, 부분상관도표, 스펙트럼
plot(gdp_o, main="", xlab="", ylab="", col="steelblue")
acf(gdp_o, main=" ", col="steelblue") # 서서히 움직임
pacf(gdp_o, main=" ", col="steelblue")

# GDP 원계열의 로그변환 및 상관도표
plot(diff(log(gdp_o)), main="", xlab="", ylab="", col="steelblue")
acf(diff(log(gdp_o)), main=" ", col="steelblue")

# 로그변환한 GDP 원계열의 4차 차분 및 상관도표
plot(diff(log(gdp_o),4), main="", xlab="", ylab="", col="steelblue")
acf(diff(log(gdp_o),4), main=" ", col="steelblue")

# 륭-박스 검정
Box.test(wn,lag=8, type="Ljung") # p-value가 0.9239로 0.05보다 크므로 백색작음계열임.
Box.test(sin,lag=8, type="Ljung")
Box.test(gdp_o,lag=8, type="Ljung")
Box.test(diff(log(gdp_o)),lag=8, type="Ljung")
Box.test(diff(log(gdp_o),4),lag=8, type="Ljung")
# p-value가 0.05보다 매우 작은 값이므로 랜던하다고 보기 어려움. 백색작음 계열이라고 할 수 없음.

par(mfrow=c(2,1))
plot(wn, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
pacf(wn, main="", col="steelblue")

plot(sin, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
pacf(sin, main="", col="steelblue")

plot(gdp_o, main="", xlab="", ylab="", col="steelblue")
pacf(gdp_o, main=" ", col="steelblue")

plot(diff(log(gdp_o)), main="", xlab="", ylab="", col="steelblue")
pacf(diff(log(gdp_o)), main=" ", col="steelblue")

plot(diff(log(gdp_o),4), main="", xlab="", ylab="", col="steelblue")
pacf(diff(log(gdp_o),4), main=" ", col="steelblue")

par(mfrow=c(2,1))
plot(wn, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
aa=spectrum(wn, spans=c(3,3), main="", col="steelblue")
plot(1:80/40, aa$spec, type="l", ylim=c(0,10))

plot(sin, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
pacf(sin, main="", col="steelblue")

plot(gdp_o, main="", xlab="", ylab="", col="steelblue")
pacf(gdp_o, main=" ", col="steelblue")

plot(diff(log(gdp_o)), main="", xlab="", ylab="", col="steelblue")
pacf(diff(log(gdp_o)), main=" ", col="steelblue")

plot(diff(log(gdp_o),4), main="", xlab="", ylab="", col="steelblue")
pacf(diff(log(gdp_o),4), main=" ", col="steelblue")

# 스펙트럼
par(mfrow=c(2,1))
sin1 = ts(sin(1:nn/nn*12*pi), start=1970, frequency=4)
sin2 = ts(sin(1:nn/nn*36*pi), start=1970, frequency=4)
plot(cbind(sin1, sin2), main="", xlab="", ylab="", col="steelblue")
spectrum(cbind(sin1, sin2), spans=c(3,3), main="", col="steelblue")
spectrum(gdp_o, spans=c(3,3), main="", col="steelblue")
plot(sin1+sin2, main="", xlab="", ylab="", col="steelblue")
spectrum(sin1+sin2, spans=c(3,3), main="", col="steelblue")

plot(wn, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
spectrum(wn, spans=c(3,3), main="", col=1:2)

# GDP 원계열과 스펙트럼
plot(gdp_o, main="", xlab="", ylab="")
lines(gdp_sa, col=2)
spectrum(cbind(gdp_o, gdp_sa), spans=c(3,3), main="", col=1:2) # GDP 원계열과 계절조정계열의 스펙트럼

dlgdp1 = diff(log(gdp_o))
dlgdp2 = diff(log(gdp_o),4)
dlgdp  = cbind(dlgdp1,dlgdp2)
plot(dlgdp1, main="", xlab="", ylab="", col="steelblue")
lines(dlgdp2, col=2)
spectrum( na.omit(cbind(dlgdp1,dlgdp2)), spans=c(3,3), main="", col=c("steelblue", "red"))


