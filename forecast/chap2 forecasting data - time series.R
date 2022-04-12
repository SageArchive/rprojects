# 제2장 예측데이터-시계열
install.packages("tseries")
install.packages("forecast")
install.packages("mfilter")
install.packages("seasonal")
install.packages("var")
install.packages("fGarch")

# 시계열 읽기
gdp <- read.csv("c:/data/forecast/data/gdpq.csv", header = TRUE)
gdp
gdp_data <-ts(gdp, start=1970, frequency=4)
gdp_data
# gdp <- ts(read.csv("c:/data/forecast/data/gdpq.csv", header = TRUE), start=1970, frequency=4) 한번에 읽기 가능

# 시계열도표 작성
plot(gdp_data[,1]/1000, ylab="GDP(조원)", xlab="연도", col="steelblue")
lines(gdp_data[,2]/1000, col="red")

# 시계열의 차분과 이동평균
ww=c(.5,1,1,1,.5); gdpm5=filter(gdp_data, sides=2, ww/sum(ww))
# 실질 GDP를 5분기 중심화 이동평균하여 gdpm5로 지정. 여기서 ww는 가중치, filter는 이동평균 함수, sides=2는 중심화 이동평균을 의미, ww/sum(ww)는 가중치. 문장 후 ';'는 두 문장을 동시에 입력할 경우 이용.
dlgdp1 = diff(log(gdp_data))
# GDP를 로그변환 후 1차 차분하여 dlgdp1으로 지정, diff는 차분 함수로, 별다른 지정이 없으면 1차 차분을 의미.
dlgdp4 = diff(log(gdp_data), 4)
# GDP를 로그변환 후 4차 차분하여 dlgdp4로 지정, diff 문의 옵션으로 4를 지정하여 4차 차분 실시.
par(mfrow=c(2,2)) # 그래프를 한 화면에 2×2로 출력
plot(gdp_data, main="GDP", ylab=" ", xlab="YEAR")
plot(gdpm5, main="GDPM5", ylab=" ", xlab="YEAR")
plot(dlgdp1, main="diff(log(GDP))", ylab=" ", xlab="YEAR")
plot(dlgdp4, main="diff(log(GDP), 4)", ylab=" ", xlab="YEAR")
par(mfrow=c(1,1))


library(forecast)
gdp <- read.csv("c:/data/forecast/data/gdpq.csv", header = TRUE)
gdp_o <- ts(gdp[,1]/1000, start=1970, frequency=4)
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa-lag(gdp_sa,-1))/lag(gdp_sa,-1)*100,  start=c(1970,2), frequency=4)

# Fit model to first few years of AirPassengers data
gdp.model <- Arima(window(gdp_o,end=2008+11/12),order=c(2,1,3),
                   seasonal=list(order=c(0,1,1),period=4),lambda=0)
plot(forecast(gdp.model,h=20))
lines(gdp_o)

# Apply fitted model to later data
gdp.model2 <- Arima(window(gdp_o,start=2009),model=gdp.model)

# Forecast accuracy measures on the log scale.
# in-sample one-step forecasts.
accuracy(gdp.model)
# out-of-sample one-step forecasts.
accuracy(gdp.model2)
# out-of-sample multi-step forecasts
accuracy(forecast(gdp.model,h=20,lambda=NULL), window(gdp_o,start=2009))