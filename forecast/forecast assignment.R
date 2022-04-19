library(forecast)
library(tseries)
library(seasonal)
library(fGarch)

# 시계열 읽기
data <- read.csv("c:/data/forecast/data/unemployment.csv", header = TRUE)
head(data)
ur_o <- ts(data[,2], start=2000, frequency=12)
ur_sa <-ts(data[,3], start=2000, frequency=12)
log_ur_sa = log(ur_sa)
diff_log_ur_sa = diff(log(ur_sa))

# 2. 실업률의 원계열과 계절조정계열의 시계열도표 같이 그리기
par(mfrow=c(1,1))
plot(ur_o, ylab="실업률(%)", xlab="연도", col=1, main="월별 실업률 시계열도표(2000년 1월~2021년 12월)")
lines(ur_sa, col=2)
legend(2015, 5.7,legend=c("원계열","계절조정계열"), lty=c(1,1), col=c(1,2))
help("plot")

# 3. 실업률의 원계열과 계절조정계열에 대한 스펙트럼을 같이 그래프로 표현
par(mfrow=c(1,1))
spectrum(cbind(ur_o, ur_sa), spans=c(3,3), main="실업률 원계열과 계절조정계열의 스펙트럼", col=1:2)
legend(4.5, 0.3,legend=c("원계열","계절조정계열"), lty=c(1,2), col=c(1,2))
spectrum(na.omit(cbind(log(ur_sa), diff(log(ur_sa)))), spans=c(3,3), main="실업률 계절조정계열과 로그차분계열의 스펙트럼", col=1:2)
legend(4.5, 0.01,legend=c("log(ur_sa)","diff(log(ur_sa))"), lty=c(1,2), col=c(1,2))


# 4. 실업률의 계절조정계열의 로그변환계열과 로그차분계열
par(mfrow=c(2,1))
plot(log(ur_sa), main="실업률 계절조정계열의 로그변환계열의 시계열도표", xlab="", ylab="", col=3)
plot(diff(log(ur_sa)), main="실업률 계절조정계열의 로그차분계열 시계열도표", xlab="", ylab="", col=4)
plot(diff(log(ur_sa),2), main="diff(log(ur_sa),2) 시계열도표", xlab="", ylab="", col=5)
# 실업률을 로그변환 후 1차 차분. diff는 차분 함수로, 별다른 지정이 없으면 1차 차분을 의미.
# 륭-박스 검정
Box.test(log(ur_sa),lag=12, type="Ljung") # p-value가 0.9239로 0.05보다 크므로 백색작음계열임.
Box.test(diff(log(ur_sa)),lag=12, type="Ljung")
# p-value가 0.05보다 매우 작은 값이므로 랜던하다고 보기 어려움. 백색작음 계열이라고 할 수 없음.
help("spectrum")

# 4-1. ADF 검정
adf.test(log(ur_sa))
adf.test(diff(log(ur_sa)))

# 4-2. 상관도표와 부분상관도표
# 실업률 계절조정계열의 로그변환계열
par(mfrow=c(2,1))
plot(log(ur_sa), main="", xlab="", ylab="", col=3)
acf(log(ur_sa), main="실업률 계절조정계열의 로그변환계열의 상관도표", col=3)
pacf(log(ur_sa), main="실업률 계절조정계열의 로그변환계열의 부분상관도표", col=3)

# 실업률 계절조정계열의 로그차분계열
plot(diff(log(ur_sa)), main="", xlab="", ylab="", col=4)
acf(diff(log(ur_sa)), main="실업률 계절조정계열의 로그차분계열의 상관도표", col=4)
pacf(diff(log(ur_sa)), main="실업률 계절조정계열의 로그차분계열의 부분상관도표", col=4)

# 실업률 계절조정계열의 로그 2차 차분계열
plot(diff(log(ur_sa),2), main="", xlab="", ylab="", col=5)
acf(diff(log(ur_sa),2), main="실업률 계절조정계열의 로그차분계열의 상관도표", col=5)
pacf(diff(log(ur_sa),2), main="실업률 계절조정계열의 로그차분계열의 부분상관도표", col=5)

# 실업률 계절조정계열의 로그 2차 차분계열
plot(diff(log(ur_sa),12), main="", xlab="", ylab="", col=6)
acf(diff(log(ur_sa),12), main="실업률 계절조정계열의 로그차분계열의 상관도표", col=6)
pacf(diff(log(ur_sa),12), main="실업률 계절조정계열의 로그차분계열의 부분상관도표", col=6)

par(mfrow=c(2,2)) # 그래프를 한 화면에 2×2로 출력
plot(ur, main="UR", ylab=" ", xlab="YEAR")
plot(ur_sa, main="URSA", ylab=" ", xlab="YEAR")
plot(log_ur, main="log(URSA)", ylab=" ", xlab="YEAR")
plot(ur12, main="diff(log(URSA), 12)", ylab=" ", xlab="YEAR")
par(mfrow=c(1,1))


