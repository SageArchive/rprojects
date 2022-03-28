# Chap1. 다변량 시각화(Visualization of Multivariate Data)

# 1.1 기술통계량과 분할표
survey = read.csv("c:/data/mva/survey.csv")
head(survey,3)
mean(survey$age)
sd(survey$age)

#nlevels(survry$sex)
survey$sex = factor(survey$sex, levels=c(1:2), labels=c("Male", "Female"))
survey$marriage = factor(survey$marriage, levels=c(1:3), labels=c("Unmarried","Married","Divorced"))
survey$job = factor(survey$job, levels=c(1:8), labels=c('a','b','c','d','e','f','g','other'))
survey$edu = ordered(survey$edu, levels=c(1:5), labels=c('none','elem','med','high','college')) 
summary(survey[,-1])

# 그룹별 기술통계랑 구하기: 나이에 대한 평균 및 표준편차
tapply(survey$age, survey$sex, mean)
with(survey, tapply(age, sex, sd))
with(survey, tapply(age, marriage, mean))
with(survey, tapply(age, marriage, sd))

sex_ma = list(survey$sex, survey$marriage)
table(sex_ma)
with(survey, tapply(age, sex_ma, mean))
with(survey, tapply(age, sex_ma, sd))

# 빈도표 및 분할표(성별, 교육)
table(survey$sex)
table(survey$edu)
table(survey$sex, survey$edu)
sex_edu = table(survey$sex, survey$edu)
summary(sex_edu)

# 1.2 단변량 그래프
survey = read.csv("c:/data/mva/survey.csv")
survey
#막대그림 및 원그림
edu_tb = table(survey$edu)
edu_tb
rownames(edu_tb) = c("무학","초졸", "중졸","고졸","대졸")
edu_tb
barplot(edu_tb)
dev.new()
pie(edu_tb, main="교육정도 원그림")
dev.off()

# 겹친 막대그림
sex_edu = list(survey$sex, survey$edu)
sex_edu_tb = table(sex_edu)
sex_edu_tb
rownames(sex_edu_tb) = c("Male", "Female")
colnames(sex_edu_tb) = c("none","elem", "med","high","college")
sex_edu_tb
barplot(sex_edu_tb, legend.text=rownames(sex_edu_tb), col=c(2,4))
title("Stacked Barplot")

# 한 화면에 여러 개의 그림 그리기: par문
par(mfrow=c(1,2)) # c(2,2)의 경우 화면을 4개로 분할
pie(sex_edu_tb[1,])
title("Education of Male")
pie(sex_edu_tb[2,])
title("Education of Female")

# 히스토그램
hist(survey$salary)
#줄기-잎 그림
stem(survey$salary)
stem(survey$salary, scale=2)

# 상자그림 box plot
boxplot(salary ~ sex, data=survey)
title("Boxplot of Salary")

# 1.3 이변량 그래프
# plot using lines
plot(co2)
lines(smooth(co2),col="BLUE")
# plot of mathematical functions
x <- seq(0, 20, 0.1) 
y <- exp(-x/10)*cos(2*x) 
plot(x,y,type="l") 

# Bivariate boxplot
install.packages("HSAUR2")
library(HSAUR2)
install.packages("MVA")
library(MVA)
data(USairpollution)
# SO2: SO2 content of air in micrograms per cubic metre.
# temp: average annual temperature in Fahrenheit.
# manu: number of manufacturing enterprises employing 20 or more workers.
# popul: population size (1970 census); in thousands.
# wind: average annual wind speed in miles per hour.
# precip: average annual precipitation in inches.
# predays: average number of days with precipitation per year.
head(USairpollution, 3)
x = USairpollution[, c(3,4)]
bvbox(x, xlab="manu", ylab="popul", pch=19)
title("Bivariate Boxplot")
identify(x) # 그래프에서 아웃라이어 표시
rownames(x)[c(7,9,14,30)] # 아웃라이어 도시 확인

# Bubble plot
plot(wind~temp, data=USairpollution, pch=9)
# symbols(USairpollution$temp, USairpollution$wind, USairpollution$circle=SO2, inches=0.5, add=T) 대신
with(USairpollution, symbols(temp, wind, circle=SO2, inches=0.5, add=T))
title("Bubble plot")

# 1.4 다차원 그래프
# 산점도 행렬
social = read.table("c:/data/mva/social.txt", header=T)
head(social, 3)
pairs(social)
round(cor(social, use="complete.obs"), 3)

# 별그림(star plot)
social2 = social[, -1]
year = social[,1]
rownames(social2) = year
stars(social2)

# 얼굴그림(faces plot)
install.packages("aplpack")
library(aplpack) 
# faces(social2, face.type=0, na.rm=TRUE)
par(mfrow=c(1,3))
faces(social2, face.type=0)
faces(social2, face.type=1)
# face.type=0은 선만 그리기, 1은 선 색칠하기