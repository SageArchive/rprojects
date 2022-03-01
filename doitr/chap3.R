#chapter3 변수, 함수, 패키지

#변수 만들기
a <- 1
a

b <- 2
b

c <- 3
c

d <- 3.5
d

a+b

a+b+c

4/b

5*b

#여러 값으로 구성된 변수 만들기
var1 <- c(1, 2, 5, 7, 8)
var1

var2 <- c(1:5)
var2

var3<- seq(1, 5)
var3

var4 <- seq(1, 10, by=2)
var4

var5 <- seq(1, 10,by=3)
var5

var1
var1+2

var1+var2

#문자로 된 변수 만들기
str1 <- "a"
str1

str2 <- "text"
str2

str3 <- "Hello World!"
str3

str4 <- c("a", "b", "c")
str4

str5 <- c("Hello!", "World", "is", "good!")
str5

str1+2 #문자로 된 변수는 연산 불가능

#숫자를 다루는 함수 이용하기
x <- c(1, 2, 3)
x
mean(x) #평균을 구하는 함수
max(x) #최댓값
min(x) #최솟값

#문자를 다루는 함수 이용하기
str5
paste(str5, collapse = ",") #쉼표를 구분자로 str5의 단어를 하나로 합치기
paste(str5, collapse = " ") #단어를 한 칸씩 띄우고 합치기
# collapse처럼 함수의 옵션을 설정하는 명령어를 '파라미터(parameter)' 또는 '매개변수'라고 함

#함수의 결과물로 새 변수 만들기
x_mean <- mean(x)
x_mean

str5_paste <- paste(str5, collapse = " ")
str5_paste

#패키지 설치하기
install.packages("ggplot2")
#패키지 로드하기
library(ggplot2)

x <- c("a", "a", "b", "c")
x
#빈도 막대 그래프 출력
qplot(x)

#ggplot2의 mpg 데이터로 그래프 그리기
qplot(data = mpg, x = hwy) #data에 mpg, x축에 hwy 변수 지정해 그래프 생성
qplot(data = mpg, x = cty)
qplot(data = mpg, x = drv, y = hwy)
qplot(data = mpg, x = drv, y = hwy, geom = "line") #선 그래프 형태
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot") #상자 그림 형태
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv) #drv별 색 표현

#함수의 기능이 궁금할 때 Help 함수 사용
?qplot

#혼자서 해보기
#Q1 시험 점수 변수 만들고 출력하기
test_score <- c(80, 60, 70, 50, 90)
test_score
#Q2 전체 평균 구하기
mean(test_score)
#전체 평균 변수 만들고 출력하기
total_avg <- mean(test_score)
total_avg