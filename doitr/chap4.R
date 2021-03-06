#chapter4 데이터 프레임

# 04-2 Data Frame
# 1.creating variables 
english <- c(90, 80, 60, 70)
english

math <- c(50, 60, 100, 20)
math

# 2.creating data frames
df_midterm <- data.frame(english, math)
df_midterm

# 3.adding class data to the df_midterm data frame
class <- c(1, 1, 2, 2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

# 4.analysis
mean(df_midterm$english) # mean of english score in df_midterm
mean(df_midterm$math) # mean of math score in df_midterm

# exercise
# Q1
fruits <- c("apple", "strawberry", "watermelon")
price <- c(1800, 1500, 3000)
volume <- c(24, 38, 13)
sales <- data.frame(fruits, price, volume)
sales
# Q2
mean(sales$price)
mean(sales$volume)

# 04-3 importing external data
install.packages("readxl")
library(readxl)
df_exam <-read_excel("C:/data/doitr/Data/excel_exam.xlsx")
df_exam
#english와 science 점수 평균 구하기
mean(df_exam$english)
mean(df_exam$science)

df_exam_novar <- read_excel("C:/data/doitr/Data/excel_exam_novar.xlsx") #엑셀 파일 첫 번째 행을 변수명으로 인식
df_exam_novar

df_exam_novar <- read_excel("C:/data/doitr/Data/excel_exam_novar.xlsx", col_names = F) #첫 번째 행을 변수명이 아닌 데이터로 인식, 변수명은 '숫자'로 자동 지정
df_exam_novar

#엑셀 파일에 시트가 여러개인 경우
df_exam_sheet <- read_excel("C:/data/doitr/Data/excel_exam_sheet.xlsx", sheet = 3)

#csv 파일 불러오기: 별도의 패키지 설치 불필요
df_csv_exam <- read.csv("C:/data/doitr/Data/csv_exam.csv")
df_csv_exam
#첫 번째 행에 변수명이 없는 경우
df_csv_exam <- read.csv("C:/data/doitr/Data/csv_exam.csv", header = F)

#데이터 프레임을 CSV 파일로 저장하기
#데이터 프레임 만들기
df_midterm <- data.frame(english = c(90, 80, 60, 70), math = c(50, 60, 100, 20), class = c(1, 1, 2, 2))
df_midterm
#CSV 파일로 저장하기
write.csv(df_midterm, file = "df_midterm.csv") #저장한 파일은 프로젝트 폴더에 생성됨

#RDS 파일: R 전용 파일, 읽고 쓰는 속도가 빠르고 용량이 작음
#데이터 프레임을 RDS 파일로 저장하기
saveRDS(df_midterm, file = "df_midterm.rds") #저장한 파일은 프로젝트 폴더에 생성됨
#RDS 파일 불러오기 
rm(df_midterm) #앞에서 만든 데이터프레임 삭제
df_midterm #Error: object 'df_midterm' not found
df_midterm <- readRDS("df_midterm.rds") #RDS 파일을 불러와 df_midterm에 할당
df_midterm #데이터 제대로 출력