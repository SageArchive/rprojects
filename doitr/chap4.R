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