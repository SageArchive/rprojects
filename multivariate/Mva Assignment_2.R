# 인자분석 연습문제 3장 3번 

# 결측치 제거
# sum(is.na(fifa))
# colSums(is.na(fifa))
# fifa_data <- na.omit(fifa)

# 자료 가져오기 및 기술통계량
fifa = read.csv("c:/data/mva/FIFA21_official_data_withoutNaN.csv")
head(fifa)
# 변수 선택
fifa_data = fifa[,-1] #ID는 능력치와 무관하므로 변수에서 제거
summary(fifa_data)

# 인자분석 유의성 검정
library(psych)
cortest.bartlett(fifa_data) # p.value=0으로 귀무가설 기각, 통계적으로 유의
KMO(fifa_data) # Overall MSA 값이 0.97로 0.6보다 크므로 인자분석이 가능

# 초기 인자분석 실행 by 주성분 인자법
library(psych)
library(GPArotation)
fifa_factor = principal(fifa_data, rotate="none")
names(fifa_factor)
fifa_factor$values # 다섯 번째 인자까지 고유근이 1이상임
plot(fifa_factor$values, type="b", pch=19) # 스크리 그림에서는 여섯 번째 인자부터 그래프의 기울기가 완만해짐 -> 유효한 인자의 수는 5개
title("Scree Plot")

# varimax 인자회전
fifa_varimax = principal(fifa_data, nfactors=5, rotate="varimax", scores=T, method="regression")
fifa_varimax
print(fifa_varimax$loadings, digits = 2, cutoff = 0.5, sort = TRUE)

# 인자점수 by 추정방법: 회귀분석
head(fifa_varimax$scores)
biplot(fifa_varimax)

# 인자분석 실행 by 최우추정법
library(stats)
fifa_fact0 = factanal(fifa_data1, factors=10) # varimax 인자회전이 디폴트
sosq = function(v) { sum(v^2) }
loadings = as.matrix(fifa_fact0$loadings)
eigen_value = apply(loadings, 2, sosq)
round(eigen_value, 3) # 여섯 번째 인자까지만 고윳값이 1 이상이므로 6개의 인자가 유효하다고 판단
round(apply(fifa_fact0$loadings^2, 2, sum), 3) #same result

fifa_fact = factanal(fifa_data1, factors=10, rotation="none")
names(fifa_fact)
loadings = as.matrix(fifa_fact$loadings)
eigen_value = apply(loadings, 2, sosq)
round(eigen_value, 3)

# varimax 회전 결과
fifa_fact1 = factanal(fifa_data1, factors=6, rotation="varimax", scores="Bartlett")
fifa_fact1
print(fifa_fact1$loadings, digits = 2, cutoff = 0.5, sort = TRUE)
names(fifa_fact1)

# 인자점수
head(fifa_fact1$scores)

# promax 회전 결과
fifa_fact2 = factanal(fifa_data1, factors=6, rotation="promax")
fifa_fact2
print(fifa_fact2$loadings, digits = 2, cutoff = 0.5, sort = TRUE)
names(state_fact)
write.csv(fifa_data, "c:/data/mva/FIFA21_official_data_withoutNaN.csv", row.names = FALSE)
