# Chap3. 인자분석(Factor Analysis)

# 자료 가져오기 및 요약통계량
med_data = read.csv("c:/data/mva/medFactor.csv")
head(med_data)
summary(med_data)

# Significance Test of Factor Analysis
# install.packages("psych")
library(psych)
cortest.bartlett(med_data)
KMO(med_data)

# 초기 인자분석 실행
# install.packages("GPArotation")
library(GPArotation)
med_factor = principal(med_data, rotate="none")
names(med_factor)
med_factor$values
plot(med_factor$values, type="b", pch=19) 

# varimax 인자회전
med_varimax = principal(med_data, nfactors=3, rotate="varimax", scores=T, method="regression")
med_varimax

# varimax 인자점수
head(med_varimax$scores)

# oblimin 인자회전
med_oblimin = principal(med_data, nfactors=3, rotate="oblimin", scores=T, method="regression")
med_oblimin

# oblimin 인자점수
head(med_oblimin$scores)

# 행렬도
biplot(med_varimax)

# 자료 및 요약통계량
head(state.x77, 3)
state = state.x77
summary(state)

# 인자분석 by 최우추정법
# 고윳값 구하기
library(stats)
state_fact0 = factanal(state, factors=4)
sosq = function(v) { sum(v^2) }
loadings = as.matrix(state_fact0$loadings)
eigen_value = apply(loadings, 2, sosq)
round(eigen_value, 3)
round(apply(state_fact0$loadings^2, 2, sum), 3) #same result

# 인자분석 실행
library(stats)
state_fact = factanal(state, factors=3, rotation="none")
state_fact1 = factanal(state, factors=3, rotation="varimax")
state_fact2 = factanal(state, factors=3, rotation="promax")
names(state_fact)

# varimax 회전 결과(Population 제외 전)
state_fact1 = factanal(state, factors=3, rotation="varimax")
state_fact1

# varimax 회전 결과(Population 제외)
state_fact1_1 = factanal(state[,-1], factors=3, rotation="varimax", scores="Bartlett")
state_fact1_1