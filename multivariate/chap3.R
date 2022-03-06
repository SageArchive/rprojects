# Chap3. Factor Analysis

# R 3.1
med_data = read.csv("c:/data/mva/medFactor.csv")
head(med_data)
summary(med_data)

# Significance Test of Factor Analysis
cortest.bartlett(med_data)
KMO(med_data)

#R 3.2
#install.packages("psych")
#install.packages("GPArotation")
library(psych)
library(GPArotation)
med_factor = principal(med_data, rotate="none")
names(med_factor)
med_factor$values
plot(med_factor$values, type="b", pch=19) 

#R 3.3
med_varimax = principal(med_data, nfactors=3, rotate="varimax", scores=T, method="regression")
med_varimax

#R 3.4
head(med_varimax$scores)

# R 3.5
med_oblimin = principal(med_data, nfactors=3, rotate="oblimin", scores=T, method="regression")
med_oblimin

# R 3.6
head(med_oblimin$scores)

# R 3.7
biplot(med_varimax)

# R 3.8
head(state.x77, 3)
state = state.x77
summary(state)

# R 3.9
library(stats)
state_fact0 = factanal(state, factors=4)
sosq = function(v) { sum(v^2) }
loadings = as.matrix(state_fact0$loadings)
eigen_value = apply(loadings, 2, sosq)
round(eigen_value, 3)
round(apply(state_fact0$loadings^2, 2, sum), 3) #same result

# R 3.10
library(stats)
state_fact = factanal(state, factors=3, rotation="none")
state_fact1 = factanal(state, factors=3, rotation="varimax")
state_fact2 = factanal(state, factors=3, rotation="promax")
names(state_fact)

# R 3.11
state_fact1 = factanal(state, factors=3, rotation="varimax")
state_fact1

# R 3.12
state_fact1_1 = factanal(state[,-1], factors=3, rotation="varimax", scores="Bartlett")
state_fact1_1