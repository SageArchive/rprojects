# R 3.8
state = state.x77
summary(state)

# R 3.9
library(stats)
state_fact0 = factanal(state, factors=4)
sosq = function(v) { sum(v^2) }
loadings = as.matrix(state_fact0$loadings)
eigen_value = apply(loadings, 2, sosq)
round(eigen_value, 3)

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
state_fact1_1 = factanal(state[,-1], factors=3, rotation="varimax",
                         scores="Bartlett")
state_fact1_1
