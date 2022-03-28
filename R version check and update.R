#When R version error happens, check current R version
package_version(R.version)

#Solve the problem as follows
install.packages("installr")
library(installr)
check.for.updates.R()
install.R()