### Load libraries and set working directory
library(devtools)
library(roxygen2)

setwd("C:/Users/jorda/OneDrive/2022 Spring/POLS 5265 - Applied Statistical Programming/Class [Git]/AppliedStatisticalProgramming2022_FromHome/Exam")

### Verifying the package
current.code <- as.package("easyPoisJDW")
load_all(current.code)
document(current.code)
check(current.code)

### Some testing
set.seed(666)
y <- rpois(1000, 10)

# Each function individually
my_mle <- mle(y)
myLL <- logLik(y, lambda = my_mle)
mySE_basic <- standardError(y, "basic")
mySE_boot <- standardError(y, "bootstrap", B = 1000)

# And the final result
estimatePois(y, SEtype = "basic")
estimatePois(y, SEtype = "bootstrap", B = 1000)

# Help functions
?mle()
?logLik()
?standardError()
?estimatePois()
