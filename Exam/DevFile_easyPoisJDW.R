## Load libraries and set working directory
library(devtools)
library(roxygen2)

setwd("C:/Users/jorda/OneDrive/2022 Spring/POLS 5265 - Applied Statistical Programming/Class [Git]/AppliedStatisticalProgramming2022_FromHome/Exam")

# Verifying the package
current.code <- as.package("easyPoisJDW")
load_all(current.code)
document(current.code)
check(current.code)
