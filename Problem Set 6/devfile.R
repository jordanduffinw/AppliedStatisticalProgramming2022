##### LIBRARIES #####
library(devtools)
library(roxygen2)

##### DEV STUFF #####
current.code <- as.package("ps6EBMA")
load_all(current.code)
document(current.code)

##### TESTING USING `EBMAforecast` PACKAGE #####
library(EBMAforecast)
dat <- presidentialForecast
 
# Testing z_hat with x, y, weights from the presidentialForecast data
x <- as.matrix(dat[,1:6])
y = dat[,7]
my_weights <- replicate(dim(x)[2], 1/dim(x)[2])

help(z_hat)
my_z_hat <- z_hat(x = x,
                  y = y,
                  weights = my_weights,
                  sd = 1)
my_z_hat

# Testing w_hat
help(w_hat)
my_w_hat <- w_hat(my_z_hat)
my_w_hat

# And the full EM algorithm
help(fullEM)
my_fullEM <- fullEM(x = x,
                    y = y,
                    weights = my_weights,
                    sd = 1,
                    tolerance = 0.00005)
my_fullEM
