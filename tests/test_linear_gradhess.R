## This is the test script for the gradient and hessian for the liniear model.
Rprof()

setwd("~/workplace/MovingKnots/R/")
sourceDir(c("models", "stable"),recursive = TRUE)
#load("data/lidar.Rdata")

##
require("fields") 

x <- matrix(rnorm(2000), 200)
Y <- Y


x <- StdData(x,"-1to1")
splineArgs <- list(method = "thinplate", withInt = TRUE)
xi <- make.knots(x = x,k0 = 10,method = "mahalanobis-eball", list(RadiusShrink = 1))

XX <- d.matrix(x,xi,splineArgs)
B <- matrix(lm(Y~XX-1)$coef)
# Sigma <- vcov(lm(Y~0+XX))
Sigma <- matrix(var(lm(Y~0+XX)$res))

hessMethod <- "outer"
callParams <- "xi"
Params <- list(xi = xi)

## Priors setting for B and Sigma
M <- B # The prior mean
n0 <- 30 # df for Inverse Wishart prior
S0 <-  Sigma #
KnotsSubset <- 1:20
otherArgs <- list(ka = 221, M = M, n0 = n0, S0 = S0, KnotsSubset = KnotsSubset)
system.time(replicate(100, linear_gradhess(Params, hessMethod, Y, x, callParams,splineArgs,otherArgs)))

## Rprof(NULL)
## summaryRprof()
