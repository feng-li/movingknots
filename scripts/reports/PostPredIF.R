## The predictive efficiency factor

###----------------------------------------------------------------------------
### INPUTS
###----------------------------------------------------------------------------

## Load the results dataset
setwd("/home/fli/running/alt-update/Results")

RdataFiles <- dir(pattern = "rajan_s_moving_20_plus_a_moving_4")

nTest <- n
## x.testing <- matrix(rnorm(n = nTest*ncol(x), mean = 0, sd = 1), nTest)
x.testing <- x[sample(1:n, nTest), ]
## Which cross validation fold used
iCross <- 1

###----------------------------------------------------------------------------
### The scripts
###----------------------------------------------------------------------------

OUT.IF.testing <- matrix(NA, nTest, length(RdataFiles))
nTest <- nrow(x.testing)

Y.mcmc <- matrix(NA, nTest,nIter)

for(j in 1:length(RdataFiles))
{
  load(RdataFiles[j])
  source("~/workspace/MovingKnots/R/stable/ineff.R") ## The correct version

  for(i in 1:nIter)
    {
      knots.ilst <- knots.mat2list(OUT.Params[["knots"]][, , i, iCross], splineArgs)
      ## knots.s.mat[(1+q.s*(i-1)):(i*q.s), ] <- knots.ilst[["thinplate.s"]]
      ## knots.a.mat[(1+q.a1*(i-1)):(i*q.a1), ] <- matrix(knots.ilst[["thinplate.a"]], q.a1, 2)
      X.i <- d.matrix(x.testing, knots.ilst, splineArgs)
      B.i <- matrix(OUT.Params[["coefficients"]][, , i, iCross], , p)
      Y.mcmc[, i] <- X.i %*% B.i # Transformed scale,  but should be OK here.
    }

 OUT.IF.testing[, j] <- apply(Y.mcmc[, nIter:1], 1, ineff)

}
