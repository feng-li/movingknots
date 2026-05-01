## Canonical correlation study among the different components in the moving
## knots project.

###----------------------------------------------------------------------------
### CONFIGURATIONS
###----------------------------------------------------------------------------
rm(list = ls())

Rdatafile <- "/home/fli/archives/ResearchPapers/MovingKnots/running/welldone/rajan_s_moving_8_plus_a_moving_4_2011Apr02-20.55.26.Rdata"


## Which cross validation fold to use
foldCur <- 1


## How many random sample draws
nRndSample <- 1000

## Console print width
options(width = 100)

###----------------------------------------------------------------------------
### THE MAIN SCRIPT
###----------------------------------------------------------------------------

## Load the MCMC file
load(Rdatafile)


knotsCur <- knots_mat2list(knots.mat = OUT.Params.mode$knots[, , foldCur],
                           splineArgs = splineArgs)
xPost <- d.matrix(x = x,
                  knots = knotsCur,
                  splineArgs = splineArgs)

isIntercept <- "intercept" %in% splineArgs$comp
nCov <- ncol(x)
idx.s <- (1+nCov+isIntercept):(splineArgs$thinplate.s.dim[1]+isIntercept+nCov)
idx.a <- (splineArgs$thinplate.s.dim[1]+isIntercept+1+nCov):q


## Reserve the storage
OUT.cancor.a <- matrix(NA, nRndSample, length(idx.a)/2)
OUT.cancor.sa1 <- matrix(NA, nRndSample, length(idx.a))
OUT.cancor.sa2 <- matrix(NA, nRndSample, length(idx.a))

OUT.eigen.a1 <- matrix(NA, nRndSample, length(idx.a)/2)
OUT.eigen.a2 <- matrix(NA, nRndSample, length(idx.a)/2)
OUT.eigen.a12 <- matrix(NA, nRndSample, length(idx.a))
OUT.eigen.s <- matrix(NA, 1, length(idx.a))
OUT.eigen.sa1 <- matrix(NA, nRndSample, length(idx.a))
OUT.eigen.sa2 <- matrix(NA, nRndSample, length(idx.a))
OUT.eigen.sa12 <- matrix(NA, nRndSample, length(idx.a)+length(idx.s))


## The conditional number
OUT.cond.a1 <- matrix(NA, nRndSample, 1)
OUT.cond.a2 <- matrix(NA, nRndSample, 1)
OUT.cond.a12 <- matrix(NA, nRndSample, 1)
OUT.cond.s <- matrix(NA, 1, 1)
OUT.cond.sa1 <- matrix(NA, nRndSample, 1)
OUT.cond.sa2 <- matrix(NA, nRndSample, 1)
OUT.cond.sa12 <- matrix(NA, nRndSample, 1)



for(i in 1:nRndSample)
  {
    canCorr.a1 <- sample(idx.a, size = round(length(idx.a)/2))
    canCorr.a2 <- idx.a[!idx.a%in%canCorr.a1]


###----------------------------------------------------------------------------
### THE CANONICAL CORRELATION
###----------------------------------------------------------------------------

    ## WITHIN ADDITIVE KNOTS COMPONENT (PART 1 AND PART 2)
    OUT.cancor.a[i, ] <- cancor(xPost[, canCorr.a1], xPost[, canCorr.a2])$cor

    ## BETWEEN ADDITIVE AND SURFACE COMPONENTS (WITH PART 1)
    OUT.cancor.sa1[i, ] <- cancor(xPost[, idx.s], xPost[, canCorr.a1])$cor

    ## BETWEEN ADDITIVE AND SURFACE COMPONENTS (WITH PART 2)
    OUT.cancor.sa2[i, ] <- cancor(xPost[, idx.s], xPost[, canCorr.a2])$cor

###----------------------------------------------------------------------------
### THE EIGENVALUES
###----------------------------------------------------------------------------

    ## WITHIN ADDITIVE KNOTS COMPONENT (PART 1 AND PART 2)
    OUT.eigen.a1[i, ] <- eigen(crossprod(xPost[, canCorr.a1]))$values

    OUT.eigen.a2[i, ] <- eigen(crossprod(xPost[, canCorr.a2]))$values

    OUT.eigen.a12[i, ] <- eigen(crossprod(xPost[, c(canCorr.a1, canCorr.a2)]))$values

    ## BETWEEN ADDITIVE AND SURFACE COMPONENTS (PART 1)
    OUT.eigen.sa1[i, ] <- eigen(crossprod(xPost[, c(idx.s, canCorr.a1)]))$values

    OUT.eigen.sa2[i, ] <- eigen(crossprod(xPost[, c(idx.s, canCorr.a2)]))$values

    OUT.eigen.sa12[i, ] <- eigen(crossprod(xPost[, c(idx.s, canCorr.a1, canCorr.a2)]))$values


### The conditional number

    ## WITHIN ADDITIVE KNOTS COMPONENT (PART 1 AND PART 2)
    OUT.cond.a1[i ] <- rcond(crossprod(xPost[, canCorr.a1]))

    OUT.cond.a2[i ] <- rcond(crossprod(xPost[, canCorr.a2]))

    OUT.cond.a12[i ] <- rcond(crossprod(xPost[, c(canCorr.a1, canCorr.a2)]))

    ## BETWEEN ADDITIVE AND SURFACE COMPONENTS (PART 1)
    OUT.cond.sa1[i ] <- rcond(crossprod(xPost[, c(idx.s, canCorr.a1)]))

    OUT.cond.sa2[i ] <- rcond(crossprod(xPost[, c(idx.s, canCorr.a2)]))

    OUT.cond.sa12[i ] <- rcond(crossprod(xPost[, c(idx.s, canCorr.a1, canCorr.a2)]))

  }

    ## WITHIN ADDITIVE KNOTS COMPONENT
    OUT.eigen.s <- eigen(crossprod(xPost[, idx.s]))$values
    OUT.cond.s <- rcond(crossprod(xPost[, idx.s]))
