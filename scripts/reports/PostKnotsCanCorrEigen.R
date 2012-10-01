## Canonical correlation study among the different components in the moving
## knots project.

rm(list = ls())

Rdatafile <- "/home/fli/archives/ResearchPapers/MovingKnots/running/welldone/rajan_s_moving_8_plus_a_moving_4_2011Apr02-20.55.26.Rdata"


## Which cross validation fold to use
foldCur <- 1


## Load the MCMC file
load(Rdatafile)


knotsCur <- knots.mat2list(knots.mat = OUT.Params.mode$knots[, , foldCur],
                           splineArgs = splineArgs)
xPost <- d.matrix(x = x,
                  knots = knotsCur,
                  splineArgs = splineArgs)

isIntercept <- "intercept" %in% splineArgs$comp
nCov <- ncol(x)
idx.s <- (1+nCov+isIntercept):(splineArgs$thinplate.s.dim[1]+isIntercept+nCov)
idx.a <- (splineArgs$thinplate.s.dim[1]+isIntercept+1+nCov):q


canCorr.a1 <- sample(idx.a, size = round(length(idx.a)/2))
canCorr.a2 <- idx.a[!idx.a%in%canCorr.a1]


options(width = 100)
###----------------------------------------------------------------------------
### THE CANONICAL CORRELATION
###----------------------------------------------------------------------------

## WITHIN ADDITIVE KNOTS COMPONENT (PART 1 AND PART 2)
cancor(xPost[, canCorr.a1], xPost[, canCorr.a2])$cor

## BETWEEN ADDITIVE AND SURFACE COMPONENTS (WITH PART 1)
cancor(xPost[, idx.s], xPost[, canCorr.a1])$cor

## BETWEEN ADDITIVE AND SURFACE COMPONENTS (WITH PART 2)
cancor(xPost[, idx.s], xPost[, canCorr.a2])$cor


###----------------------------------------------------------------------------
### THE EIGENVALUES
###----------------------------------------------------------------------------

## WITHIN ADDITIVE KNOTS COMPONENT (PART 1 AND PART 2)
eigen(crossprod(xPost[, canCorr.a1]))$values

eigen(crossprod(xPost[, canCorr.a2]))$values

eigen(crossprod(xPost[, c(canCorr.a1, canCorr.a2)]))$values


## WITHIN ADDITIVE KNOTS COMPONENT
eigen(crossprod(xPost[, idx.s]))$values


## BETWEEN ADDITIVE AND SURFACE COMPONENTS (PART 1)
eigen(crossprod(xPost[, c(idx.s, canCorr.a1)]))$values

eigen(crossprod(xPost[, c(idx.s, canCorr.a2)]))$values

eigen(crossprod(xPost[, c(idx.s, canCorr.a1, canCorr.a2)]))$values
