##----------------------------------------------------------------------------------------
## Check gradient: ??
## put brower() at the end of linear_gradhess.R and paste these lines in
##----------------------------------------------------------------------------------------

myfun<-function(vecX, ...)
  {
    xi <- matrix(vecX, )
    linear_logpost(...)
  }

  

out.num <- numgrad(myfun, vecX)$g




## testing for analytical gradients
xx = x
myfun <- function(x, ...)
  {
    
    ## xi <- t(matrix(x, 2))
    ka <- x
    linear_logpost(Y = Y, x = xx, Params = list(xi = xi, ka = ka), callParam = list(id =
    "ka", subset = subsets.location[[1]][[1]]),
                   splineArgs = splineArgs, priorArgs = priorArgs)        
  }
## grad.num <- numgrad(myfun, x = matrix(t(xi)))
grad.num <- numgrad(myfun, x = ka)

## stop("STOP HERE")




##----------------------------------------------------------------------------------------
## D_lnx: Passed
##----------------------------------------------------------------------------------------
myfun <- function(x)
  {
    Xx <- matrix(x, 200, 5)
    out <- determinant(crossprod(Xx))$modulus
    out
  }

vecX <- matrix(rnorm(1000))
n <- 200
p <- 5
out.num <- numgrad(myfun, vecX)$g

Xx <- matrix(vecX, 200, 5)
out.the <- matrix(2*Xx%*%solve(crossprod(Xx)), , 1)
plot(out.the, out.num)
##----------------------------------------------------------------------------------------
## D_lnx2: Passed
##----------------------------------------------------------------------------------------

myfun <- function(x)
  {
    Xx <- matrix(x, 100, 100)
    out <- determinant(Xx)$modulus
    out
  }

vecX <- matrix(rnorm(10000))
out.num <- numgrad(myfun, vecX)$g
Xx <- matrix(vecX, 100, 100)
out.the <- matrix(solve(t(Xx)), , 1)
plot(out.the, out.num)

##----------------------------------------------------------------------------------------
## D_lnx2: Passed
##----------------------------------------------------------------------------------------

myfun <- function(x)
  {
    Xx <- matrix(x, 100, 5)
    out <- solve(crossprod(Xx))
    matrix(out, , 1)
  }

vecX <- matrix(rnorm(500))
out.num <- numgrad(myfun, vecX)$g
Xx <- matrix(vecX, 100, 5)
out.the <- matrix(solve(t(Xx)), , 1)
plot(out.the, out.num)


##----------------------------------------------------------------------------------------
## D_lnx2: Passed
##----------------------------------------------------------------------------------------

myfun <- function(x)
  {
    sum(x^2)
  }

vecX <- rnorm(10)
numgrad(myfun, vecX)
