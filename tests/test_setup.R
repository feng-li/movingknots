##########################################################################################
##                               Testings                                               ##
##########################################################################################

##----------------------------------------------------------------------------------------
## Testing: numerical maximum posterior working? PASSED.
##----------------------------------------------------------------------------------------
## This is just a test to check if the numerical maximum is working or not
## myfun <- function(vecxi, ...)
##   {

##     xi <- t(matrix(vecxi, 2))
    
##     splineArgs <- list(method = spline.type, withInt = withInt)
##     prior_type <- "mvnorm"
##     prior_args <- list(n0 = n0, ka = ka, M = M, S0 = S0, mu = mu, 
##                        ka0 = ka0, Sigma0 = Sigma0, mu0 = mu0)

##     logpost <- linear_logpost(x, xi, splineArgs, prior_type, prior_args)
##     return(-logpost)
##   }

## vecxi <- matrix(t(xi), , 1)

## iter <- csminwel(myfun,vecxi, H0 = diag(k*2), nit = 10000)

## plot(xi.desi, xlim = c(0, 1), ylim = c(0, 1))
## xi.new = t(matrix(iter$x, 2))
## points(xi.new, col = "red")

##----------------------------------------------------------------------------------------
## Testing: analytical gradient correct? PASSED.
##----------------------------------------------------------------------------------------

## testing gradient
## xx = x
## splineArgs <- list(method = spline.type, withInt = withInt)

## prior_type <- "mvnorm"
## prior_args <- list(n0 = n0, ka = ka, M = M, S0 = S0, mu = mu, 
##                    ka0 = ka0, Sigma0 = Sigma0, mu0 = mu0)

## logpost <- linear_logpost(x, xi, splineArgs, prior_type, prior_args)

## myfun <- function(x, ...)
##   {
##     xi <- t(matrix(x, 2))
##     linear_logpost(x = xx , xi, splineArgs, prior_type, prior_args)
##   }
## grad.num <- numgrad(myfun, x = matrix(t(xi)))


##----------------------------------------------------------------------------------------
## Testing: pure Newton method working? PASSED
##----------------------------------------------------------------------------------------

## sub.idx <- knots.subsets[[1]]
## param.cur <- matrix(t(xi), , 1)
## for(k in 1:nNewtonStep)
##   {
##     gradhess <- linear_gradhess(Params = list(xi = xi), hessMethod, Y, x, callParams
##                                 = "xi", splineArgs = list(method = spline.type,
##                                           withInt = withInt), otherArgs =
##                                 list(KnotsSubset = sub.idx, ka = ka,ka0 =
##                                      ka0, M = M, n0 = n0, mu0 = mu0, Sigma0 =
##                                      Sigma0, S0 = S0,  prior_type =
##                                      knots.prior.type))    
##     grad.cur <- gradhess$gradObs
##     hess.cur <- gradhess$hessObs
##     invHess <- solve(hess.cur)
##     param.cur <- param.cur - invHess%*%grad.cur # 0ne-column matrix
##     xi <- t(matrix(param.cur, m))
##   } # for(k in 1:nNewtonStep)
