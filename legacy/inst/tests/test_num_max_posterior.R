## This is just a test to chech if the numerical maximum is working or not
myfun <- function(vecxi, ...)
  {

    xi <- t(matrix(vecxi, 2))
    
    splineArgs <- list(method = spline.type, withInt = withInt)
    prior_type <- "mvnorm"
    prior_args <- list(n0 = n0, ka = ka, M = M, S0 = S0, mu = mu, 
                       ka0 = ka0, Sigma0 = Sigma0, mu0 = mu0)

    logpost <- linear_logpost(x, xi, splineArgs, prior_type, prior_args)
    return(-logpost)
  }

vecxi <- matrix(t(xi), , 1)

iter <- csminwel(myfun,vecxi, H0 = diag(10), nit = 10000)
