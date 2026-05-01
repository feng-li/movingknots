xx = x
splineArgs <- list(method = spline.type, withInt = withInt)

prior_type <- "mvnorm"
prior_args <- list(n0 = n0, ka = ka, M = M, S0 = S0, mu = mu, 
                   ka0 = ka0, Sigma0 = Sigma0, mu0 = mu0)

logpost <- linear_logpost(x, xi, splineArgs, prior_type, prior_args)

myfun <- function(x, ...)
  {
    xi <- t(matrix(x, 2))
    linear_logpost(x = xx , xi, splineArgs, prior_type, prior_args)
  }
grad.num <- numgrad(myfun, x = matrix(t(xi)))


myfun <- function(x)
  {
    sum(x^2)
  }
set.seed(1)
numgrad(myfun, rnorm(10))
