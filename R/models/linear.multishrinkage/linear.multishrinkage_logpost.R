##' The conditinal and joint log posterior function
##'
##' Details.
##' @name 
##' @title 
##' @param x 
##' @param Params
##'         Params$xi:
##'         Parmas$K:
##' @param splineArgs "list".
##'         splineArgs$method:
##'         splineArgs$withInt:
##' @param prior_args "list".
##'         priorArgs$prior_type:
##'         priorArgs$n0:
##'         priorArgs$S0:
##'         priorArgs$mu:
##'         priorArgs$M:
##'         priorArgs$mu0:
##'         priorArgs$Sigma0:
##'         priorArgs$ka0:
##'         priorArgs$ka.mu0:
##'         priorArgs$ka.Sigma0:
##'         priorArgs$Sigma.mu0
##'         priorArgs$Sigma.Sigma0
##' @param callParam
##'        callParam$id: It should be able to obtain conditianl posterior or joint
##'        posterior or just likelihood.
##'        callParam$subset: If the paramater set is larg, we may only update a subset of them.
##' 
##' @return "scalar".
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Sat Nov 06 22:23:57 CET 2010;
##'       Current:       Sat Nov 06 22:24:06 CET 2010.
linear.multishrinkage_logpost <- function(Y, x, Params, callParam, splineArgs, priorArgs,
                                          Params_Transform) 
{


  xi.orig <- Params[["xi"]]
  diag.K.orig <- Params[["diag.K"]]
  vech.Sigma.orig <- Params[["vech.Sigma"]]
  B.orig <- Params[["B"]]

  xi <- par.transform(par = xi.orig, method = Params_Transform[["xi"]])
  diag.K <- par.transform(par = diag.K.orig, method = Params_Transform[["diag.K"]])
  vech.Sigma <- par.transform(par = vech.Sigma.orig, method = Params_Transform[["vech.Sigma"]])
  B <- par.transform(par = B.orig, method = Params_Transform[["B"]])
  
  ## Get the parameters
  K <- diag(as.vector(diag.K), length(diag.K))
  Sigma <- vech2m(vech.Sigma)
  
  X <- d.matrix(x,xi,splineArgs)
  dim.x <- dim(x)
  n <- dim.x[1]
  p <- dim(Y)[2]
  q <- dim(X)[2]
  
  M <- priorArgs$M
  mu <- priorArgs$mu
  
  n0 <- priorArgs$n0
  S0 <- priorArgs$S0
  
  logprior <- list()
  logpost <- list()
  
  
  if(tolower(callParam$id[1]) == "likelihood") # The full log likelihood
    {
      logpost[["likelihood"]] <- -n/2*determinant(2*pi*Sigma)$modulus[1] + 1/2*
        tr(solve(Sigma) %*% crossprod(Y-X%*%B))
    }
  else
    {

      ## Marginal likelihood
      P <- crossprod(X)
      P.inv <- try(solve(P), silent = TRUE) # Parameters boundary check
      if(is(P.inv, "try-error")) # Must be something wrong about the boundary.
        { loglike.margi <- NaN }
      else # all fine
        { 
          
          Sigma.inv <- solve(Sigma)
          C <- K^(1/2) %d*d% Sigma
          K.inv2 <- solve(sqrt(K)) # K^(-1/2) FIXME:
          C.inv <- K.inv2%d*d%Sigma.inv
      
          Sigma.tilde <- solve(Sigma.inv+ C.inv)
          B.tilde <- (P.inv%*%crossprod(X, Y) %*% Sigma.inv + M%*%C.inv)%*%Sigma.tilde
          E.tilde <- Y-X%*%B.tilde # 2 The residual
          S.tilde <- crossprod(E.tilde)/n  # Resd' * Resd
          D <- B.tilde-M # 3
          
          loglike.margi <- -(n0+n+p+q+1)/2*determinant(Sigma)$modulus[1] -
                1/2*determinant(Sigma.inv + C.inv)$modulus[1] -
                  1/2*determinant(K)$modulus[1] -
                    1/2*tr(Sigma.inv%*%(n0*S0+n*S.tilde+ K.inv2%d*%t(D)%*%P%*%(D%*d%K.inv2))) 
          
        }

      
      ## The priors w.r.t. differnt conditions
      if ("xi" %in% callParam$id) ## The prior for the knots
        {
          ## Get the priors paramters
          pri.type <- priorArgs$prior_type[["xi"]]
          pri.mean <- matrix(t(priorArgs$xi.mu0))
          pri.covariance <- priorArgs$xi.Sigma0
          pri.shrinkage <- priorArgs$xi.c
          logprior[["xi"]] <- log_prior(B = matrix(t(xi.orig), prod(dim(xi.orig)), 1), priorArgs = list(prior_type =
                                                                                    pri.type, mean = pri.mean,
                                                                                    covariance =
                                                                                    pri.covariance, shrinkage
                                                                                    =
                                                                                    pri.shrinkage))
        }
      if ("diag.K" %in% callParam$id) ## The prior for the  skrinkage
        {
          ## Get the priors paramters
          pri.type <- priorArgs$prior_type[["diag.K"]]
          pri.mean <- priorArgs$diag.K.mu0
          pri.covariance <- priorArgs$diag.K.Sigma0
          pri.shrinkage <- priorArgs$diag.K.c  
          logprior[["diag.K"]] <- log_prior(B = diag.K.orig, priorArgs = list(prior_type =
                                                               pri.type, mean = pri.mean,
                                                               covariance =
                                                               pri.covariance, shrinkage
                                                               =  pri.shrinkage))
        }
      if ("vech.Sigma" %in% callParam$id)
        {
          ## Pre-specified.
          logprior[["vech.Sigma"]] <- 0
        }  
      if(tolower(callParam$id[1]) == "marginal-likelihood") # Only return likelihood.
        {  logprior[["likelihood"]] <- 0 }
      
      logpost[["margi"]] <- loglike.margi + sum(unlist(logprior))
      
      if("B" %in% callParam$id) 
        {
          beta <- matrix(B, 1, p*q)
          beta.tilde <- matrix(B.tilde)
          Norm.Sigma0 <- Sigma.tilde %x% P.inv
          Norm.Sigma <- (Norm.Sigma0 + t(Norm.Sigma0))/2
          
          logpost[["B"]] <- dmvnorm(x = beta, mean = beta.tilde, sigma = Norm.Sigma, log = TRUE)
        }
      
    }
      
  out <- sum(unlist(logpost))
  return(out)
}
