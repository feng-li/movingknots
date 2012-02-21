##' The conditinal and joint log posterior function
##'
##' Details.
##' @name 
##' @title 
##' @param x 
##' @param Params
##'         Params$xi:
##'         Parmas$ka:
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
##' @param callParam
##'        callParam$id: It should be able to obtain conditianl posterior or joint
##'        posterior or just likelihood.
##'        callParam$subset: If the paramater set is larg, we may only update a subset of them.
##' 
##' @return "scalar".
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
##' TODO: The joint posterior density
linear.singleshrinkage_logpost <- function(Y, x, Params, callParam, splineArgs, priorArgs,
                                           Params_Transform) 
{

  xi.orig <- Params[["xi"]]
  ka.orig <- as.vector(Params[["ka"]])
  B.orig <- Params[["B"]]
  Sigma.orig <- Params[["Sigma"]]

  xi <- par.transform(par = xi.orig, method = Params_Transform[["xi"]])
  ka <- par.transform(par = ka.orig, method = Params_Transform[["ka"]])
  B <- par.transform(par = B.orig, method = Params_Transform[["B"]])
  Sigma <- par.transform(par = Sigma.orig, method = Params_Transform[["Sigma"]])
  
  X <- d.matrix(x,xi,splineArgs)
  dim.x <- dim(x)
  n <- dim.x[1]
  p <- dim(Y)[2]
  q <- dim(X)[2]
  
  M <- priorArgs$M
  mu <- priorArgs$mu
  n0 <- priorArgs$n0
  S0 <- priorArgs$S0
  
  P <- crossprod(X)
  P.inv <- try(solve(P), silent = TRUE) # Parameters boundary check
  
  logprior <- list()
  logpost <- list()
  
  
  if(tolower(callParam$id[1]) == "likelihood") # The full log likelihood
    {
      logpost[["likelihood"]] <- -n/2*determinant(2*pi*Sigma)$modulus[1] + 1/2*
                                 tr(solve(Sigma) %*% crossprod(Y-X%*%B)) 
    }
  else
    {

      ## The marginal likelihood
      if(is(P.inv, "try-error")) # Must be something wrong about the boundary.
        { loglike.margi <- NaN }
      else # all fine,  give the marginal likelihood
        { 

          B.tilde <- 1/(1+1/ka)*P.inv%*%(crossprod(X, Y)+1/ka*P%*%M)
          E.tilde <- Y-X %*% B.tilde # 2 The residual
          S.tilde <- crossprod(E.tilde)/n  # Resd' * Resd
          B.tilde_M <- B.tilde-M # 3
          S.tilde_S0 <- n0*S0+n*S.tilde+1/ka*crossprod(B.tilde_M, P)%*%B.tilde_M
          
          log.r <- (p*q/2)*log(1/ka) - mvgamma(p, n0/2, log = TRUE)+
            p/2*determinant(P)$modulus[1]+n0/2*determinant(n0*S0)$modulus[1]
          
          loglike.margi <- log.r-n*p/2*log(pi)+mvgamma(p, (n+n0)/2, log = TRUE)-
            p/2*determinant((1/ka+1)*P)$modulus[1]-
              (n+n0)/2*determinant(S.tilde_S0)$modulus[1] 
        }
      
      
      ## The priors
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
                                                                                         =  pri.shrinkage))
        }
      if ("ka" %in% callParam$id) ## The prior for the  skrinkage
        {
          ## Get the priors paramters
          pri.type <- priorArgs$prior_type[["ka"]]
          pri.mean <- matrix(priorArgs$ka.mu0)
          pri.covariance <- matrix(priorArgs$ka.Sigma0)
          pri.shrinkage <- priorArgs$ka.c # ka is just a scaler
          logprior[["ka"]] <- log_prior(B = matrix(ka.orig), priorArgs = list(prior_type =
                                                               pri.type, mean = pri.mean,
                                                               covariance =
                                                               pri.covariance, shrinkage
                                                               =  pri.shrinkage))
        }
      if (tolower(callParam$id[1]) == "marginal-likelihood") # Only return log (marginal) likelihood.
        {
          logprior[["margi-likelihood"]] <- 0
        }
      logpost[["margi"]] <- loglike.margi + sum(unlist(logprior))
         
      if("B" %in% callParam$id) 
        {
          beta <- matrix(B, 1, p*q)
          beta.tilde <- matrix(B.tilde)
          Norm.Sigma0 <- Sigma %x% P.inv * 1/(1/ka +1)
          Norm.Sigma <- (Norm.Sigma + t(Norm.Sigma))/2
          logpost[["B"]] <- dmvnorm(x = beta, mean = beta.tilde, sigma = Norm.Sigma, log = TRUE)
        }
      if ("Sigma" %in% callParam$id)
        {
          Omega <- S.tilde_S0
          logpost[["Sigma"]] <- diwishart(X = Sigma, df = n+n0, V = Omega, log = TRUE)
        }
      
    }
  
  ## The log posterior(conditional or joint) or loglikelihood (marginal or conditional)
  out <- sum(unlist(logpost))
  
  return(out)
}
