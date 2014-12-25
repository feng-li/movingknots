##' This is the gradient and hessian matrix for the simple spline model with conjugate
##' priors.                 
##'
##' This function will only return the gradient and hessian part for the *labeled* knots.
##' It is possiable to provide the full gradient and hessian matrix if the parameter
##' otherArgs$subsets contains the full knots lables.
##' 
##' @name linear_gradhess
##' @title Gradient and Hessian matrix for the "marginal posterior" for knots in the
##'        linear model. 
##' 
##' @param Params "list".
##'         Params$xi:
##'         Contains the matrices for the parameters.Currently only "xi" is implemented.
##'         Params$ka: The shrinkage for B variance
##'         Params$Sigma: 
##' @param hessMethod "character".
##'         Method to be used in the Hessian approximation.
##' @param Y "matrix".
##'         The response matrix, we consider the multivariate case.
##' @param x "matrix".
##'         The covaritates, you should *NOT* provide the intercept.
##'         It will be added automatically if necessary.
##' @param callParams "character".
##'         callParams$id: the calling tag for the gradient. Possible values is "xi". "ka"
##'         callParams$subset: not all updated.
##' @param splineArgs "list".
##'         Parameters for the spline options to pass to the function, where
##'         splineArgs$method should be the spline method for x, see also d.matrix().
##'         splineArgs$method: "character". The method of splines, which can be
##'         "thinplate". 
##'         splineArgs$withInt: "logical". If TRUE, will return design matrix with
##'         intercept; if FALSE, design matrix without intercept.
##' @param priorArgs
##'         priorArgs$prior_type: gradient type for prior
##'         priorArgs$M: mean of B,  q-by-q
##'         priorArgs$n0: df. of inverse wishart distribution. 
##'         priorArgs$S0: Covariance matrix from the prior of B.
##'         priorArgs$xi.mu0: Mean of the knots locations from the prior
##'         priorArgs$xi.Sigma0: Covariance matrix from the prior of knots
##'         priorArgs$K.mu0: mean for ka
##'         priorArgs$K.Sigma0: variance for ka
##' 
##' @return "list",  see bellow.
##' \item   {gradObs}
##'         {"matrix". n*p-by-1 The observed gradient for xi.}
##' \item   {hessObs}
##'         {"matrix". The obsered Hessian matrix for xi.}
##' 
##' @references Villani's note
##' @author Feng Li, Dept. of Statistics, Stockholm University, Sweden.
##' @note First version: Fri Aug 27 10:36:38 CEST 2010.
##'       Current:	 Wed Sep 15 23:57:50 CEST 2010.
##' 
##' DEPENDS:
##' TODO: Add the gradient for shrinkage parameter.
linear.multishrinkage_gradhess <- function(Params, hessMethod, Y, x, callParam,
                                           splineArgs, priorArgs, Params_Transform)  
{
  ## Let me explain what I want to do here.
  ## 1) The *gradient* fot some knot condition on other knots
  ## 2) The *hessian matrix*
  ## I should be careful about the order and subsets where containning knots,  xi.
  
  xi.orig <- Params[["xi"]]
  diag.K.orig <- Params[["diag.K"]]
  vech.Sigma.orig <- Params[["vech.Sigma"]]

  xi <- par.transform(par = xi.orig, method = Params_Transform[["xi"]])
  diag.K <- par.transform(par = diag.K.orig, method = Params_Transform[["diag.K"]])
  vech.Sigma <- par.transform(par = vech.Sigma.orig, method = Params_Transform[["vech.Sigma"]])

  #print(diag.K.orig)
  #print(diag.K)
  
  if(callParam$id == "xi")
    {
      
      x.dim <- dim(x) 
      m <- x.dim[2] # no. of covariates(dimension).
      n <- x.dim[1] # no. of obs.

      k <- dim(xi.orig)[1] # no. of total knots
      no.subset <- length(callParam$subset) # no. of parameters in the subset
      k0 <- no.subset/m # no. of knots location to be updated

      KnotsSubset<- matrix(callParam$subset, m)[1, ] # Detect which knots are going to update.
      p <- dim(Y)[2]  # dimension of response variable,  p > 1 for multivariate model
      OrigKnotsOrder <- 1:k # The original knots order.

      ## Reorder the knots matrix so that the knots to be gradiented should be alway in
      ## font of other knots. This makes it convenient to partition the matrix later.
      ## This shall not affect B(slop) and Sigma(covariance matrix).
      NewKnotsOrder <- c(KnotsSubset,OrigKnotsOrder[-KnotsSubset]) 

      xi2 <- matrix(xi[NewKnotsOrder,], k, m)  # Reorder the knots matrix and don't
                                        # drop the dim in case the dim is NULL
      K <- diag(as.vector(diag.K), length(diag.K))

      Sigma <- vech2m(vech.Sigma)
      
      M <- priorArgs$M # mean of B,  q-by-p
     
      n0 <- priorArgs$n0 # df. of inverse wishart distribution. 
      S0 <- priorArgs$S0 #
      xi.mu0 <- priorArgs$xi.mu0 # Mean of the knots locations
      xi.Sigma0 <- priorArgs$xi.Sigma0 # Covariance matrix.
      xi.c <- priorArgs$xi.c
      
      prior_type <- priorArgs$prior_type[["xi"]]
      
      ## The exact gradient should be here
      ## Reconstruct the data structures given args$KnotsSubset.
      
      X <- d.matrix(x,xi2,splineArgs) # Design matrix that knots have been
                                     # reordered. Denoted as a capital letter. n-by-q.
      
      q <- dim(X)[2] # column dim of the design matrix. q = 1+m+k (withInt); q  =  m+k
                     # (withInt = FALSE) 
      P <- crossprod(X)  # P is X'X. But you may change them later

      P.inv <- try(solve(P), silent = TRUE) # inverse of X'X
      if(is(P.inv, "try-error")) # If P is singular, the knots went to far from the
                                 # boundary. No need to continue. 
        {
          gradObs <- NaN
          hessObs <- NaN
        }
      else # all fine, continue to compute the gradien and Hessian.
        {
          Sigma.inv <- solve(Sigma)
          C <- K^(1/2) %d*d% Sigma
          K.inv2 <- solve(sqrt(K))
          C.inv <- K.inv2%d*d%Sigma.inv
          
          
          Sigma.tilde <- solve(Sigma.inv+ C.inv)
          B.tilde <- (P.inv%*%crossprod(X, Y) %*% Sigma.inv + M%*%C.inv)%*%Sigma.tilde
          B.hat <- P.inv%*%crossprod(X, Y)
          # I.tilde <- X%*%tcrossprod(P.inv, X)
          E.tilde <- Y-X%*%B.tilde # 2 The residual
          S.tilde <- crossprod(E.tilde)/n  # Resd' * Resd
          D <- B.tilde-M # 3
          E.hat <- Y - X%*%B.hat
          
          ## I.tilde - diag(n) # FAST
          ## I.tilde_diagN <- I.tilde
          ## diag.idx <- (1:n)^2
          ## I.tilde_diagN[diag.idx]<-I.tilde_diagN[diag.idx]-1
          
          grad.tmp.part0 <- X %*% D %*% C.inv %*% (t(D)-Sigma.tilde %*% tcrossprod(Sigma.inv, B.hat)) +
                            E.hat %*% Sigma.inv %*% Sigma.tilde %*%
                              (C.inv%*%t(D)-Sigma.inv %*% t(E.tilde) %*% X %*% P.inv)+
                            X %*% (B.hat - B.tilde) %*% Sigma.inv %*% Sigma.tilde %*% tcrossprod(Sigma.inv, B.hat)-
                            E.tilde %*% tcrossprod(Sigma.inv, B.tilde)

          grad.tmp.part <- -matrix(grad.tmp.part0) # q*n-by-1

          ## Calculate gradient for design matrix with respect to the knots (subset)
          grad.tmp.xi <- t(delta.xi(x,xi,list(SplineType = splineArgs$method,
                                                   KnotsSubset = KnotsSubset))) # m*k0-by-n
                                        # The gradient w.r.t. xi FIXME. array? or matrix
      
          ## Using the chain rule to obtain gradient for  marginal likelihood with respect to
          ## knots (subset). We have to partition the matrix and obtain the gradient
          ## w.r.t. given knots (subset)
          ## According to the rule of partition matrix multiplication,  we only need to some
          ## parts. 
          ## The details can be find in the note.
          grad.interval <- (n*(q-k)+1):(n*(q-k+k0)) # We only need the gradient at this
                                        # interval.
          
          grad.tmp.part.nonzero <-  grad.tmp.part[grad.interval,]   # n*k0-by-1 , dim has been
                                        # dropped...
          
          grad.tmp.part.nonzero2 <- matrix(grad.tmp.part.nonzero,k0,n,byrow = TRUE) # k0-by-n
          grad.tmp.part.nonzero3 <- grad.tmp.part.nonzero2[rep(1:k0, each = m), ]

          gradObs.margi <- matrix(rowSums(grad.tmp.xi*grad.tmp.part.nonzero3))
          ## FIXME: Transformation
          
          ## The whole gradient and hessian for the prior part.
          xi.orig.vec <- matrix(t(xi.orig),k*m,1) # vec,  but by row.
          
          xi.mu0.vec <- matrix(t(xi.mu0),k*m,1) # vec,  but by row
          
          gradHessObsPri <- deriv_prior(xi.orig.vec, priorArgs = list(mean =
                                                       xi.mu0.vec, covariance = xi.Sigma0,
                                                       shrinkage = xi.c, prior_type =
                                                       prior_type))  
          
          ## Pick gradient and hessian part for the knots (subset)
          gradObs.pri.part <- matrix(t(matrix(gradHessObsPri$gradObsPri, k, m, byrow =
                                              TRUE)[KnotsSubset, ]), m*k0, 1)  # be careful when
                                        # only one knot left. the dimesion may drop...
          
          ## This is a trick
          ## We first need to find the location in the hessian for those knots(subset)
          KnotsSubset.idx <- t(matrix((KnotsSubset-1)*m, k0, m)
                               + matrix(1:m, k0, m, byrow = TRUE)) # m-by-k0 
          ## Then pick then from the hessian matrix.
          hess.pri.idx <- as.numeric(matrix(KnotsSubset.idx, k0*m, k0*m) +
                                     (matrix(KnotsSubset.idx, k0*m, k0*m, byrow = TRUE)
                                      -1)*(k*m)) 
          
          
          hessObs.pri <- matrix(gradHessObsPri$hessObsPri[hess.pri.idx],  k0*m,  k0*m)

          if(hessMethod == "exact") # Use the exact hessian
            {
              hessObs <- "Write the exact hessian here"
            }
          else # call the approximation of hessian
            {
              hessObs.margi <- hessian_approx(gradient = gradObs.margi, method = hessMethod)     
            }

          ## The final gradient and hessian
          gradObs = gradObs.margi+gradObs.pri.part
          hessObs = hessObs.margi+hessObs.pri

          ##grad.num.xi <- numgrad(myfun1, x = t(xi))
          
          ##print((grad.num.xi$g/gradObs-1))
        } # else # all fine, continue to compute the gradien and Hessian.

      return(list(gradObs = gradObs, hessObs = hessObs))
      
    } # if(tolower(callParam$id) == "xi")
  else if(callParam$id == "diag.K") ## gradient for shrinkage K.
    {
      x.dim <- dim(x) 
      m <- x.dim[2] # no. of covariates(dimension).
      n <- x.dim[1] # no. of obs.

      p <- dim(Y)[2]
     
      K <- diag(as.vector(diag.K), length(diag.K))
      K.inv <- try(solve(K), silent=TRUE)
      K.inv2 <- try(solve(sqrt(K)), silent=TRUE)
      if(is(K.inv2, "try-error") ||is(K.inv, "try-error")) # Shrinkage is almost zero.
        {
          gradObs <- NaN
          hessObs <- NaN
        }
      else
        {
      
          Sigma <- vech2m(vech.Sigma)
          
          M <- priorArgs$M # mean of B,  q-by-p
          S0 <- priorArgs$S0
          
          X <- d.matrix(x,xi,splineArgs) # Design matrix that knots have been
                                        # reordered. Denoted as a capital letter. n-by-q.
          
          q <- dim(X)[2] # column dim of the design matrix. q = 1+m+k (withInt); q  =  m+k
                                        # (withInt = FALSE) 
          P <- crossprod(X)  # P is X'X. But you may change them later
          P.inv <- solve(P) # inverse of X'X
          Sigma.inv <- solve(Sigma)
          C <- K^(1/2) %d*d% Sigma
          C.inv <- K.inv2%d*d%Sigma.inv
          
          Sigma.tilde <- solve(Sigma.inv+ C.inv)
          B.tilde <- (P.inv%*%crossprod(X, Y) %*% Sigma.inv + M%*%C.inv)%*%Sigma.tilde
          B.hat <- P.inv%*%crossprod(X, Y)
          ## I.tilde <- X%*%tcrossprod(P.inv, X)
          E.tilde <- Y-X%*%B.tilde # 2 The residual
          S.tilde <- crossprod(E.tilde)/n  # Resd' * Resd
          D <- B.tilde - M # 3
     
          gradObs.part0 <- Sigma.inv%*d%K.inv2%*%t(D)%*%(t(X)%*%E.tilde%*%Sigma.inv-P%*%D%*%C.inv)%*%Sigma.tilde+
            Sigma.inv%*d%K.inv2%*%Sigma.tilde%*%(diag(p)+Sigma.inv%*%t(E.tilde)%*%X%*%D-C.inv%*%t(D)%*%P%*%D)+
              t(D)%*%P%*%D%*%K.inv2%*%Sigma.inv # p-by-p
          
          gradObs.part <- matrix(gradObs.part0, 1, p^2)%*%delta.K(K, power = -1/2) # 1-by-p*p
          
          gradObs.margi0 <- -1/2*matrix(solve(K), 1, p^2)%*%delta.K(K, power = 1) - gradObs.part #  
          
          ## Add the transformation with the links

          gradObsTras <- deriv_link(diag.K.orig, link = Params_Transform[["diag.K"]])
          
          
          gradObs.margi <- t(gradObs.margi0 %*% gradObsTras) # p-by-1
          
          ## gradient for the prior
          gradHessObsPri <- deriv_prior(diag.K.orig, priorArgs = list(mean =
                                                  priorArgs$diag.K.mu0, covariance =
                                                  priorArgs$diag.K.Sigma0, shrinkage = priorArgs$diag.K.c
                                                  , prior_type =
                                                  priorArgs$prior_type[["diag.K"]]))
          gradObs.pri <- gradHessObsPri$gradObsPri
          hessObs.pri <- gradHessObsPri$hessObsPri
          
          if(hessMethod == "exact") # Use the exact hessian
            {
              hessObs <- "Write the exact hessian here"
            }
          else # call the approximation of hessian
            {
              hessObs.margi <- hessian_approx(gradient = gradObs.margi, method = hessMethod)     
            }
          
          gradObs = gradObs.margi+gradObs.pri
          hessObs = hessObs.margi+hessObs.pri

          # browser()

          ## gradObs = gradObs.margi
          ## hessObs = hessObs.margi
          
          ## cat(gradObs.margi, gradObs.pri, "\n")
          
          ## xx = x
          ## grad.num.xi <- numgrad(myfun2, x = diag.K.orig) ## ??? 
          ## cat("grad.num.K:",  grad.num.K$g, "gradObs", gradObs, "\n")

        }
      return(list(gradObs = gradObs, hessObs = hessObs))  # 
      
    } # else if(tolower(callParam$id) == "k")
  else if(callParam$id == "vech.Sigma") ## gradient for shrinkage K.
    {
      x.dim <- dim(x) 
      m <- x.dim[2] # no. of covariates(dimension).
      n <- x.dim[1] # no. of obs.

      p <- dim(Y)[2]

      K <- diag(as.vector(diag.K))
      Sigma <- vech2m(vech.Sigma)
      
      M <- priorArgs$M # mean of B,  q-by-p
      S0 <- priorArgs$S0
      
      X <- d.matrix(x,xi,splineArgs) # Design matrix that knots have been
                                        # reordered. Denoted as a capital letter. n-by-q.
      
      q <- dim(X)[2] # column dim of the design matrix. q = 1+m+k (withInt); q  =  m+k
                                        # (withInt = FALSE) 
      P <- crossprod(X)  # P is X'X. But you may change them later
 
      P.inv <- solve(P) # inverse of X'X

      Sigma.inv <- solve(Sigma)
      C <- K^(1/2) %d*d% Sigma
      K.inv2 <- solve(sqrt(K))
      C.inv <- K.inv2%d*d%Sigma.inv
      
      Sigma.tilde <- solve(Sigma.inv+ C.inv)
      B.tilde <- (P.inv%*%crossprod(X, Y) %*% Sigma.inv + M%*%C.inv)%*%Sigma.tilde
      B.hat <- P.inv%*%crossprod(X, Y)
      I.tilde <- X%*%tcrossprod(P.inv, X) ## FIXEME too slow
      E.tilde <- Y-X%*%B.tilde # 2 The residual
      S.tilde <- crossprod(E.tilde)/n  # Resd' * Resd
      D <- B.tilde-M 

      gradObs.margi0 <-  2*Sigma.inv%*d%K.inv2%*%t(D)%*%(
                        P%*%D%*d%solve(K)-t(X)%*%E.tilde%*%Sigma.inv)%*%Sigma.tilde%*d%K.inv2%*%Sigma.inv -
                          Sigma.inv%*%Sigma.tilde%*%Sigma.inv
                          + (n0+n+p+q+1)*Sigma.inv+2*Sigma.inv%*%t(B.tilde-B.hat)%*%
                          (P%*%D%*d%solve(K)-t(X)%*%E.tilde%*%Sigma.inv)%*%Sigma.tilde%*%Sigma.inv-
                          Sigma.inv%*d% K.inv2%*%Sigma.tilde%*d%K.inv2%*%Sigma.inv -
                            Sigma.inv%*%(n0*S0+n*S.tilde+K.inv2%d*%t(D)%*%P%*%D%*d%K.inv2)%*%Sigma.inv

      gradObs.margi <- -1/2*vech(gradObs.margi0) # Sigma is symmetric. Only need helf vectorization.
     
      ## gradient for the prior
      ## The prior has be pre-specified
      gradObs.pri <- 0
      hessObs.pri <- 0

      if(hessMethod == "exact") # Use the exact hessian
        {
          hessObs <- "Write the exact hessian here"
        }
      else # call the approximation of hessian
        {
          hessObs.margi <- hessian_approx(gradient = gradObs.margi, method = hessMethod)     
        }

      gradObs = gradObs.margi+gradObs.pri
      hessObs = hessObs.margi+hessObs.pri
      ## browser()
      return(list(gradObs = gradObs, hessObs = hessObs))  # 
            
    } # else if(tolower(callParam$id) == "sigma")
  else
    {
      stop("Wrong argument for callparams!")
    }
}

