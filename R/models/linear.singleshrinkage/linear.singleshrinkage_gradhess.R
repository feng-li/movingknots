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
##'         priorArgs$M: mean of B,  q-by-q
##'         priorArgs$n0: df. of inverse wishart distribution. 
##'         priorArgs$S0: Covariance matrix from the prior of B.
##'         priorArgs$ka0: The shrinkage for knots variance
##'         priorArgs$mu0: Mean of the knots locations from the prior
##'         priorArgs$Sigma0: Covariance matrix from the prior of knots
##'         priorArgs$prior_type: gradient type for prior
##'         priorArgs$ka.mu0: mean for ka
##'         priorArgs$ka.Sigma0: variance for ka
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
linear.singleshrinkage_gradhess <- function(Params, hessMethod, Y, x, callParam,
                                            splineArgs, priorArgs,  Params_Transform) 
{
  ## Let me explain what I want to do here.
  ## 1) The *gradient* fot some knot condition on other knots
  ## 2) The *hessian matrix*
  ## I should be careful about the order and subsets where containning knots,  xi.

  ## This is the marginal likelihood of xi which does not depend on B and Sigma



  xi.orig <- Params[["xi"]]
  ka.orig <- Params[["ka"]]

  xi <- par.transform(par = xi.orig, method = Params_Transform[["xi"]])
  ka <- par.transform(par = ka.orig, method = Params_Transform[["ka"]])
  
  
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
      
      
      ## Extract argument from the conjugate prior and the prior for knots.
      ## Does not depends on knots, don't have to reorder them
      ## TODO: write a function to extra everything from a list with its name?
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


          ## The New API. see my notes
          ## B.tilde <- 1/(1+1/ka)*P.inv%*%(crossprod(X, Y)+1/ka*P%*%M)
          ## E.tilde <- Y - X%*%B.tilde
          ## S.tilde <- crossprod(E.tilde)/n 
          ## D <- B.tilde-M
          ## Omega <- n0*S0 + n*S.tilde + 1/ka * t(D) %*% P %*% D
          ## Omega.inv <- solve(Omega)

          ## Q1 <- 2*X %*% P.inv %*% t(X) %*% E.tilde  %*% Omega.inv %*% t(M-(1/ka+1)*B.tilde) +
          ##       2*(1/ka*E.tilde - X %*% D) %*% Omega.inv %*% t(E.tilde) %*% X %*% P.inv -
          ##       (Y - X %*% M) %*% Omega.inv %*% t(D) + X %*% D %*% Omega.inv %*% t(M)
          ## grad.tmp1 <- -1/(1/ka +1)* matrix(Q1)

          ## Q2 <- (X %*% P.inv %*% t(X) %*% Y - X %*% M) %*% Omega.inv %*%
          ##       t(ka*M - (ka +1)*B.tilde) +
          ##       (E.tilde - ka*X %*% D) %*% Omega.inv %*% t(P.inv %*% t(X) %*% Y - M)
          ## grad.tmp2 <- (1/ka)/(1/ka + 1)^2 * matrix(Q2)

          ## grad.tmp3 <- 2 * matrix(E.tilde %*% Omega.inv %*% t(B.tilde))

          ## grad.tmp.part <- grad.tmp1 + grad.tmp2 + grad.tmp3
          
          ##

          ## The old interface, (not so) slow and works
          XP.inv<- X%*%P.inv # 6
          B.tilde <- 1/(1+1/ka)*P.inv%*%(crossprod(X, Y)+1/ka*P%*%M)
          E.tilde <- Y-X%*%B.tilde # 2 The residual
          S.tilde <- crossprod(E.tilde)/n  # Resd' * Resd
          
          B.tilde_M <- B.tilde-M # 3
      
          S.tilde_S0 <- n0*S0+n*S.tilde+1/ka*t(B.tilde_M)%*%P%*%B.tilde_M
          
          invS.tilde_S0 <- solve(S.tilde_S0)
          
          ## symmetric matrix if P  = X'X. Be careful when P  ! = X'X 
          ## TODO: Can this be faster?
          
          grad.tmp2 <- matrix(invS.tilde_S0,ncol=1) # vec

          Q_MB <- t(1/ka*M-(1/ka+1)*B.tilde) # 1
          Q_YXMXB <- t(Y+1/ka*X%*%M-(1/ka+1)*X%*%B.tilde) # 4
          Q_XYPM <- t(crossprod(X, Y)-P%*%M) # 5

          Q.tmp1.0 <- crossprod(E.tilde, XP.inv)
          Q.tmp1.1 <- tcrossprod(Q.tmp1.0, X) 
          Q.tmp1.2 <-  Q_MB %x% Q.tmp1.1
          Q.tmp1 <- -ka/(ka+1)*(Q.tmp1.2 + K.X(p, p, Q.tmp1.2, t = FALSE))

          Q.tmp2.0 <- Q_YXMXB%x%Q.tmp1.0
          Q.tmp2 <- -ka/(ka+1)*(K.X(n,q,(Q.tmp2.0+K.X(p,p,Q.tmp2.0,t = FALSE)),t  = TRUE))
                                        # TODO: Can this be faster?  

          Q.tmp3.0 <- (t(Y)-t(X%*%M))%x%t(B.tilde_M)
          Q.tmp3.1 <- t(M)%x%(t(X%*%B.tilde_M))
          Q.tmp3 <- 1/(ka+1)*(K.X(n,q,Q.tmp3.0,t = TRUE)-Q.tmp3.1)

          Q.tmp4.0 <- Q_MB%x%(tcrossprod(Q_XYPM, XP.inv))
          Q.tmp4 <- ka/(ka+1)^2*K.X(p,p,Q.tmp4.0,t = FALSE)

          Q.tmp5.0 <- (Q_XYPM%*%P.inv)%x%Q_YXMXB
          Q.tmp5 <- ka/(ka+1)^2*Q.tmp5.0

          Q.tmp6.0 <- t(B.tilde) %x% t(E.tilde)
          Q.tmp6 <- -Q.tmp6.0-K.X(p,p,Q.tmp6.0,t=FALSE)

          
          grad.tmp.Qx <- Q.tmp1 +Q.tmp2+Q.tmp3+Q.tmp4+Q.tmp5+Q.tmp6
          
          ## The gradient for the marginal likelihood w.r.t. the design matrix.
          grad.tmp.part <- - (n+n0)/2*crossprod(grad.tmp.Qx,  grad.tmp2)
                                        # n*q-by-1
         

          ## ## FIXME: should xi.orig with new order????
          ## ## Calculate gradient for design matrix with respect to the knots (subset)
          grad.tmp.xi <- t(delta.xi(x,xi,list(SplineType = splineArgs$method,
                                                   KnotsSubset = KnotsSubset))) # m*k0-by-n
                                      ##  The gradient w.r.t. xi FIXME. array? or matrix
      
          ## Using the chain rule to obtain gradient for  marginal likelihood with respect to
          ## knots (subset). We have to partition the matrix and obtain the gradient
          ## w.r.t. given knots (subset)
          ## According to the rule of partition matrix multiplication,  we only need to some
          ## parts. 
          ## The details can be find in the note.
          grad.interval <- (n*(q-k)+1):(n*(q-k+k0)) # We only need the gradient at this
                                        # interval.
          
          grad.tmp.part.nonzero <-  grad.tmp.part[grad.interval,]   # n*k0-by-1 , dim has
                                        # been dropped... 
          grad.tmp.part.nonzero2 <- matrix(grad.tmp.part.nonzero,k0,n,byrow = TRUE) # k0-by-n
          grad.tmp.part.nonzero3 <- grad.tmp.part.nonzero2[rep(1:k0, each = m), ]

          gradObs.margi <- matrix(rowSums(grad.tmp.xi*grad.tmp.part.nonzero3))
          ##FIXME: Transformation 

          ## The whole gradient and hessian for the prior part.
          xi.orig.vec <- matrix(t(xi.orig),k*m,1) # vec,  but by row.
          
          xi.mu0.vec <- matrix(t(xi.mu0),k*m,1) # vec,  but by row
 
          gradHessObsPri <- deriv_prior(xi.orig.vec, priorArgs = list(mean =
                                                       xi.mu0.vec, covariance = xi.Sigma0,
                                                       shrinkage = xi.c, prior_type =
                                                       prior_type))  
          
          ## Pick gradient and hessian part for the knots (subset)
          gradObs.pri.part <- matrix(t(matrix(gradHessObsPri$gradObsPri, k, m, byrow =
                                              TRUE)[KnotsSubset, ]), m*k0, 1)  # be
                                        # careful when only one knot left. the dimesion
                                        # may drop... 
          
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
          ##  browser()
          ##print(gradObs)
          
        } # else # all fine, continue to compute the gradien and Hessian.

      return(list(gradObs = gradObs, hessObs = hessObs))
      
    } # if(tolower(callParam$id) == "xi")
  else if(callParam$id == "ka") ## gradient for shrinkage ka.
    {

      x.dim <- dim(x) 
      m <- x.dim[2] # no. of covariates(dimension).
      n <- x.dim[1] # no. of obs.

      p <- dim(Y)[2]
      
      M <- priorArgs$M # mean of B,  q-by-p
      S0 <- priorArgs$S0


      X <- d.matrix(x,xi,splineArgs) # Design matrix that knots have been
                                        # reordered. Denoted as a capital letter. n-by-q.
      
      q <- dim(X)[2] # column dim of the design matrix. q = 1+m+k (withInt); q  =  m+k
                                        # (withInt = FALSE) 
      P <- crossprod(X)  # P is X'X. But you may change them later
      

      P.inv <- solve(P) # inverse of X'X
      
      XP.inv<- X%*%P.inv # 6
      B.tilde <- 1/(1+1/ka)*P.inv%*%(crossprod(X, Y)+1/ka*P%*%M)
      E.tilde <- Y-X%*%B.tilde # 2 The residual
      S.tilde <- crossprod(E.tilde)/n  # Resd' * Resd
      B.tilde_M <- B.tilde-M # 3
      B.hat <- crossprod(XP.inv, Y)
      S.tilde_S0 <- n0*S0+n*S.tilde+1/ka*t(B.tilde_M)%*%P%*%B.tilde_M
      invS.tilde_S0 <- solve(S.tilde_S0)

      #grad.r.tmp <- -(p*q/2)/ka

      #grad.logdetP.tmp <- (p*q/2)/(ka+ka^2) 

      grad.r.tmp <- - p*q/(2*(1+ka))
      
      tXY <- crossprod(X, Y)
      tMPM <- crossprod(M, P)%*%M
      
      Q_ka1 <- (1-ka)/(1+ka)^3*crossprod((P.inv%*%tXY), (tXY-P%*%M)) -
              2/(1+ka)^3*(crossprod(M, tXY)-tMPM) +
              1/(1+ka)^2*tMPM
      Q_ka2 <- -2/(1+ka)^3*(crossprod(Y, X) %*% (B.hat - M) +
                              t(M) %*% crossprod(X, (X%*%M - Y)))

      ## gradObs.margi0 <- grad.r.tmp+ grad.logdetP.tmp -(n+n0)/2*crossprod(matrix(Q_ka),
      ##                   matrix(invS.tilde_S0))  

      gradObs.margi0 <- grad.r.tmp - (n+n0)/2*crossprod(matrix(Q_ka1 + Q_ka2),
                                                        matrix(invS.tilde_S0))   


      ## Add the transformation with the links
      gradObsTrans <- deriv_link(ka.orig, link = Params_Transform[["ka"]])
      
      gradObs.margi <- t(gradObs.margi0 %*% gradObsTrans) # p-by-1

      ## gradient for the prior
      
      gradHessObsPri <- deriv_prior(ka.orig, priorArgs = list(mean =
                                                   matrix(priorArgs$ka.mu0), covariance =
                                          matrix(priorArgs$ka.Sigma0), shrinkage =
                                          priorArgs$ka.c , prior_type =
                                          priorArgs$prior_type[["ka"]]))
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

      ##################### debug
      ## browser()
      ## grad.num.ka2 <- numgrad(myfun2, x = ka.orig)
      ## cat("num.grad/gradObs:", grad.num.ka$g/gradObs, "\n")
      #cat("margi:", gradObs.margi, "pri:", gradObs.pri, "\n")
      
      return(list(gradObs = gradObs, hessObs = hessObs))  # 
            
    } # else if(tolower(callParam$id) == "ka")
  else
    {
      stop("Wrong argument for callparams!")
    }

}

