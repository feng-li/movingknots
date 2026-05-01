## --------------------Introduction and Help----------------------------------------------
## KStepNewtonDimMove:
##	Iterates the generalized (dimension-changing) Newton algorithm.
##      This orginal matlab version was written by Villani, M.
##
## Usage:
##	KStepNewtonDimMove(XOld,K,GradHessFunc,Index1,Index2,x,...)
##
## Arguments:
##      XOld           p x 1      Starting point of the Newton algorithm.
##      K              Scalar     The number of iterations of the Newton algorithm. K = Inf is allowed.
##      GradHessFunc   String     The name of the function that computes the gradient and Hessian
##                                of the target density.
##      Index1         p x 1      Logical vector with variable selection indicator for the current draw.
##      Index2         p x 1      Logical vector with variable selection indicator for the proposed draw.
##      ...            Unkown     the parameters after ... must be exactly matched.
##
## Values:
##      XNew           p x 1      The terminal vector of the Newton algorithm.
##      Hess           p x p      Hessian of the target density at XNew
##      Grad           Scalar     Gradient of the target density at XNew
##      ErrorFlag      0-1        If = 0, all is OK. if = 1, Hessian is ill-behaved (NaNs)
##
##
## References:
##     Villani, M., Kohn, R. and Giordani, P. (2008)
##     Wegmann, B. and Villani, M. (2008)
##
## Version:
##     First:     2009-01-15
##     Current:   2010-01-30
##
## --------------------The code-----------------------------------------------------------

KStepNewtonDimMove <- function (XOld,K,GradHessFunc,Index1,Index2,x,...)
  {

   if (all(Index1==0))
     {
       Index1 <- Index2
       XOld <- as.matrix(rep(0,length(Index2))) ## length(Index2) x 1
       K <- 2*K
     }
   ErrorFlag <- 0
   XNew <- XOld
   ## Infinite number of Newton steps: iterate until tolerance is met

   if (is.infinite(K)==TRUE)
     {
       ## Iterate until convergence.
       Crit <- 1
       Tol <- 1e-7
       Count <- 0
       while (Crit>Tol)
         {
         #  Count <- Count+1
           if(all(Index1==Index2)==FALSE) # Dimension changeing move
             {

               GradHess <- eval(call(GradHessFunc,Parames=XNew,Ic=Index1,Ip=Index2,x=x))

               Gradient <- GradHess$Grad
               Hessian22 <- GradHess$Hessp
               Hessian21 <- GradHess$Hessc
               XNewTemp <- solve(Hessian22,tol=1e-1000)%*%Hessian21%*%XOld[Index1!=0]-solve(Hessian22,tol=1e-1000)%*%Gradient
               XNew <- as.matrix(rep(0,length(Index2))) ## length(Index2) x 1
               XNew[Index2!=0] <- XNewTemp
               Index1 <- Index2
             }
           else # Dimesion preserving move
             {
               GradHess <- eval(call(GradHessFunc, Parames=XNew, Ic=Index1, Ip=Index2,x=x))
               Gradient <- GradHess$Grad
               Hess <- GradHess$Hessp
               XNewTemp <- XOld[Index2!=0] -solve(Hess,tol=1e-1000) %*% Gradient
               XNew <- as.matrix(rep(0,length(Index2)))
               XNew[Index2!=0] <- XNewTemp
               Crit <- sqrt(sum((XNew-XOld)^2)) ## Matrix norm
             }
           XOld <- XNew
         }
     }

   ## Finite pre-determined number of Newton steps
   else
     {
       for (k in 1:K)
         {
           if(all(Index1==Index2)==FALSE) ##
             {
               GradHess <- eval(call(GradHessFunc,Parames=XNew, Ic=Index1, Ip=Index2,x=x))
               Gradient <- GradHess$Grad
               Hessian22 <- GradHess$Hessp
               Hessian21 <- GradHess$Hessc
               XNewTemp <-
                 solve(Hessian22,tol=1e-1000)%*%Hessian21%*%XOld[Index1!=0]-solve(Hessian22,tol=1e-1000)%*%Gradient 
               XNew <- as.matrix(rep(0,length(Index2))) ## length(Index2) x 1
               XNew[Index2!=0] <- XNewTemp
               Index1 <- Index2
             }
           else # Dim preserving move
             {
               GradHess <- eval(call(GradHessFunc,Parames=XNew,Ic=Index1,Ip=Index2,x=x))
               Gradient <- GradHess$Grad
               Hess <- GradHess$Hessp
               XNewTemp <- XOld[Index2!=0]-solve(Hess,tol=1e-1000) %*% Gradient
               XNew <- as.matrix(rep(0,length(Index2))) # length(Index2) x 1
               XNew[Index2!=0] <- XNewTemp
             }
           XOld <- XNew

       }

   }

   ## Compute the Gradient and Hessian at XNew, the terminal point
   GradHess <- eval(call(GradHessFunc,Parames=XNew,Ic=Index1,Ip=Index2,x=x))
   Gradient <- GradHess$Grad
   Hess <- GradHess$Hessp
   XNew <- XNew[Index2!=0]

   if(any(is.nan(Hess)==TRUE))
     {
       ErrorFlag <- 1
     }

  #  print(XNew)
   list(Gradient=Gradient,Hess=Hess,XNew=XNew,ErrorFlag=ErrorFlag)
  }

 ## -------------------End Here-----------------------------------------------------------
