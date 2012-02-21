## This is NOT working
## See grad help for reasions
## Fri Mar 26 10:26:57 CET 2010




## xx = x
## myfun1 <- function(x, Params, splineArgs, priorArgs, Params_Transform)
##   {
##     knots <- x
##     Params[["knots"]] = knots
##     linear_logpost(Y = Y, x = xx, Params = Params, callParam = list(id = "knots", subset = 1:length(knots)),
##                    splineArgs = splineArgs, priorArgs = priorArgs, Params_Transform = Params_Transform) 
##   }
## grad.num.xi <- numgrad(myfun1, x = INIT.knots.mat, Params = Params, splineArgs =
##                        splineArgs, priorArgs = priorArgs, Params_Transform =
##                        Params_Transform)

## xx = x
## myfun2 <- function(x, ...)
##   {
##     shrinkages <- x
##     Params[["shrinkages"]] = shrinkages 
##     linear_logpost(Y = Y, x = xx, Params = Params, callParam = list(id = "shrinkages", subset = 1:length(shrinkages)),
##                    splineArgs = splineArgs, priorArgs = priorArgs, Params_Transform = Params_Transform)
##   }
## grad.num.ka <- numgrad(myfun2, x = INIT.shrinkages)












## test.gradient <- function(xi,Y,x,l0,l,n0,S0,B,ka)
##   {
##     X <- d.matrix(x,xi,l0,l)

##     n <- dim(x)[1]
##     p <- dim(Y)[2]
##     q <- dim(X)[2]

##     P <- t(X)%*%X
##     P_1 <- solve(P)
    
##     XP_1<- X%*%P # 6
##     B_tilde <- 1/(1+ka)*P_1%*%(t(X)%*%Y+ka*P%*%M)
##     Q_YXB <- t(Y-X%*%B_tilde) # 2
##     S_tilde <- Q_YXB%*%t(Q_YXB)/n
    
##     B_tilde_M <- B_tilde-M # 3

##     S_tilde_S0 <- n0*S0+n*S_tilde+ka*t(B_tilde_M)%*%P%*%B_tilde_M # 

##     marginal.out <- -p/2*log(det((ka+1)*P))-
##                     (n+n0)/2*log(det(S_tilde_S0))
##     return(marginal.out)
##   }
## test.gradient(xi,Y,x,l0,l,n0,S0,B,ka)
## library("numDeriv")
## tmp.grad2 <- grad(test.gradient,xi=xi,Y=Y,x=x,l0=l0,l=l,n0=n0,S0=S0,B=B,ka=ka)
