
## Bad initial values can make troubles of the hessian matrix at the beginning of the
## iterations 
## Use BFGS to make it robust

## The function to be *minimized* with respect to the first arguement "par"
## par.optim.logpost <- function(par, Y, covariates, logpost.fun.name, Params, callParam, splineArgs,
##                               priorArgs)
## {
##   Params[[callParam$id]][callParam$subset] <- par
##   caller <- call(logpost.fun.name, Y = Y, x = covariates, Params = Params, callParam = callParam,
##                  splineArgs = splineArgs, priorArgs = priorArgs) 
##   neg.logpost.out <- -eval(caller)
##   return(neg.logpost.out)
## }


## for(iIter in 1:10) # loop nIter times
## {
##  for (iPar in Params_Name[1:2]) # only update xi and ka.
##     {
##       for(iSub in Params.sub.loc[[iPar]]) # update subsets
##         {
##           ## The parameters to be updated,  one-column matrix
##           param.cur <- matrix(Params[[iPar]][iSub])
##           iter.tmp <- csminwel(par.optim.logpost, x0 = param.cur, H0 =
##                            -diag(length(param.cur)), Y = Y, covariates = x, Params =
##                            Params, callParam = list(id = iPar, subset = iSub),
##                            splineArgs = splineArgs, priorArgs = priorArgs,
##                            logpost.fun.name = logpost.fun.name, nit = 1)  

##           ## Update the parameters in the parameters list.
##           Params[[iPar]][iSub] <- iter.tmp$x

##         } # for(i in 1:no.subsets.cur)
##     } # for (iPar in Params_Name[1:2])

## }
