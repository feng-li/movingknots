##' The Main file for Metropolis–Hastings algorithm
##'
##' Details.
##' @name 
##' @title
##' @param param.cur 
##' @param gradhess.fun.name 
##' @param logpost.fun.name 
##' @param nNewtonStep 
##' @param Params 
##' @param hessMethod 
##' @param Y.iCross 
##' @param x.iCross 
##' @param callParam 
##' @param splineArgs 
##' @param priorArgs 
##' @param prop.df 
##' @param Params_Transform 
##' @param propMethod 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
MHPropMain <- function(param.cur, gradhess.fun.name, logpost.fun.name, nNewtonStep,
                       Params, hessMethod, Y.iCross, x.iCross, callParam, splineArgs,
                       priorArgs, prop.df, Params_Transform, propMethod)
{
  ## This is the mail MH file,  It will call individual functions for each proposal
 # print("MH-Main \n")
 # browser("MH-Main")
  
  if (tolower(propMethod) == "inverse-wishart")
    {
      out <- MHPropWithIWishart(param.cur = param.cur, logpost.fun.name =
                                logpost.fun.name, Params = Params, Y = Y.iCross, x =
                                x.iCross, callParam = callParam,
                                splineArgs = splineArgs, priorArgs = priorArgs,
                                Params_Transform = Params_Transform) 
    }
  else if((tolower(propMethod) == "kstepnewton"))
    {
      out <- MHPropWithKStepNewton(param.cur = param.cur, gradhess.fun.name =
                                   gradhess.fun.name, logpost.fun.name =
                                   logpost.fun.name, nNewtonStep =
                                   nNewtonStep, Params = Params, hessMethod =
                                   hessMethod, Y = Y.iCross, x = x.iCross,
                                   callParam = callParam,
                                   splineArgs = splineArgs, priorArgs = priorArgs,
                                   prop.df = prop.df, Params_Transform =
                                   Params_Transform)
    }
  else
    {
      stop("Wrong argument for propDensity!")
    }
  
  return(out)
}
