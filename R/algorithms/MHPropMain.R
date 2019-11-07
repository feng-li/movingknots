#' The Main MCMC algorithm for movingknots.
#'
#' Details are available in the paper.
#' @param param.cur NA
#' @param gradhess.fun.name NA
#' @param logpost.fun.name NA
#' @param nNewtonStep NA
#' @param Params NA
#' @param hessMethod NA
#' @param Y.iCross NA
#' @param x.iCross NA
#' @param callParam NA
#' @param splineArgs NA
#' @param priorArgs NA
#' @param algArgs NA
#' @param Params_Transform NA
#' @param propMethod NA
#' @return NA
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
MHPropMain <- function(param.cur, gradhess.fun.name, logpost.fun.name, nNewtonStep,
                       Params, hessMethod, Y.iCross, x.iCross, callParam, splineArgs,
                       priorArgs, algArgs, Params_Transform, propMethod)
{
    ## This is the mail MH file,  It will call individual functions for each proposal
                                        # print("MH-Main \n")
                                        # browser("MH-Main")
    require("MASS")
    require("Matrix")
    if (tolower(propMethod) == "inverse-wishart")
    {
        ## The Metropolis-Hastings with inverse Wishart proposal
        out <- MHPropWithIWishart(param.cur = param.cur,
                                  logpost.fun.name = logpost.fun.name,
                                  Params = Params,
                                  Y = Y.iCross,
                                  x0 = x.iCross,
                                  callParam = callParam,
                                  splineArgs = splineArgs,
                                  priorArgs = priorArgs,
                                  Params_Transform = Params_Transform)
    }
    else if((tolower(propMethod) == "kstepnewton"))
    {
        ## The Metropolis-Hastings with k-step Newton proposal
        out <- MHPropWithKStepNewton(param.cur = param.cur,
                                     gradhess.fun.name = gradhess.fun.name,
                                     logpost.fun.name = logpost.fun.name,
                                     nNewtonStep = nNewtonStep,
                                     Params = Params,
                                     hessMethod = hessMethod,
                                     Y = Y.iCross,
                                     x0 = x.iCross,
                                     callParam = callParam,
                                     splineArgs = splineArgs,
                                     priorArgs = priorArgs,
                                     # algArgs = algArgs,
                                     Params_Transform = Params_Transform)
    }
    else if ((tolower(propMethod) == "random-walk"))
    {
        ## The Metropolis with random walk proposal
        out <- RandomWalkMetropolis(param.cur = param.cur,
                                    logpost.fun.name = logpost.fun.name,
                                    Params = Params,
                                    Y = Y.iCross,
                                    x0 = x.iCross,
                                    callParam = callParam,
                                    splineArgs = splineArgs,
                                    priorArgs = priorArgs,
                                    Params_Transform = Params_Transform)
    }
    else if ((tolower(propMethod) == "sgld"))
    {
        out <- SGLD(param.cur = param.cur,
                    logpost.fun.name = logpost.fun.name,
                    Params = Params,
                    Y = Y.iCross,
                    x0 = x.iCross,
                    callParam = callParam,
                    splineArgs = splineArgs,
                    priorArgs = priorArgs,
                    algArgs=algArgs,
                    Params_Transform = Params_Transform)
    }
    else
    {
        stop("Wrong argument for propDensity!")
    }

    return(out)
}
