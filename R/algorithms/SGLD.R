##' Stochastic MCMC using Stochastic gradient Langevin dynamics
##'
##' <description>
##' @param param.cur NA
##' @param logpost.fun.name MA
##' @param Params NA
##' @param Y NA
##' @param x0 NA
##' @param callParam NA
##' @param splineArgs NA
##' @param priorArgs NA
##' @param Params_Transfor NA
##' @return NA
##' @references Ma, Chen, & Fox 2015 A Complete Recipe for Stochastic Gradient MCMC.
##' @author Feng Li, Central University of Finance and Economics.
##' @export
SGLD = function(param.cur,
                logpost.fun.name,
                Params,
                Y,
                x0,
                callParam,
                splineArgs,
                priorArgs,
                algArgs,
                Params_Transform)
{
    minibatchSize = algArgs[["minibatchSize"]]
    nEpoch= algArgs[["nEpoch"]]
    calMHAccRate = algArgs[["calMHAccRate"]] # if TRUE, SGLD will be the proposal of
                                             # MH. See Welling & Teh (2011), p 3.
    stepsizeSeq = algArgs[["stepsizeSeq"]]

    nPar = length(param.cur)
    nObs = dim(Y)[1]


    nIterations = celing(nObs / batchSize) # no. runs with one epoch.
    nRuns = (nIteration * nEpoch)
    param.out = matrix(NA, nPar, nRuns)
    accept.prob = matrix(NA, 1, nRuns)

    for(iRun in 1:nRuns)
    {
        ## Redo a splitting when finishing one epoch
        if(iIter %in% seq(1, nRuns, by = nIterations))
        {
            subsetIdxLst = split(sample(1:nObs, size = nObs), 1:nIterations)
        }
        whichIteration = iRun %% nIterations
        whichIteration[whichIteration == 0] = nIterations

        subsetIdx = subsetIdxLst[[whichIteration]]
        epsilon = stepsizeSeq[iRun]
        minibatchSizeeNew = length(subsetIdx) # The minibatch size is not exactly same as
                                        # required due to unequal splitting.
        Params[[callParam$id]][callParam$subset] <- param.cur
        caller = call(gradhess.fun.name,
                      Params = Params,
                      hessMethod = hessMethod,
                      Y = Y[subsetIdx,, drop = FALSE],
                      x0 = x0[subsetIdx,, drop = FALSE],
                      callParam = callParam,
                      splineArgs = splineArgs,
                      priorArgs = priorArgs,
                      Params_Transform = Params_Transform)
        gradhess <- eval(caller)

        gradObs.logLik <- gradhess$gradObs.logLik
        gradObs.logPri <- gradhess$gradObs.logPri

        gradObsSub = (gradObs.logLik / minibatchSizeNew * nObs + gradObs.logPri)

        ## Stochastic Gradient Riemannian Langevin Dynamics (SGRLD). Notation followed in Ma 2015
        fisherInfoMatG = diag(1 / param.cur)
        diffusionMatD = diag(param.cur)
        correctionGamma = diag(1, nrow = nPar)
        potentialUgrad = -gradObsSub

        noise = rmvnorm(n = 1, mean = matrix(0, nrow = 1, ncol = nprop),
                        sigma = 2 * epsilon * diffusionMatD)

        param.prop = (param.cur - epsilon * (diffusionMatD %*% potentialUgrad +
                                             correctionGamma) + noise)
        param.out[, iRun] = param.prop
        if(calMHAccRate == TRUE)
        {
            stop("Not yet done!")
        }
        else
        {
            accept.prob[, iRun] = 1
        }
    }


    out <- list(param.out = param.out, accept.prob = accept.prob)
    return(out)

}
