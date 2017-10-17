## This function originally came from Villani(2009)
## Mon Sep 20 12:06:14 CEST 2010

finiteNewtonProp <- function(paramCurr,ICurr,PrIn,OnTrial,nNewtonSteps,IUpdatePr,
                             logPostFunc,gradHessFunc,x,y,propDf,...)
{
    ## Prelims
    ## Psi = inv(invPsi); % TODO remove this when mvnpdf is replaced with more efficient function
    nPara <- length(paramCurr)

    ## Randomly selecting the subset of covariates that we allow to change in the current iteration.
    nOnTrial <- length(OnTrial) # Number of covariates that may be in or out.

    if (PrIn<1)
    {
        IUpdate <- OnTrial[runif(nOnTrial)<IUpdatePr] # These are the covariates that we update at THIS iteration.
    }
    else # No variable selection
    {
        IUpdate <- NULL
    }

    ## Adding artificial zero for the final step where only the parameters are updated, but not the indicators.
    IUpdate <- c(IUpdate, 0)

    ## Looping over the selected covariates, jointly proposing variable indicator and parameter
    for (update in IUpdate)
    {
        ## Proposing the indicator. More general than needed because we may want to use Kohn-Nott adaptive proposals later on.
        IProp <- ICurr # Initializing the indicator proposal.

        if (update>0) # Variable selection step
        {
            ## Here we use a simple Metropolized proposal of indicators: Out -> In or In -> Out.
            ## TODO: Add adaptive scheme.

            IProp[update] <- 1-IProp[update]
            gProp <- 1      # The proposal probability for the proposed model
            gCurr <- 1      # The proposal probability for the current model

            priorIProp <- PrIn*IProp[update] + (1-PrIn)*(1-IProp[update]) # Prior probability of the proposed model
            priorICurr <- PrIn*ICurr[update] + (1-PrIn)*(1-ICurr[update]) # Prior probability of the current model
        }
        else
        {
            ## Extra step. Only update parameters.
            priorIProp <- 1
            priorICurr <- 1
            gProp <- 1
            gCurr <- 1
        }

        ## Propose the parameters conditional on indicators using the finite step Newton method

        paramProp <- rep(0, length(paramCurr)) # Initialization, later we fill this up.
        errFlagProp <- 0
        errFlagRev <- 0

    if (all(IProp==ICurr) && update>0)
    {
        ## Proposed to stay - do not update parameters.
        ## This cannot happen with the current proposals, but more generally it can.
    }
    else # Proposed a move - do update parameters.
    {
        if (any(IProp!=0)) # At least one covariate included in the proposal draw. Do Newton iterations and propose.
        {
            ## Finite-step Newton: paramCurr -> (propMean,propHess),to get the mean and covariance matrix of the proposal.

            KStepNewtonDimMove <- KStepNewtonDimMove(XOld=paramCurr,K=nNewtonSteps,
                                                     GradHessFunc=gradHessFunc,Index1=ICurr,
                                                     Index2=IProp,x=x)


            propMean <- KStepNewtonDimMove$XNew
            propHess <- KStepNewtonDimMove$Hess
            ## logLikProp
            ## Generating the proposal draw from t(propMean,-InvHess,dfDelta),
            ## where -InvHess is the covariance matrix, NOT the scale matrix.
            ## We need to check if -InvHess is positive definite
            
            if (all(eigen(-propHess)$values>0))  # This is only true for symmetric matrix
            {
              paramProp[IProp!=0] <- RndMultiT(propMean,-solve(propHess,tol=1e-1000),propDf)
              errFlagProp <- FALSE
            }
            else # Not positive definite
            {
              paramProp[IProp!=0] <- NaN
              errFlagProp <- TRUE
            }
          }
        else # No covariate in, no parameters to propose.
         {
           propMean = NULL
           propHess = NULL
           ## ?  [whatever1, whatever2, logLikProp] = feval(logFCP,paramProp,y,V,linkType,condDens,feat,1,1);
         }

        if (errFlagProp == FALSE) # Nothing wrong
          {
            ## Evaluating the proposal density in the proposal point
            LogPropProp <- DensMultiT(paramProp[IProp!=0], propMean, -(propHess),propDf)

            ## Computing the proposal density in the current point. Reversing the Newton.

            ## Reversing the Newton: paramProp -> paramRev
            if (any(ICurr!=0))
              {

                  KStepNewtonDimMove <- KStepNewtonDimMove(XOld=paramProp, K=nNewtonSteps,
                                                           GradHessFunc=gradHessFunc,Index1=IProp,
                                                           Index2=ICurr,x=x)
                  revMean <- KStepNewtonDimMove$XNew
                  revHess <- KStepNewtonDimMove$Hess

                 # LogPropCurr <- DensMultiT(paramCurr[ICurr!=0], revMean, -(revHess),propDf)
                  if(all(eigen(-revHess)$values>0))
                  {
                      LogPropCurr <- DensMultiT(paramCurr[ICurr!=0], revMean, -(revHess),propDf)
                      errFlagRev <- FALSE
                  }
                  else
                  {
                      LogPropCurr <- NULL
                      errFlagRev <- TRUE
                  }

              }
            else
              {
                LogPropCurr <- 0
              }


            if (errFlagRev==FALSE)
              {
                ## Computing the target density in the proposed parameter point

                 logPostProp <- eval(call(logPostFunc,x=x,y=y, ParamesPost=paramProp))

                 ## Computing the target density in the current parameter point
                 logPostCurr <- eval(call(logPostFunc,x=x,y=y, ParamesPost=paramCurr))

                 ## [whatever1, whatever2, logLikCurr] = feval(logFCP,paramCurr,y,V,linkType,condDens,feat,1,1);

                 AccPrRaw <- exp(logPostProp-logPostCurr+LogPropCurr-LogPropProp)*(priorIProp/priorICurr)*(gCurr/gProp)

                 if (is.nan(AccPrRaw)==TRUE)
                   {
                     AccPrRaw <- 0
                   }

                 AccPr <- min(1, AccPrRaw)
               }
            else # Bad reversed proposal (Hessian or something else is not valid). Reject.
              {
                AccPr <- 0
              }
          } # if(errflgapror==FALSE)
        else # Bad proposal (Hessian or something else is not valid). Reject.
          {
            AccPr <- 0
          }

        ## Decide on acceptance and, possibly, update posterior arguments.
        if (runif(1)<AccPr)
          {
            paramCurr <- paramProp
            ICurr <- IProp
          }

      } # End if all(IProp==ICurr) & update>0

  } # End Update loop

    list(paramCurr = paramCurr, ICurr=ICurr, AccPr=AccPr)
}
