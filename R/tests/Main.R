rm(list=ls()) # clear

## ---------------------Source the R scripts-----------------------------------------------
## setwd("/where-is-your-Main.R/")
source("src/sourceDir.R") # Load a more powerful source function
sourceDir("src") # Source all R scripts in "src" folder

## ---------------------Load the data------------------------------------------------------
## "R.matlab" is to read the matlab data directly(without need Matlab software).
## library("R.matlab")
## sp500 <- readMat("SP500Oct28_2008Logs.mat")
## x <- cbind(1,sp500$X) # We include the intercept in the model
## y <- sp500$y

sp500 <- read.table("sp500log.txt")
x <- as.matrix(sp500[,1:11])  ## Intercept include already.
y <- as.matrix(sp500[,12])


## --------------------- Pre-Computing----------------------------------------------------
## This part inculde some matrix results which may speed up the algorithm
XX <- crossprod(x)  # FAST!  t(x)%*%x
invXX <- solve(XX)


## --------------------Prameters input----------------------------------------------------

## The Intercepts are alway in.
OnTrialDelta <- 2:11
OnTrialAlpha <- 2:11
OnTrialGamma <- 2:11

## The starting value
library("hett")  # Get the starting value for t error model. Taylor and Verbyla (2004)
Alpha <- as.matrix(tlm(y~x-1)$loc.fit$coefficients+.15) # The intercept is already in x.
## Too close to true value is not good. The iteration does not move at all.

Delta <- as.matrix(rnorm(11))
Gamma <- as.matrix(rnorm(11))

nIter <- 1000

ICurrAlpha <- as.matrix(c(1,0,0,0,0,0,0,0,0,0,0))
ICurrDelta <- as.matrix(c(1,0,0,0,0,0,0,0,0,0,0))
ICurrGamma <- as.matrix(c(1,0,0,0,0,0,0,0,0,0,0)) ## All out is not good, All in?

cAlpha <- length(y)
cDelta <- length(y)
cGamma <- length(y)

PrInAlpha <- .5
PrInDelta <- .5
PrInGamma <- .5

IUpdatePrAlpha <- .2
IUpdatePrDelta <- .2
IUpdatePrGamma <- .2

nNewtonStepsAlpha <- 3
nNewtonStepsDelta <- 3
nNewtonStepsGamma <- 3

propDfAlpha <- 50
propDfDelta <- 50
propDfGamma <- 50

Sigma <- 1

gradHessFuncAlpha <- "GradHessAlpha"
gradHessFuncDelta <- "GradHessDelta"
gradHessFuncGamma <- "GradHessGamma"

logPostFuncAlpha <- "LogPostAlpha"
logPostFuncDelta <- "LogPostDelta"
logPostFuncGamma <- "LogPostGamma"

## --------------------The output---------------------------------------------------------

AlphaNew <- NULL                # p x nInter    The output of alpha for each iteration
DeltaNew <- NULL                # p x nInter    The output of delta for each iteration
GammaNew <- NULL                # p x nInter    The output of gamma for each iteration

ICurrAlphaNew <- NULL           # p x nInter    The variable selection indicators of alpha for each iteration
ICurrDeltaNew <- NULL           # p x nInter    The variable selection indicators of delta for each iteration
ICurrGammaNew <- NULL           # p x nInter    The variable selection indicators of gamma for each iteration

AccPrAlphaNew <- NULL           # nInter x 1    Accept probability for alpha
AccPrDeltaNew <- NULL           # nInter x 1    Accept probability for delta
AccPrGammaNew <- NULL           # nInter x 1    Accept probability for gamma

## --------------------The main iteration-------------------------------------------------
for (i in 1:nIter)
{

    finGamma <- finiteNewtonProp(paramCurr=Gamma,ICurr=ICurrGamma,PrIn=PrInGamma,
                                 OnTrial=OnTrialGamma, nNewtonSteps=nNewtonStepsGamma,
                                 IUpdatePr=IUpdatePrGamma,logPostFunc=logPostFuncGamma,
                                 gradHessFunc=gradHessFuncGamma,x=x,y=y,propDf=propDfGamma)
    Gamma <- finGamma$paramCurr
    GammaNew <- rbind(GammaNew,t(Gamma))
    ICurrGamma <- finGamma$ICurr
    ICurrGammaNew <- rbind(ICurrGammaNew,t(ICurrGamma))
    AccPrGamma <- finGamma$AccPr
    AccPrGammaNew <- rbind(AccPrGammaNew,t(AccPrGamma))

    finDelta <- finiteNewtonProp(paramCurr=Delta,ICurr=ICurrDelta,PrIn=PrInDelta,
                                 OnTrial=OnTrialDelta, nNewtonSteps=nNewtonStepsDelta,
                                 IUpdatePr=IUpdatePrDelta,logPostFunc=logPostFuncDelta,
                                 gradHessFunc=gradHessFuncDelta,x=x,y=y,propDf=propDfDelta)
    Delta <- finDelta$paramCurr
    DeltaNew <- rbind(DeltaNew,t(Delta))
    ICurrDelta <- finDelta$ICurr
    ICurrDeltaNew <- rbind(ICurrDeltaNew,t(ICurrDelta))
    AccPrDelta <- finDelta$AccPr
    AccPrDeltaNew <- rbind(AccPrDeltaNew,t(AccPrDelta))


    finAlpha <- finiteNewtonProp(paramCurr=Alpha,ICurr=ICurrAlpha,PrIn=PrInAlpha,
                                 OnTrial=OnTrialAlpha, nNewtonSteps=nNewtonStepsAlpha,
                                 IUpdatePr=IUpdatePrAlpha,logPostFunc=logPostFuncAlpha,
                                 gradHessFunc=gradHessFuncAlpha,x=x,y=y,propDf=propDfAlpha)
    Alpha <- finAlpha$paramCurr
    AlphaNew <- rbind(AlphaNew,t(Alpha))
    ICurrAlpha <- finAlpha$ICurr
    ICurrAlphaNew <- rbind(ICurrAlphaNew,t(ICurrAlpha))
    AccPrAlpha <- finAlpha$AccPr
    AccPrAlphaNew <- rbind(AccPrAlphaNew,t(AccPrAlpha))

    print(paste(i,"of",nIter)) # where I am?
}
## --------------------End Here-----------------------------------------------------------

