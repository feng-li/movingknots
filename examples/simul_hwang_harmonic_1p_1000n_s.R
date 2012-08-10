#!/usr/bin/env Rscript
##########################################################################################
##                            INTRODUCTION AND HELP
##
##----------------------------------------------------------------------------------------
## This is the main settings file for the moving knots project with multi-shrinkage model.
##
##---USER INPUTS--------------------------------------------------------------------------
## Variables commented with all CAPITAL LETTERS are user defined variables.
##
##---OUTPUTS------------------------------------------------------------------------------
## Variables named with the format of "OUT.xxx" are the final outputs
##
##---HOW TO SPEEDUP-----------------------------------------------------------------------
## You may recompile R from source with an optimized BLAS (Basic Linear Algebra
## Subprograms), e.g ATLAS(BSD-style license), GotoBLAS(BSD-style license ), Intel Math
## Kernel Library(free for personal use). All of them support multi-threaded computing via
## openMP. Read the R-admin guide and individual BLAS users guide to enable it.
##
##---------------------------------------------------------------------------------------
## AUTHOR: Feng Li,  Department of statistics, Stockholm University, Sweden
## DATE:   Sat Mar 05 19:05:40 CET 2011
##
##########################################################################################

##########################################################################################
##                                   User settings
##########################################################################################

##----------------------------------------------------------------------------------------
## Initialize R environment
##----------------------------------------------------------------------------------------

rm(list = ls())
gc()

## LOAD DEPENDENCES
require("mvtnorm")

## PATH FOR THE MOVING KNOTS LIBRARY
path.lib <- "../R"

## SAVE OUTPUT PATH
save.output <- "Results" # "save.output = FALSE" will not save anything

## Load sourceDir() function
sys.source(file.path(path.lib, "utils/sourceDir.R"), envir = .GlobalEnv)

## Load user defined functions
sourceDir(file.path(path.lib, c("utils", "stable", "models/linear")),
          byte.compile = FALSE, recursive = TRUE, silent = FALSE, ignore.error = FALSE)

## MCMC TRAJECTORY
track.MCMC = TRUE

##----------------------------------------------------------------------------------------
## Data input and summary
##----------------------------------------------------------------------------------------

## no. of observations
n <- 1000

## no. of dimensions
p <- 1

## Covariance matrix
{if(p >= 2)
   {
     Sigma.gen <- diag(p)*0.1
     Sigma.gen[Sigma.gen == 0] <- 0.1
   }
else
  {
    Sigma.gen <- matrix(.1)
  }}

## no. of knots in the estimation
q.moving_seq <- c(5, 10, 15)

q.fixed_seq <- c(5, 10, 15, 20, 25, 50)

## no. of replications.
nRep <- 10

## no. covariates simulate for predictions
nPred <- n

## Starting date
Running.date <- Sys.time()

##----------------------------------------------------------------------------------------
## Model configurations
##----------------------------------------------------------------------------------------
## SHORT MODEL DESCRIPTION
ModelDescription <- "simul_hwang_harmonic_1p_1000n"

## MODEL NAME
Model_Name <- "linear"

## DGP MODEL
## "simple" "radial", "harmonic", "additive", "interaction"
DGP.model <- c("harmonic")

## Simulations with different
OUT.q.s_seq <- c(q.moving_seq, q.fixed_seq)

## Total runs in a lap
oneRep.len <- length(OUT.q.s_seq)

## Which runs should be fixed
OUT.fixed_seq <- rep(TRUE, oneRep.len)
OUT.fixed_seq[0:length(q.moving_seq)] <- FALSE

## storage for loss results.
LOSS.tmp <- matrix(NA, oneRep.len, nRep)

## Storage for computing time
CompTim.tmp <- matrix(NA, oneRep.len, nRep)

## Total Iterations
totalRep <- oneRep.len*nRep

## DGP output
OUT.Data.gen <- list()

## Measure of nonlinearities in DGP
OUT.NolinFct <- list()

## The random testing locations
OUT.Data.testing <- list()

## Prediction results
OUT.Data.pred <- list()

idx4Data <- 0
for(iRep in 1:totalRep)
{
  ## Check if need to generate New dataset
  if(iRep  %in% seq(1, totalRep, by = oneRep.len)) # Generate new data
    {
      idx4Data <- idx4Data + 1

      ## Generate new dataset
      Data.gen <- DGP.hwang(n = n, Sigma = Sigma.gen, model = DGP.model, PlotData = FALSE)

      OUT.Data.gen[[idx4Data]] <- Data.gen
      OUT.NolinFct[[idx4Data]] <- Data.gen$NonlinFactor
      x <- Data.gen$x
      Y <- Data.gen$Y
      q.o <- ncol(x)

      ## Generate new x for out-of-sample predictions
      ## TODO: Use Ellpses (Mardia 39)
      x.testing <- matrix(runif(nPred*q.o, min(x), max(x)), nPred)
    }
  ## Which model should run
  which.model <- iRep %% oneRep.len
  if(which.model == 0)
    {which.model <- oneRep.len}


  if(OUT.fixed_seq[which.model] == FALSE) # Moving knots
    {
      Params4Gibbs <- c("knots", "shrinkages", "covariance")
    }
  else # Fixed knots
    {
      Params4Gibbs <- c("shrinkages", "covariance")
    }
  q.s <- OUT.q.s_seq[which.model] # knots used for current model

  ## ARGUMENTS FOR SPLINES
  splineArgs <- list(comp = c("intercept", "covariates", "thinplate.s"), # the components of the design matrix.
                         thinplate.s.dim = c(q.s, q.o)) # the dimension of the knots for surface.

  ## FIXED PARAMETERS
  Params_Fixed <- list("knots" = list(thinplate.s = 0, thinplate.a = 0), # which knots
                                        # from which part of model are not updated.
                       "shrinkages" = 1:p, # the shrinkages for covariates not updated
                       "covariance"  = 0,   # zero means all are updated
                       "coefficients" = 0)

  ## ARGUMENTS FOR PARTITION PARAMETERS (BATCHES UPDATE)
  Params_subsetsArgs <- list("knots" = list(N.subsets = 1, partiMethod = "systematic", split = FALSE),
                             "shrinkages" = list(N.subsets = 1, partiMethod = "systematic"),
                             "covariance"  = list(N.subsets = 1, partiMethod = "systematic"),
                             "coefficients" = list(N.subsets = 1, partiMethod = "systematic"))

##----------------------------------------------------------------------------------------
## Parameters settings
##----------------------------------------------------------------------------------------

  ## TRANSFORMATION FUNCTION
  Params_Transform <- list("knots" = "identity",
                           "shrinkages" = "log",
                           "covariance" = "identity",
                           "coefficients" = "identity")

  ## HESSIAN METHODS
  hessMethods <- list("knots" = "outer",
                      "shrinkages" = "outer",
                      "covariance" = NA,
                      "coefficients" = NA)

  ## Propose method in Metropolis-Hasting
  propMethods <- list("knots" = "KStepNewton",
                      "shrinkages" = "KStepNewton",
                      "covariance" = "Inverse-Wishart", # random MH without K-step Newton
                      "coefficients" = NA)

##----------------------------------------------------------------------------------------
## MCMC configurations
##----------------------------------------------------------------------------------------

  ## NO. OF ITERATIONS
  nIter <- 5000

  ## BURN-IN
  burn.in <- 0.2  # [0, 1) If 0: use all MCMC results.

  ## LPDS SAMPLE SIZE
  LPDS.sampleProp <- 0.05 # Sample proportion to the total posterior after burn-in.

  ## CROSS-VALIDATION
  cross.validation <- list(N.subsets = 1, # No. of folds. If 1:, no cross-validation.
                           partiMethod = "systematic", # How to partition the data
                           full.run = FALSE)     # Also include a full run.

  ## NO. OF FINITE NEWTON MOVE FOR EACH PARAMETERS
  nNewtonSteps <- list("knots" = 1,
                       "shrinkages" = 1,
                       "covariance" = NA, # random MH
                       "coefficients" = NA) # integrated out

  ## THE DF. FOR A MULTIVARIATE T-PROPOSAL IN MH ALGORITHM.
  MH.prop.df <- list("knots" = 10,
                     "shrinkages" = 10,
                     "covariance" = NA,
                     "coefficients" = NA)
##----------------------------------------------------------------------------------------
## Set up Priors
##----------------------------------------------------------------------------------------

  ## TODO: The prior should be set in the transformed scale when the linkages is not
  ## "identity". Write a general function to handle this.

  ## Regression
  knots.by.kmeans <- make.knots(x = x, method = "k-means", splineArgs)
  X.init <- d.matrix(x, knots = knots.by.kmeans, splineArgs)
  lm.init <- lm(Y~0+X.init)
  S0.init <- matrix(var(lm.init$residual), p, p)
  q <- dim(X.init)[2]

  ## P MATRIX TYPE
  ## P.type <- c("identity", "identity", "identity") # can be "identity" or "X'X"
  P.type <- c("X'X", "identity", "identity") # can be "identity" or "X'X"

  ## PRIOR FOR COVARIANCE
  covariance.priType <- "Inverse-Wishart"
  covariance.df0 <- 10
  covariance.S0 <- S0.init # p-by-p, see Mardia p.158

  ## PRIOR FOR COEFFICIENTS
  coefficients.priType <- "mvnorm"
  coefficients.mu0 <- matrix(0, q*p, 1)  # mean of B|Sigma, assume no covariates in.

  ## PRIOR FOR KNOTS
  knots.priType <- "mvnorm"
  knots.mu0 <- knots.list2mat(knots.by.kmeans) # mean from
                                        # k-means
  knots.Sigma0 <- make.knotsPriVar(x, splineArgs) # the covariance for each knots came
                                        # from x'x
  knots.c <- n # The shrinkage

  ## PRIOR FOR SHRINKAGES
  model.comp.len <- length(splineArgs[["comp"]][ "intercept" != splineArgs[["comp"]] ])
                                        # how many components does the model have
  shrinkages.pri.trans <- convert.densParams(mean = n/2, var = (n/2)^2, linkage =
                                             Params_Transform[["shrinkages"]]) # assume
                                        # normal prior with "mean" and "var"
  shrinkages.priType <- "mvnorm"
  shrinkages.mu0 <- matrix(rep(shrinkages.pri.trans[1], p*model.comp.len)) # The mean of
                                        # shrinkage,  "n" is unit information
                                        # prior. (n*(X'X)^(-1))
  shrinkages.Sigma0 <- diag(rep(shrinkages.pri.trans[2], p*model.comp.len), p*model.comp.len) # The variance
                                        # for the shrinkage parameter.
  shrinkages.c <- 1 # The shrinkage

  ## Organize the arguments
  priorArgs <- list(P.type = P.type,

                    knots.priType = knots.priType,
                    knots.mu0 = knots.mu0, # prior for knots
                    knots.Sigma0 = knots.Sigma0,
                    knots.c = knots.c,

                    shrinkages.priType = shrinkages.priType,
                    shrinkages.mu0 = shrinkages.mu0, # prior for shrinkages
                    shrinkages.Sigma0 = shrinkages.Sigma0,
                    shrinkages.c  = shrinkages.c,

                    coefficients.priType = coefficients.priType,
                    coefficients.mu0 = coefficients.mu0, # prior for coefficients

                    covariance.priType = covariance.priType,
                    covariance.df0 = covariance.df0, # prior for covariance
                    covariance.S0 = covariance.S0)

##----------------------------------------------------------------------------------------
## Initial values
##----------------------------------------------------------------------------------------
  ## TODO: The initial values should be transformed into the new scale according to the
  ## linkages if it is not "identity"

  ## INITIAL KNOTS LOCATIONS, "list"
  INIT.knots <- knots.by.kmeans

  ## INITIAL SHRINKAGE FOR MODEL COVARIANCE "matrix"
  INIT.shrinkages <- shrinkages.mu0

  ## INITIAL COVARIANCE "matrix"
  INIT.covariance <- covariance.S0

##########################################################################################
##                                   System settings
##########################################################################################

##----------------------------------------------------------------------------------------
## Initialize the data
##----------------------------------------------------------------------------------------
  ## Gradient function name
  gradhess.fun.name <- tolower(paste(Model_Name, "gradhess", sep = "_"))

  ## Log posterior function name
  logpost.fun.name <-  tolower(paste(Model_Name, "logpost", sep = "_"))

##----------------------------------------------------------------------------------------
## Set up cross validation etc
##----------------------------------------------------------------------------------------

  ## The training($training) and testing($testing) structure.
  ## If no cross-validation, $training is also $testing.
  ## If full run is required, the last list in $training and $testing is for a full run.
  crossvalid.struc <- set.crossvalid(n.obs = n, crossvalidArgs = cross.validation)

  ## No. of total runs
  nCross <- length(crossvalid.struc$training)

  ## No. of training obs. in each data subset.
  nTraining <- unlist(lapply(crossvalid.struc$training, length))

  ## Params
  Params <- list("knots" = knots.list2mat(INIT.knots),
                 "shrinkages" = INIT.shrinkages,
                 "covariance" = vech(INIT.covariance),
                 "coefficients" = matrix(NA, q, p))

  ## The parameters subset structures.
  Params.sub.struc <- Params.subsets(p, splineArgs, Params_Fixed, Params_subsetsArgs)

##----------------------------------------------------------------------------------------
## Construct the output formats
##----------------------------------------------------------------------------------------

  ## NOTATIONS TO USE
  ## The output is alway with "OUT.XXX"
  ## The last dimension is always for the i:th cross-validation subsets.

  ## Accept probabilities for MH.
  OUT.accept.probs <- mapply(function(x) array(NA, c(length(x), nIter, nCross)),
                             Params.sub.struc, SIMPLIFY = FALSE)

  ## Parameters updates in each MH step
  INIT.knots.mat <- knots.list2mat(INIT.knots)

  OUT.Params <- list("knots" = array(INIT.knots.mat, c(length(INIT.knots.mat), 1, nIter,
                       nCross)) ,
                     "shrinkages" = array(INIT.shrinkages, c(p*model.comp.len, 1, nIter,
                       nCross)),
                     "coefficients" = array(NA, c(q, p, nIter, nCross)),
                     "covariance" = array(vech(INIT.covariance), c((p+1)*p/2, 1, nIter,
                       nCross)))

##########################################################################################
##                                 Testings
##########################################################################################
## See the "tests" folder and tests at end of each function.


##########################################################################################
##                                   Main algorithm
##########################################################################################

##----------------------------------------------------------------------------------------
## Stabilize the initial values
##----------------------------------------------------------------------------------------
## see "tests/test.init.BFGS.R" file

##----------------------------------------------------------------------------------------
## MovingKnots MCMC
##----------------------------------------------------------------------------------------
  init.time <- proc.time()

  cat(paste("Running: \"", ModelDescription, "_rep(", iRep, ")\" @",Sys.time(),".\n\n",
            sep = ""))

  MovingKnots_MCMC(gradhess.fun.name, logpost.fun.name, nNewtonSteps, nIter, Params,
                   Params4Gibbs, Params.sub.struc, hessMethods, Y, x, splineArgs, priorArgs,
                   MH.prop.df, Params_Transform, propMethods, crossvalid.struc, OUT.Params,
                   OUT.accept.probs, burn.in, LPDS.sampleProp, track.MCMC)
  finish.time <- proc.time()
##----------------------------------------------------------------------------------------
## Posterior analyses
##----------------------------------------------------------------------------------------
  MovingKnots_Dignosis <- FitDiagnosis.hwang(x = x.testing, Y = NA, OUT.Params, Data.gen,
                                             logpost.fun.name, splineArgs,
                                             Params_Transform, crossvalid.struc, burn.in,
                                             criterion = c("LOSS"), hwang.model = DGP.model)

  LOSS.tmp[iRep] <- MovingKnots_Dignosis$LOSS
  OUT.Data.pred[[iRep]] <- MovingKnots_Dignosis
  CompTim.tmp[iRep] <- (finish.time-init.time)["elapsed"]
  cat("LOSS for ", ifelse(OUT.fixed_seq[which.model], "fixed_", "moving_"), q.s,
      "_knots: ", MovingKnots_Dignosis$LOSS, "\n", sep = "")
}

## The final mean squared loss
OUT.LOSS <- t(LOSS.tmp)
## The using time
OUT.time <- t(CompTim.tmp)

##----------------------------------------------------------------------------------------
## Save outputs to files
##----------------------------------------------------------------------------------------
save.all(save.output, ModelDescription, Running.date)

cat(paste("Finished at", Sys.time(),"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n"))
##########################################################################################
##                                     THE END
##########################################################################################
