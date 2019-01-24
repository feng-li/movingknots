###----------------------------------------------------------------------------
### Generate mesh data for the posterior plots
###----------------------------------------------------------------------------

## Load the raw MCMC results
setwd("~/running/prioreval/Results/new")

rdfiles <- dir(pattern = "rajan_pri")
for(i in rdfiles)
  {
    load(i)
    knots.ary <- OUT.Params[["knots"]]
    nDrop <- round(nIter*burn.in) # no. of burn-in

    ngrid0 <- 70
    ## x1.grid <- seq(min(x[, 1]), max(x[, 1]), length.out = ngrid0)
    ## x2.grid <- seq(min(x[, 2]), max(x[, 2]), length.out = ngrid0)

    ## The final grid on the original scale
    ## X1 <- seq(-, 43, length.out = ngrid0) # for the knots
    ## X2 <- seq(-3.6, 1.2, length.out = ngrid0)

    X1 <- seq(-0.1, 15.1, length.out = ngrid0) # for the surface
    X2 <- seq(-1.1, 1.1, length.out = ngrid0)

    ## The standard ones
    x1.grid <- (X1- mean(X[, 2]))/sd(X[, 2])
    x2.grid <- (X2- mean(X[, 4]))/sd(X[, 4])

    x.mesh <- mesh.grid(x1.grid, x2.grid)

    ## obtain locations TODO: apply
    q.s <- splineArgs$thinplate.s.dim[1]
    q.a1 <- splineArgs$thinplate.a.locate[1]
    Y.mesh <- matrix(NA, ngrid0^2, nIter)
    knots.s.mat <- matrix(NA, nIter*q.s, 2)
    knots.a.mat <- matrix(NA, nIter*q.a1, 2) # not right if
                                        # additive knots are not the same
    for(i in 1:nIter)
      {
        knots.ilst <- knots_mat2list(OUT.Params[["knots"]][, , i, ], splineArgs)
        knots.s.mat[(1+q.s*(i-1)):(i*q.s), ] <- knots.ilst[["thinplate.s"]]
        knots.a.mat[(1+q.a1*(i-1)):(i*q.a1), ] <- matrix(knots.ilst[["thinplate.a"]], q.a1, 2)

        X.desi <- d.matrix(x.mesh, knots.ilst, splineArgs)
        B <- matrix(OUT.Params[["coefficients"]][, , i,], , 1)
        Y.mesh[, i] <- X.desi %*% B
        ## progressbar(i, nIter)
      }

    y.orig0 <- exp(Y.mesh)/(1+exp(Y.mesh))

    y.origmean <- matrix(rowMeans(y.orig0), ngrid0)
    y.origsd <- matrix(apply(y.orig0, 1, sd), ngrid0)

    rm(y.orig0, Y.mesh)
    ofile <- paste("Plot_", ModelDescription, ".Rdata", sep = "")
    save.image(file = ofile)

  }


###----------------------------------------------------------------------------
### Posterior plot
###----------------------------------------------------------------------------

## Load the plot mesh

setwd("~/running/prioreval/Results/plots")
PlotRDfiles <- dir(pattern = "Plot_rajan_pri_kmeans")
for(i in PlotRDfiles)
  {
    load(i)
    load("knots.Rdata")
    source("~/workspace/MovingKnots/R/scripts/plots/PlotRajanPosterior.R")
  }
