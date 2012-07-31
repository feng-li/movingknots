##' A collection of DGPs for different models.
##'
##' The model can be extended.
##' @name DGP
##' @title DATA GENERATING PROCESSES
##' @param n "numeric".
##'         No. of obs. to be generated
##' @param Sigma "matrix".
##'         The variance covariance matrix.
##' @param model "character".
##'         The model name to be use.
##'         When the model is "spline",  you also need to supply paramters,  see otherArgs.
##' @param otherArgs "list".
##'         Other arguemnts need to pass to the function.
##'         otherArgs$seed: Set the seed. If this vaule is unset, use a random seed.
##' @param PlotData "logical"
##'         If need to plot the data out.
##'
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Tue Mar 30 20:11:06 CEST 2010;
##'       Current:       Sun Sep 19 11:58:28 CEST 2010.
##' DEPENDS: mvtnorm
DGP <- function(n, Sigma, model, otherArgs,  PlotData)
{
  ## Get user supplied seed.
  seed <- otherArgs$seed

  ## If the seed is set, use it.
  if(is.null(seed)  == FALSE) set.seed(seed)
  ## n uniform draws on the unit square.
  X.orig <- matrix(runif(n*2), n, 2) # [0, 1]

  ## Make the grid values for printing out the surface.
  ## FIXME: maybe replace this with expand.grid().
  ## I don't like expand.grid which gives a data frame.
  n.grid <- 20
  X1.grid <- seq(from = 0, to = 1, length.out = n.grid) # n-by-1
  X2.grid <- seq(from = 0, to = 1, length.out = n.grid) # n-by-1
  X.mesh <- mesh.grid(X1.grid, X2.grid)
  ## X1.mesh <- rep(X1.grid, each = n.grid) # n*n-by-1
  ## X2.mesh <- rep(X2.grid, times = n.grid) # n*n-by-1
  ## X.mesh <- cbind(X1.mesh, X2.mesh) # n*n-by-2
  X <- rbind(X.orig, X.mesh) # (n + n*n)-by-2
  p <- dim(Sigma)[1]

  ## Generate errors
  ## If the seed is set, use it.
  if(is.null(seed) == FALSE) set.seed(seed)
  Errors <- rmvnorm(n = n, mean = rep(0, p), sigma = Sigma)

  if(tolower(model == "spline")) ## Both univariate and multivariate cases when
    ## "spline.type" is "no-knots", reduces to linear regression
    {

      xi = otherArgs$xi
      withInt <- otherArgs$withInt
      spline.type <- otherArgs$spline.type

      X.desi <- d.matrix(x = X, xi = xi, args = list(method = spline.type, withInt =
                                           withInt))

      q <- dim(X.desi)[2]

      ## If the seed is set, use it.
      if(is.null(seed)  == FALSE)
        {set.seed(seed)}

      B <- matrix(runif(p*q), q, p)

      MeanSurface <- X.desi%*%B
    }
  else if(tolower(model) == "simple") ## only univariate response case
    {
      MeanSurface <- 10.391*((X[,1, drop = FALSE]-0.4)*(X[,2, drop = FALSE]-0.6)+0.36)
    }
  else if(tolower(model)  == "radial") ## only univariate response case
    {
      r2 <- (X[,1, drop = FALSE]-0.5)^2+(X[,2, drop = FALSE]-0.5)^2
      MeanSurface <- 24.234*(r2*(0.75-r2))
    }
  else if(tolower(model) == "harmonic") ## only univariate response case
    {
        XTilde <- X-0.5
        MeanSurface <- 42.659*(0.1+XTilde[,1, drop = FALSE]
                               *(0.05+XTilde[,1, drop = FALSE]^4
                                 -10*XTilde[,1, drop = FALSE]^2*XTilde[,2, drop = FALSE]^2
                                 +5*XTilde[,2, drop = FALSE]^4))
    }
  else if(tolower(model) == "additive") ## only univariate response case
    {
      MeanSurface <- 1.3356*(1.5*(1-X[,1, drop = FALSE])
                             + exp(2*X[,1, drop = FALSE]-1)
                             *sin(3*pi*(X[,1, drop = FALSE]-0.6)^2)
                             +exp(3*(X[,2, drop = FALSE]-0.5))
                             *sin(4*pi*(X[,2, drop = FALSE]-0.9)^2))
    }
  else if(tolower(model) == "interaction") ## only univariate response case
    {
      MeanSurface <- 1.9*(1.35 + exp(X[,1, drop = FALSE])
                          *sin(13*(X[,1, drop = FALSE]-0.6)^2)
                          *exp(-X[,2, drop = FALSE])*sin(7*X[,2, drop = FALSE]))
    }

  ## The final response variable
  MeanSurface.orig <- MeanSurface[1:n, , drop = FALSE]
  y.orig <- MeanSurface.orig + Errors

  y.grid <- matrix(MeanSurface[(n+1):(n+n.grid^2),], n.grid, n.grid)

  out <- list(y = y.orig, Sigma = Sigma, X  = X.orig)

  ## option for print the data out if on unitvariate model
  ## FIXME: use try command to avoid error when gui is not available.
  if(PlotData == TRUE && dim(y.orig)[2] == 1 && dim(x.orig)[2] == 2)
    {
      image(X1.grid, X2.grid, y.grid, xlab = "X1", ylab = "X2")
      filled.contour(X1.grid, X2.grid, y.grid)
      title(toupper(model))

      ## The 3D scatter plot
      require(rgl)
      plot3d(x = X.orig[, 1], y = X.orig[, 2], z = y.orig, col = "red",
            xlab = "X1", ylab = "X2", zlab = "y")

      ## Put the surface on
      ## FIXME: merge the two figures
      rgl.open()
      ## rgl.points(x = X.orig[, 1], y = X.orig[, 2], z = y.orig, radius = 0.01)
      rgl.surface(X1.grid, X2.grid, y.grid, front = "lines", color="#CCCCFF", alpha = .8)
      ##      spheres3d(x = X.orig[, 1], y = X.orig[, 2], z = y.orig, radius = 0.01)
    }


  return(out)
}
