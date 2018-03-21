PredSurface <- function(Y, x0, OUT.params, jIter, iCross, splineArgs)
{
    Params.j <- lapply(OUT.Params, function(x)
        apply(x[, , jIter, iCross, drop = FALSE], c(1, 2), "["))

    ## diag.K <- Params.j[["shrinkages"]]
    ## Sigma <- vech2m(Params.j[["covariance"]])
    B <- Params.j[["coefficients"]]
    knots.mat <- Params.j[["knots"]]
    knots.list <- knots.mat2list(knots.mat, splineArgs)

    X.pred <- d.matrix(x = x0, knots = knots.list, splineArgs)

    SurfaceMean <- X.pred %*% B

    return(SurfaceMean)
}
