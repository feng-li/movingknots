##----------------------------------------------------------------------------------------
## Some plots if within 3D
##----------------------------------------------------------------------------------------

## if(p == 1 && m == 2 && track.MCMC == TRUE)
## {
##   points(OUT.Params.mode[["xi"]][, , nCross], col = "red", pch = 17, lwd = 2); points(xi.desi, col =
##                                                       "blue", pch = 16, lwd = 2) 
##   title(xlab = "Background: contour plot for the true surface.\n Red: Posterior modes of  knots; Blue: True knots") 
  
##   n.grid <- 20
##   x1 <- seq(0, 1, length.out = n.grid)
##   x2 <- seq(0, 1, length.out = n.grid)
##   x.mesh <- mesh.grid(x1, x2)
##   Y.mesh <- matrix(NA, n.grid^2, nIter)
  
##   for(kIter in 1:nIter) # Find the mean surface
##     {
##       X.mesh.tmp <- d.matrix(x = x.mesh, xi  = OUT.Params[["xi"]][, , kIter, nCross], args = splineArgs)
##       Y.mesh[, kIter] <- X.mesh.tmp%*%OUT.Params[["B"]][, , kIter, nCross]
##     }
    
##   Y.mesh.mode <- matrix(rowMeans(Y.mesh))
  
##   ## Add the mean surface to the DGP surface.
##   rgl.surface(x1, x2, Y.mesh.mode, color="#FF2222",alpha=0.5)
## }
