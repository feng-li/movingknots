## The posterior plot for rajan data
load("rajan_s_moving_20_plus_a_moving_4_2011May26-18.01.26.Rdata")
require("gplots")

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
    knots.ilst <- knots.mat2list(OUT.Params[["knots"]][, , i, ], splineArgs) 
    knots.s.mat[(1+q.s*(i-1)):(i*q.s), ] <- knots.ilst[["thinplate.s"]]
    knots.a.mat[(1+q.a1*(i-1)):(i*q.a1), ] <- matrix(knots.ilst[["thinplate.a"]], q.a1, 2)
    
    X.desi <- d.matrix(x.mesh, knots.ilst, splineArgs)
    B <- matrix(OUT.Params[["coefficients"]][, , i,], , 1)
    Y.mesh[, i] <- X.desi %*% B
    progressbar(i, nIter)
  }

y.orig0 <- exp(Y.mesh)/(1+exp(Y.mesh))

y.origmean <- matrix(rowMeans(y.orig0), ngrid0)
y.origsd <- matrix(apply(y.orig0, 1, sd), ngrid0)

rm(y.orig0, Y.mesh)
save.image(file = "RajanPlot.Rdata")

##########################################################################################
## The plotting procedure 
##########################################################################################
## load("test.Rdata")
load("RajanPlot.Rdata") # too big
require("gplots")

## Transform back to raw scale
meanRaw.s <- matrix(colMeans(X[, c(2, 4)]),  nIter*q.s, 2, byrow = TRUE)
sdRaw.s <- matrix(sd(X[, c(2, 4)]), nIter*q.s, 2, byrow = TRUE)
meanRaw.a <- matrix(colMeans(X[, c(2, 4)]),  nIter*q.a1, 2, byrow = TRUE)
sdRaw.a <- matrix(sd(X[, c(2, 4)]), nIter*q.a1, 2, byrow = TRUE)

knots.s.matRaw <- (knots.s.mat + meanRaw.s)*sdRaw.s
knots.a.matRaw <- (knots.a.mat + meanRaw.a)*sdRaw.a

contourData <- hist2d(knots.s.matRaw, show = FALSE, nbins=100)

##load("rajan_s_moving_20_plus_a_moving_4.Rdata")

market2book <- contourData$x
profit <- contourData$y
cnt <- contourData$counts/nIter
##
xlim0 <- c(-3.6, 38)
ylim0 <- c(-3.6, 1.2)
x.at <- seq(-5, 45, 5)
y.at <- seq(-4, 1.5, .5)

nbinsx0 <- 70
knots.x <- knots.a.matRaw[, 1]
fake.y <- seq(-3.58,-3.38, length.out = nIter*q.a1)
cont.x <- hist2d(cbind(knots.x, fake.y), show = FALSE, nbins=nbinsx0)

knots.y <- knots.a.matRaw[, 2][knots.a.matRaw[, 2]<1.2]
fake.x <- seq(-3.5, -2.20, length.out = nIter*q.a1)
nbinsy0 <- 70
cont.y <- hist2d(cbind(fake.x, knots.y), show = FALSE, nbins=nbinsy0)


## knots in the raw scale
init.knots.s <- (INIT.knots[["thinplate.s"]] + meanRaw.s[1:q.s, , drop = FALSE])*sdRaw.s[1:q.s, , drop = FALSE]
init.knots.a <- (matrix(INIT.knots[["thinplate.s"]], q.a1, 2) + meanRaw.s[1:q.a1, , drop = FALSE])*sdRaw.s[1:q.a1, , drop = FALSE]




nlev <- 14
color0 <- terrain.colors(nlev+1)[15:1]

par(ps = 10, cex = 1, cex.main = 1, cex.sub = 1, cex.axis = 1, bty = "o")

filled.contour(market2book, profit, cnt, xlim = xlim0, ylim = ylim0, col = color0,
               levels = seq(0, 1400/nIter, length.out = nlev+1), axes = TRUE, las = 1,  
               xlab = "Market2Book", ylab = "Profit", main =
               "Posterior locations of knots", key.title = title(main=""),
               plot.axes = {axis(1, at = x.at)
                            axis(2, at = y.at)
                            points(init.knots.s, pch = 19, col = "blue", cex = 0.6)
                            contour(market2book, profit, cnt, xlim = xlim0, ylim = ylim0, add = TRUE, col = "black", lwd = 0.8, lty = "solid")
                            image(cont.x$x, cont.x$y, matrix(rowSums(cont.x$counts), nbins0, nbins0), col = color0,
                                  add = TRUE, axes = FALSE)
                            image(cont.y$x, cont.y$y, matrix(colSums(cont.y$counts), nbins0, nbins0, byrow = TRUE),
                                  col = color0, add = TRUE, axes = FALSE)    
                            points(init.knots.a[1:3, 1],rep(-3.6, 3),  pch = 18, col = "white", cex = 1.5)
                            points(init.knots.a[1:3, 1],rep(-3.6, 3),  pch = 18, col = "blue", cex = 1.0, xpd = NA)
                            points(init.knots.a[4, 1]-0.3,rep(-3.6, 1),  pch = 18, col = "white", cex = 1.5)
                            points(init.knots.a[4, 1]-0.3,rep(-3.6, 1),  pch = 18, col = "blue", cex = 1.0, xpd = NA)
                            points(rep(-3.65, 4), init.knots.a[, 2],  pch = 18, col = "white", cex = 1.5)
                            points(rep(-3.65, 4), init.knots.a[, 2],  pch = 18, col = "blue", cex = 1.0, xpd = NA)
                            legend(x = 20, y = -2.5, pch = c(20, 18), col = c("blue", "blue"), cex = c(1, 1), legend =
                                   c("Prior mean of surface knots", "Prior mean of additive knots"))})
                            
##----------------------------------------------------------------------------------------
## Plot Post mean surface and variance
##----------------------------------------------------------------------------------------
xlim1 <- c(-0.1, 15.1)
ylim1 <- c(-1.1, 1.1)
x.at1 <- seq(-5, 20, 5)
y.at1 <- round(seq(-1.2, 1.2, 0.2), 1)


par(ps = 10, cex = 1, cex.main = 1, cex.sub = 1, cex.axis = 1, bty = "o")
filled.contour(X1, X2, y.origmean, col = color0, xlim = xlim1, ylim = ylim1, 
               levels = seq(min(y.origmean), max(y.origmean), length.out = nlev+1), xlab = "Market2Book", ylab = "Profit",
               main = "Posterior surface (mean)",
               key.axes = {axis(4, at = seq(0, 1, 0.1))}, 
               plot.axes = {axis(1, at = x.at1)
                            axis(2, at = y.at1)
                            contour(X1, X2, y.origmean, add = TRUE,  col = "black", lwd = 0.8, lty = "solid", xlim = xlim1, ylim = ylim1)
                          })
## points(X[, 2], X[, 4], col = "white", cex = 1.2, pch = 20)
## points(X[, 2], X[, 4], col = "blue", pch = 20, cex = 0.6)

par(ps = 10, cex = 1, cex.main = 1, cex.sub = 1, cex.axis = 1, bty = "o")
filled.contour(X1, X2, y.origsd, col = color0, xlim = xlim1, ylim = ylim1, 
               levels = seq(min(y.origsd), max(y.origsd), length.out = nlev+1), xlab = "Market2Book", ylab
               = "Profit", main = "Posterior surface (standard deviation)",
               key.axes = {axis(4, at = seq(0, 0.5, 0.05))}, 
               plot.axes = {axis(1, at = x.at1)
                            axis(2, at = y.at1)
                            points(X[, 2], X[, 4], col = "white", cex = 1.2, pch = 20, xlim = xlim1, ylim = ylim1)
                            points(X[, 2], X[, 4], col = "blue", cex = 0.6, pch = 20, xlim
                                   = xlim1, ylim = ylim1)
                            contour(X1, X2, y.origsd, add = TRUE,  col = "black", lwd = 0.8, lty = "solid", xlim = xlim1, ylim = ylim1, )
                          })
