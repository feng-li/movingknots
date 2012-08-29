## The posterior plot for rajan data
## This is the private script, you should run PlotRajanPosteriorBatch.R
##########################################################################################
## The plotting procedure
##########################################################################################

## load("RajanPlotTmp.Rdata")
require("gplots") # hist2d()

SavePlot <- TRUE

## Transform back to raw scale
## TODO: Hard coded for "norm-0-1" method
meanRaw.s <- matrix(colMeans(X[, c(2, 4)]),  nIter*q.s, 2, byrow = TRUE)
sdRaw.s <- matrix(apply(X[, c(2, 4)], 2, sd), nIter*q.s, 2, byrow = TRUE)
meanRaw.a <- matrix(colMeans(X[, c(2, 4)]),  nIter*q.a1, 2, byrow = TRUE)
sdRaw.a <- matrix(apply(X[, c(2, 4)], 2, sd), nIter*q.a1, 2, byrow = TRUE)

knots.s.matRaw <- knots.s.mat*sdRaw.s + meanRaw.s
knots.a.matRaw <- knots.a.mat*sdRaw.a + meanRaw.a

contourData <- hist2d(knots.s.matRaw, show = FALSE, nbins=100)

market2book <- contourData$x
profit <- contourData$y
cnt <- contourData$counts/nIter

##
xlim0 <- c(-3.6, 36)
ylim0 <- c(-3.6, 1.2)
x.at <- seq(-5, 40, 5)
y.at <- seq(-4, 1.5, .5)

nbinsx0 <- 70
knots.x <- knots.a.matRaw[, 1]
fake.y <- seq(ylim0[1]+0.01,ylim0[1]+0.2, length.out = nIter*q.a1)
cont.x <- hist2d(x = knots.x, y = fake.y, show = FALSE, nbins=nbinsx0)

## knots.y <- knots.a.matRaw[, 2][knots.a.matRaw[, 2]<1.2]
knots.y <- knots.a.matRaw[, 2]
fake.x <- seq(xlim0[2]-2.4, xlim0[2]-0.1, length.out = nIter*q.a1)
nbinsy0 <- 70
cont.y <- hist2d(x = fake.x, y = knots.y, show = FALSE, nbins=nbinsy0)


## knots in the raw scale
## TODO: Hard coded for "norm-0-1" method
init.knots.s <- (INIT.knots[["thinplate.s"]]*sdRaw.s[1:q.s, , drop = FALSE] +
                 meanRaw.s[1:q.s, , drop = FALSE])
init.knots.a <- (matrix(INIT.knots[["thinplate.a"]], q.a1, 2)*
                 sdRaw.s[1:q.a1, , drop = FALSE] +
                 meanRaw.s[1:q.a1, , drop = FALSE])

nlev <- 14
color0 <- terrain.colors(nlev+1)[15:1]

dev.new()
par(ps = 10, cex = 1, cex.main = 1, cex.sub = 1, cex.axis = 1, bty = "o")
filled.contour2(market2book, profit, cnt, xlim = xlim0, ylim = ylim0, col = color0,
               levels = seq(0, 0.07, length.out = nlev+1), axes = TRUE, las = 1,
               xlab = "Market2Book", ylab = "Profit",
               main = "Posterior locations of knots", key.title = title(main=""),
               plot.axes = {axis(1, at = x.at)
                            axis(2, at = y.at)
                            points(init.knots.s, pch = 19, col = "blue", cex = 0.6)
                            contour(market2book, profit, cnt, xlim = xlim0,
                                    ylim = ylim0, add = TRUE, col = "black",
                                    lwd = 0.8, lty = "solid")
                            image(cont.x$x, cont.x$y,
                                  matrix(rowSums(cont.x$counts), nbinsx0,
                                         nbinsy0), col = color0,add = TRUE, axes
                                  = FALSE)
                            image(cont.y$x, cont.y$y,
                                  matrix(colSums(cont.y$counts), nbinsx0,
                                         nbinsy0, byrow = TRUE), col = color0,
                                  add = TRUE, axes = FALSE)

                            points(init.knots.a[, 1],rep(ylim0[1], 4),  pch =
                                   18, col = "white", cex = 1.5)
                            points(init.knots.a[, 1],rep(ylim0[1], 4),  pch =
                                   18, col = "blue", cex = 1.0, xpd = NA)

                            ## points(init.knots.a[4, 1]-0.3,rep(-3.6, 1),  pch =
                            ##        18, col = "white", cex = 1.5)
                            ## points(init.knots.a[4, 1]-0.3,rep(-3.6, 1),  pch =
                            ##        18, col = "blue", cex = 1.0, xpd = NA)

                            points(rep(xlim0[2], 4), init.knots.a[, 2],  pch = 18,
                                   col = "white", cex = 1.5)
                            points(rep(xlim0[2], 4), init.knots.a[, 2],  pch = 18,
                                   col = "blue", cex = 1.0, xpd = NA)

                            legend(x = 12, y = ylim0[2]-0.05, pch = c(20, 18), col =
                                   c("blue", "blue"), cex = c(1, 1), legend =
                                   c("Prior mean of surface knots",
                                     "Prior mean of additive knots"))
                          },
                bg = rgb(242, 242, 242, maxColorValue=255)
                )

if(SavePlot)
  {
    dev.copy2eps(file = paste(ModelDescription, "_PostKnots.eps", sep = ""))
  }
##----------------------------------------------------------------------------------------
## Plot Post mean surface and variance
##----------------------------------------------------------------------------------------
xlim1 <- c(-0.1, 15.1)
ylim1 <- c(-1.1, 1.1)
x.at1 <- seq(-5, 20, 5)
y.at1 <- round(seq(-1.2, 1.2, 0.2), 1)

dev.new()
par(ps = 10, cex = 1, cex.main = 1, cex.sub = 1, cex.axis = 1, bty = "o")
filled.contour(X1, X2, y.origmean, col = color0, xlim = xlim1, ylim = ylim1,
               levels = seq(0, 0.75, length.out = nlev+1),
               xlab = "Market2Book", ylab = "Profit",
               main = "Posterior surface (mean)",
               key.axes = {axis(4, at = seq(0, 1, 0.1))},
               plot.axes = {axis(1, at = x.at1)
                            axis(2, at = y.at1)
                            contour(X1, X2, y.origmean, add = TRUE,  col =
                                    "black", lwd = 0.8, lty = "solid", xlim =
                                    xlim1, ylim = ylim1)
                          })

if(SavePlot)
  {
    dev.copy2eps(file = paste(ModelDescription, "_PostSurfaceMean.eps", sep = ""))
  }

dev.new()
par(ps = 10, cex = 1, cex.main = 1, cex.sub = 1, cex.axis = 1, bty = "o")
filled.contour(X1, X2, y.origsd, col = color0, xlim = xlim1, ylim = ylim1,
               levels = seq(0, 0.25, length.out = nlev+1),
               xlab = "Market2Book",
               ylab = "Profit",
               main = "Posterior surface (standard deviation)",
               key.axes = {axis(4, at = seq(0, 0.5, 0.05))},
               plot.axes = {axis(1, at = x.at1)
                            axis(2, at = y.at1)
                            points(X[, 2], X[, 4], col = "white", cex = 1.2,
                                   pch = 20, xlim = xlim1, ylim = ylim1)
                            points(X[, 2], X[, 4], col = "blue", cex = 0.6, pch = 20, xlim
                                   = xlim1, ylim = ylim1)
                            contour(X1, X2, y.origsd, add = TRUE,  col =
                                    "black", lwd = 0.8, lty = "solid", xlim =
                                    xlim1, ylim = ylim1, )
                          })

if(SavePlot)
  {
    dev.copy2eps(file = paste(ModelDescription, "_PostSurfaceSD.eps", sep = ""))
  }
