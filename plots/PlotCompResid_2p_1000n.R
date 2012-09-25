###############################################################################
### User input
###############################################################################
LOSS <- read.table("LOSS_1000n_2p.csv", header = TRUE)

NData <- nrow(LOSS)
DataUsed <- c(3, round(NData/2), NData-2)

## Index for the moving knots model used in each lap
idxM <- 1
qsmovingPlot <- 10 # the no. of free knots model to plot

## Index for the fixed knots model used each lap
idxF <- 6
qsfixedPlot <- 20 # the no. of fixed knots model to plot

## No. of models for each dataset
nRunPerDat <- 9 ## 3 fixed + 6 free

## Order by Move(5)
LOSSM <- order(LOSS[, idxM], decreasing=TRUE)[DataUsed]

## Order by Fixed(15)
LOSSF <- order(LOSS[, idxF], decreasing=TRUE)[DataUsed]

## The data are plotted in column-wise
par(mfcol = c(3, 2), mar = c(2.5, 2, 2, 1),
    oma = c(3+2, 3+2, .1+2, .1+2), las = 1, ps = 10,
    cex = 1, cex.main = 0.8, cex.sub = 1, cex.axis = 0.8, lwd = 0.5)

##########################################################################################
### Dataset (a)
##########################################################################################
## 18

iLOSS <- LOSSF[1]
load(as.character(LOSS[iLOSS, "file"]))

iData <- LOSS[iLOSS, "innerID"]

x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


## Corresponding moving knots results

j <- (iData-1)*nRunPerDat + idxM # Index
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))
meanMoving0 <- mean(uiMoving0)
sdMoving0 <- sd(uiMoving0)
uiMoving1 <- (uiMoving0-meanMoving0)/sdMoving0

## Corresponding fixed knots results
i <- (iData-1)*nRunPerDat + idxF # Index
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <- sqrt(rowSums(uiFixed^2))
uiFixed1 <-  (sqrt(rowSums(uiFixed^2))- meanMoving0)/sdMoving0


ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-60, 60)
xlim0 = c(0.8, 1.6)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main =
     "Ranking w.r.t. fixed knots model \n\n 3rd best", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)

atCurr <- seq(ylim0[1], ylim0[2], 20)
axis(2,  at = atCurr, labels = abs(atCurr), lwd = 0.5)

axis(1, lwd = 0.5)
legend("top", col = "gray", lwd = 2,
       legend = expression(paste("Fixed knots model (", q[s] == 15, ")")),
       cex = 0.8, box.lty = 0, box.lwd = 0)
legend("bottom",  col = "red", lwd = 2,
       legend = expression(paste("Free knots model (", q[s] == 5, ")", sep = "")),
       cex = 0.8, box.lty = 0, box.lwd = 0)
title(ylab = expression(paste(paste("||", tilde(bold(epsilon)), "||"), "              ",
    paste("||", tilde(bold(epsilon)), "||"))), xpd = NA)
## title(main = "Best fixed knots model", ylab = "Free knots(5)        Fixed knots(15)", xpd = NA)


##########################################################################################
### Dataset (b)
##########################################################################################
## 69

iLOSS <- LOSSF[2]
load(as.character(LOSS[iLOSS, "file"]))

iData <- LOSS[iLOSS, "innerID"]

x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


j <- (iData-1)*nRunPerDat + idxM
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- (iData-1)*nRunPerDat + idxF
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))


ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-45, 45)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main
     = "median", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)

atCurr <- seq(ylim0[1], ylim0[2], 15)
axis(2,  at = atCurr, labels = abs(atCurr), lwd = 0.5)

axis(1, lwd = 0.5)
title(ylab = expression(paste(paste("||", tilde(bold(epsilon)), "||"), "              ",
    paste("||", tilde(bold(epsilon)), "||"))), xpd = NA)

##########################################################################################
### Dataset (c)
##########################################################################################

iLOSS <- LOSSF[3]
load(as.character(LOSS[iLOSS, "file"]))
iData <- LOSS[iLOSS, "innerID"]

j <- (iData-1)*nRunPerDat + idxM
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
             OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- (iData-1)*nRunPerDat + idxF
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))


ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-30, 30)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main
     = "3rd worst", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)

atCurr <- seq(ylim0[1], ylim0[2], 10)
axis(2,  at = atCurr, labels = abs(atCurr), lwd = 0.5)

axis(1, lwd = 0.5)
title(ylab = expression(paste(paste("||", tilde(bold(epsilon)), "||"), "              ",
    paste("||", tilde(bold(epsilon)), "||"))), xpd = NA)
title(xlab = expression(paste("||", bold(x)-bar(bold(x))[o], "||")), xpd = NA)


## Ordered by Moving(5): 67, 57, 42
## [1,] "simul_10cov_5knots_2p_200n_s_2011May26-12.01.11.Rdata"
## [2,] "simul_10cov_5knots_2p_200n_s_2011May26-12.00.28.Rdata"
## [3,] "simul_10cov_5knots_2p_200n_s_2011May26-11.59.31.Rdata"

##########################################################################################
### Dataset (d)
##########################################################################################


iLOSS <- LOSSM[1]
load(as.character(LOSS[iLOSS, "file"]))
iData <- LOSS[iLOSS, "innerID"]

x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


j <- (iData-1)*nRunPerDat + idxM
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))
meanMoving0 <- mean(uiMoving0)
sdMoving0 <- sd(uiMoving0)
uiMoving1 <- (uiMoving0-meanMoving0)/sdMoving0

i <- (iData-1)*nRunPerDat + idxF
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <- sqrt(rowSums(uiFixed^2))
uiFixed1 <-  (sqrt(rowSums(uiFixed^2))- meanMoving0)/sdMoving0

ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-45, 45)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0,
     ylim = ylim0, main = "Ranking w.r.t. free knots model \n\n 3rd best", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)

atCurr <- seq(ylim0[1], ylim0[2], 15)
axis(2,  at = atCurr, labels = abs(atCurr), lwd = 0.5)

axis(1, lwd = 0.5)

##########################################################################################
### Dataset (e)
##########################################################################################

iLOSS <- LOSSM[2]
load(as.character(LOSS[iLOSS, "file"]))

iData <- LOSS[iLOSS, "innerID"]

x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


j <- (iData-1)*nRunPerDat + idxM
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- (iData-1)*nRunPerDat + idxF
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))

ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
## ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-60, 60)

plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main = "median", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)

atCurr <- seq(ylim0[1], ylim0[2], 20)
axis(2,  at = atCurr, labels = abs(atCurr), lwd = 0.5)

axis(1, lwd = 0.5)

##########################################################################################
### Dataset (f)
##########################################################################################

## 42
iLOSS <- LOSSM[3]
load(as.character(LOSS[iLOSS, "file"]))

iData <- LOSS[iLOSS, "innerID"]

x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))

j <- (iData-1)*nRunPerDat + idxM
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- (iData-1)*nRunPerDat + idxF
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))

ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-30, 30)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main
     = "3rd worst", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)

atCurr <- seq(ylim0[1], ylim0[2], 10)
axis(2,  at = atCurr, labels = abs(atCurr), lwd = 0.5)

axis(1, lwd = 0.5)
title(xlab = expression(paste("||", bold(x)-bar(bold(x))[o], "||")), xpd = NA)

## dev.copy2eps(file = "test.eps")
