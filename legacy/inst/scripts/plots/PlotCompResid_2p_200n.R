setwd("/home/fli/Dropbox/workspace/MovingKnots/Projects/Results/")

LOSS <- read.table("LOSS_200n_2p.csv", header = TRUE)

LOSSM.5 <- LOSS[order(LOSS[, 1]) , , drop = FALSE]
LOSSF.15 <- LOSS[order(LOSS[, 6]) , , drop = FALSE]

par(mfcol = c(3, 2), mar = c(2.5, 2, 2, 1), oma = c(3+2, 3+2, .1+2, .1+2), las = 1, ps = 10, cex = 1, cex.main = 0.8, cex.sub = 1, cex.axis = 0.8, lwd = 0.5)

### Order by Fixed(15): 18, 69, 63
## [1,] "simul_10cov_5knots_2p_200n_s_2011May26-11.57.23.Rdata"
## [2,] "simul_10cov_5knots_2p_200n_s_2011May26-12.01.11.Rdata"
## [3,] "simul_10cov_5knots_2p_200n_s_2011May26-11.55.47.Rdata"
setwd("/home/fli/myData/ResearchPapers/MovingKnots/running/Projects/Results")


##########################################################################################
### Dataset (a) 
##########################################################################################
## 18
load("simul_10cov_5knots_2p_200n_s_2011May26-11.57.23.Rdata")

iData <- 3
x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


j <- 19
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))
meanMoving0 <- mean(uiMoving0)
sdMoving0 <- sd(uiMoving0)
uiMoving1 <- (uiMoving0-meanMoving0)/sdMoving0

i <- 24
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <- sqrt(rowSums(uiFixed^2))
uiFixed1 <-  (sqrt(rowSums(uiFixed^2))- meanMoving0)/sdMoving0


ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-15, 15)
xlim0 = c(0.8, 1.6)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main =
     "Ranking w.r.t. fixed knots model \n\n 3rd best", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)
axis(2,  at = seq(-15, 15, 5), labels = c("15", "10", "5", "0", "5", "10", "15"), lwd = 0.5)
axis(1, lwd = 0.5)
legend("top", col = "gray", lwd = 2, legend = expression(paste("Fixed knots model ", (q[s] == 15))), cex = 0.8, box.lty = 0, box.lwd = 0)
legend("bottom",  col = "red", lwd = 2, legend = expression(paste("Free knots model ", (q[s] == 5))), cex = 0.8,
       box.lty = 0, box.lwd = 0)
title(ylab = expression(paste(paste("||", tilde(bold(epsilon)), "||"), "              ", 
    paste("||", tilde(bold(epsilon)), "||"))), xpd = NA) 
## title(main = "Best fixed knots model", ylab = "Free knots(5)        Fixed knots(15)", xpd = NA)


##########################################################################################
### Dataset (b) 
##########################################################################################
## 69
load("simul_10cov_5knots_2p_200n_s_2011May26-12.01.11.Rdata")

iData <- 3
x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


j <- 19
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- 24
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))


ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-30, 30)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main
     = "medium", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)
axis(2,  at = seq(-30, 30, 10), labels = c("30", "20", "10", "0",  "10", "20", "30") , lwd = 0.5)
axis(1, lwd = 0.5)
title(ylab = expression(paste(paste("||", tilde(bold(epsilon)), "||"), "              ", 
    paste("||", tilde(bold(epsilon)), "||"))), xpd = NA) 

##########################################################################################
### Dataset (c) 
##########################################################################################

## 63
load("simul_10cov_5knots_2p_200n_s_2011May26-11.55.47.Rdata")
j <- 19
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- 24
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))


ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-100, 100)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main
     = "3rd worst", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)
axis(2, at = seq(-100, 100, 25), labels = c("100", "75", "50","25", "0", "25", "50", "75", "100"), lwd = 0.5)
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

## 67
load("simul_10cov_5knots_2p_200n_s_2011May26-12.01.11.Rdata")

iData <- 3
x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


j <- 1
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))
meanMoving0 <- mean(uiMoving0)
sdMoving0 <- sd(uiMoving0)
uiMoving1 <- (uiMoving0-meanMoving0)/sdMoving0

i <- 6
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <- sqrt(rowSums(uiFixed^2))
uiFixed1 <-  (sqrt(rowSums(uiFixed^2))- meanMoving0)/sdMoving0

ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-100, 100)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main = "Ranking w.r.t. free knots model \n\n 3rd best", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)
axis(2, at = seq(-100, 100, 25), labels = c("100", "75", "50","25", "0", "25", "50", "75", "100"), lwd = 0.5)
axis(1, lwd = 0.5)

##########################################################################################
### Dataset (e) 
##########################################################################################

## 57
load("simul_10cov_5knots_2p_200n_s_2011May26-12.00.28.Rdata")
iData <- 3
x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))


j <- 19
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- 24
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))

ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
## ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-40, 40)

plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main = "medium", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)
axis(2,  at = seq(-40, 40, 10), labels = c("40", "30", "20", "10", "0",  "10", "20", "30", "40"), lwd = 0.5)
axis(1, lwd = 0.5)

##########################################################################################
### Dataset (f) 
##########################################################################################

## 42
load("simul_10cov_5knots_2p_200n_s_2011May26-11.59.31.Rdata")
iData <- 3
x.testing <- OUT.Data.gen[[iData]]$x.testing
x.training <- OUT.Data.gen[[iData]]$x
x.testing2cntr <- rdist(x.testing, matrix(colMeans(x.training), 1))

j <- 19
uiMoving <- (OUT.Data.pred[[j]]$details$SurfaceMean.pred -
       OUT.Data.pred[[j]]$details$SurfaceMean.true)
uiMoving0 <-  -sqrt(rowSums(uiMoving^2))

i <- 24
uiFixed <- (OUT.Data.pred[[i]]$details$SurfaceMean.pred -
       OUT.Data.pred[[i]]$details$SurfaceMean.true)
uiFixed0 <-  sqrt(rowSums(uiFixed^2))

ymax0 <- max(uiFixed0, abs(uiMoving0))
ymin0 <- -ymax0
##ylim0 <- c(ymin0, ymax0)
ylim0 <- c(-20, 20)
plot(x.testing2cntr, uiFixed0, type = "h", col = "gray", xlim = xlim0, ylim = ylim0, main
     = "3rd worst", axes = FALSE, lwd = 2)
points(x.testing2cntr, uiMoving0, type = "h", col = "red", lwd = 2)
grid2(y.at = 0, lty = "solid", lwd = 0.5)
axis(2,  at = seq(-20, 20, 5), labels = c("20", "15", "10", "5", "0",  "5",  "10", "15", "20"), lwd = 0.5)
axis(1, lwd = 0.5)
title(xlab = expression(paste("||", bold(x)-bar(bold(x))[o], "||")), xpd = NA)
