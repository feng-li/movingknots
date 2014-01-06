## Plot for loss function with nonlinear factor
## LOSS <- read.table("LOSS_200n_2p.csv", header = TRUE)
## file <- "/home/fli/running/Projects/Results_Plots/LOSS_200n_2p.csv"
file <- "/home/fli/running/simul1000/Results/LOSS_1000n_2p.csv"

## ylim <- c(-4, 8)
ylim <- c(-1, 5)

yat <- seq(-4, 10, 1)


LOSS <- read.table(file, header = TRUE)

NL <- sort(LOSS[, 10])
NLorder <- order(LOSS[, 10])

NLprob <- seq(0, 1, length.out = 6)
NLprob <- NLprob[-c(1, length(NLprob))]

NLlevel <- quantile(NL, probs = NLprob)

## NLlevel <- c(3.0, 3.25, 3.50, 4.0)

NLclass <- list()
idx <- list()
for(i in 1:(length(NLlevel)-1))
  {
    idx[[i]] <- (NL >= NLlevel[i] & NL < NLlevel[i+1])
    NLclass[[i]] <- NL[idx[[i]]]
  }

col0 <- "lightgray"
## par(mfrow = c(3, length(NLlevel)-1), mar = c(2, 4, 1, 0.1), ps = 10, cex = 1, cex.axis = 1, cex.main = 1, lwd = 0.7)
par(mfrow = c(3, length(NLlevel)-1), mar = c(0.1, 0.5, 1, 0.1), oma = c(4, 5.5, 0.1, 0),
    ps = 10, cex = 1, cex.axis = 1, cex.main = 1, lwd = 0.7)

ygrid <- yat[yat != 0]
xat <- 1:6
losscur <- log(LOSS[, 4:9]/LOSS[, 1])
boxplot(losscur[idx[[1]], ], ylim = ylim, xaxt = "n", main = "", ylab = "", col = col0,
        axes = FALSE)
##axis(1, xat)
axis(2, yat)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
title(main = "Low nonlinearity", ylab = expression(log*frac(Loss(tilde(f)[q[s]])[fixed], Loss(tilde(f)[q[s] == 5])[free])), xpd = NA)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
boxplot(losscur[idx[[2]], ], ylim = ylim, xaxt = "n", yaxt = "n", main = "Median nonlinearity", col = col0, axes = FALSE)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
boxplot(losscur[idx[[3]], ], ylim = ylim, xaxt = "n", yaxt = "n", main = "High nonlinearity", col = col0, axes = FALSE)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
##boxplot(losscur[idx[[4]], ], ylim = ylim)

## ylim <- c(-1, 5)
losscur <- log(LOSS[, 4:9]/LOSS[, 2])
boxplot(losscur[idx[[1]], ], ylim = ylim, xaxt = "n", ylab = "", col = col0, axes = FALSE)
axis(2, yat)
title(ylab = expression(log*frac(Loss(tilde(f)[q[s]])[fixed], Loss(tilde(f)[q[s] == 10])[free])), xpd = NA)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
boxplot(losscur[idx[[2]], ], ylim = ylim, xaxt = "n", yaxt = "n", col = col0, axes = FALSE)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
boxplot(losscur[idx[[3]], ], ylim = ylim, xaxt = "n", yaxt = "n", col = col0, axes = FALSE)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")

name0 <- c(5, 10, 15, 20, 25, 50)
## ylim <- c(-1, 5)
losscur <- log(LOSS[, 4:9]/LOSS[, 3])
boxplot(losscur[idx[[1]], ], ylim = ylim, names = name0,  xlab = "", ylab = "", col =
        col0, axes = FALSE)
axis(1, xat, labels = name0)
axis(2, yat)
title(xlab = expression(paste("No. fixed knots ", (q[s]))), ylab =
      expression(log*frac(Loss(tilde(f)[q[s]])[fixed], Loss(tilde(f)[q[s] == 15])[free])), xpd = NA)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
boxplot(losscur[idx[[2]], ], ylim = ylim, yaxt = "n" , names = name0, col = col0, axes = FALSE)
axis(1, xat, labels = name0)
title(xlab = expression(paste("No. fixed knots ", (q[s]))), xpd = NA)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
boxplot(losscur[idx[[3]], ], ylim = ylim, yaxt = "n", names = name0, col = col0, axes = FALSE)
axis(1, xat, labels = name0)
title(xlab = expression(paste("No. fixed knots", (q[s]))), xpd = NA)
grid2(y.at = ygrid)
grid2(y.at = 0, col = "red", lwd = 1.5, lty = "dashed")
