## Plot for loss function with nonlinear factor
LOSS <- read.table("LOSS_200n_1p.csv", header = TRUE)
NL <- sort(LOSS[, 12])
NLorder <- order(LOSS[, 12])
NLlevel <- c(0, 20, 40, 100)

NLclass <- list()
idx <- list()
for(i in 1:(length(NLlevel)-1))
  {
    idx[[i]] <- (NL >= NLlevel[i] & NL < NLlevel[i+1])
    NLclass[[i]] <- NL[idx[[i]]]
  }

col0 <- "lightgray"
## par(mfrow = c(3, length(NLlevel)-1), mar = c(2, 4, 1, 0.1), ps = 10, cex = 1, cex.axis = 1, cex.main = 1, lwd = 0.7)
par(mfrow = c(3, length(NLlevel)-1), mar = c(0.1, 0.5, 1, 0.1), oma = c(2, 1.5, 0.1, 0), ps = 10, cex = 1, cex.axis = 1, cex.main = 1, lwd = 0.7)

yat <- seq(-4, 8, 2)
ylim <- c(-4, 8)
losscur <- log(LOSS[, 6:11]/LOSS[, 1])
boxplot(losscur[idx[[1]], ], ylim = ylim, xaxt = "n", main = "low nonlinearity", ylab = "log[F/f5]", col = col0,  xpd = NA)
grid2(y.at = yat)
boxplot(losscur[idx[[2]], ], ylim = ylim, xaxt = "n", yaxt = "n", main = "median nonlinearity", col = col0)
grid2(y.at = yat)
boxplot(losscur[idx[[3]], ], ylim = ylim, xaxt = "n", yaxt = "n", main = "high nonlinearity", col = col0)
grid2(y.at = yat)
##boxplot(losscur[idx[[4]], ], ylim = ylim)

ylim <- c(-4, 8)
losscur <- log(LOSS[, 6:11]/LOSS[, 2])
boxplot(losscur[idx[[1]], ], ylim = ylim, xaxt = "n", ylab = "log[F/f10]", col = col0)
grid2(y.at = yat)
boxplot(losscur[idx[[2]], ], ylim = ylim, xaxt = "n", yaxt = "n", col = col0)
grid2(y.at = yat)
boxplot(losscur[idx[[3]], ], ylim = ylim, xaxt = "n", yaxt = "n", col = col0)
grid2(y.at = yat)

name0 <- c(5, 10, 15, 20, 25, 50)
ylim <- c(-4, 8)
losscur <- log(LOSS[, 6:11]/LOSS[, 3])
boxplot(losscur[idx[[1]], ], ylim = ylim, names = name0,  ylab = "log[F/f15]", col = col0)
grid2(y.at = yat)
boxplot(losscur[idx[[2]], ], ylim = ylim, yaxt = "n" , names = name0, col = col0)
grid2(y.at = yat)
boxplot(losscur[idx[[3]], ], ylim = ylim, yaxt = "n", names = name0, col = col0)
grid2(y.at = yat)
