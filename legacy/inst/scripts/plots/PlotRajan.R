rm(list = ls())
setwd("/home/fli/Dropbox/workspace/MovingKnots/Projects")
load("../R/data/Rajan.Rdata") ## Logit scale already

Y2 <- exp(Y)/(1+exp(Y)) ## The original scale

par(mfrow = c(2, 4), mar = c(4, 4, 0.2, 0.1), ps = 10, cex = 1, cex.main = 1, cex.lab = 1, cex.axis = 1)

plot(X[, 1], Y2, xlab = "", ylab = "Y", col = "blue", pch = 20, cex = 0.2)

plot(X[, 2], Y2, xlab = "", ylab = "", col = "blue", pch = 20, cex = 0.2)

plot(X[, 3], Y2, xlab = "", ylab = "", col = "blue", pch = 20, cex = 0.2)

plot(X[, 4], Y2, xlab = "", ylab = "", col = "blue", pch = 20, cex = 0.2)

plot(X[, 1], Y, xlab = "Tang", ylab = "log[Y/(1-Y)]", col = "blue", pch = 20, cex = 0.2)

plot(X[, 2], Y, xlab = "Market2Book", ylab = "", col = "blue", pch = 20, cex = 0.2)

plot(X[, 3], Y, xlab = "LogSale", ylab = "", col = "blue", pch = 20, cex = 0.2)

plot(X[, 4], Y, xlab = "Profit", ylab = "", col = "blue", pch = 20, cex = 0.2)
