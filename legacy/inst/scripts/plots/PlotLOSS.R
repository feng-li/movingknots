
## the data
LOSS <- read.table("Simul_LOSS.txt", header = FALSE)

lratio <- matrix(NA, dim(LOSS)[1], dim(LOSS)[2]/2)
for(i in 1:(dim(LOSS)[2]/2))
  {
    lratio[, i] <- LOSS[, i*2]/LOSS[, (i*2-1)] 
  }


par(mfrow = c(2, 2), mar = c(2, 4, 2, 0.5))

ylab <- expression(paste("log(", LOSS[F]/LOSS[f], ")"), names = name, ylim = ylim)
name <- c(2, 5, 10)
ylim <- c(-6, 12)

##  P = 1, n = 200
boxplot(log(lratio[, 1:3]), main = "p = 1, n = 200", ylab = ylab, names = name, ylim = ylim)
abline(h = 0, v = 0, lty = "dotted", col = "red")

##  P = 2, n = 200
boxplot(log(lratio[, 4:6]), main = "p = 2, n = 200", names = name, ylim = ylim)
abline(h = 0, v = 0, lty = "dotted", col = "red")

##  P = 1, n = 1000
boxplot(log(lratio[, 7:9]), ylab = ylab, main = "p = 1, n = 1000", names = name, ylim = ylim)
abline(h = 0, v = 0, lty = "dotted", col = "red")

##  P = 1, n = 1000
boxplot(log(lratio[, 10:12]), main = "p = 2, n = 1000", names = name, ylim = ylim)
abline(h = 0, v = 0, lty = "dotted", col = "red")
