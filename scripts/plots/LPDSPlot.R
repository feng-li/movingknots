lpds <- read.csv("LPDS_Rajan.csv", header = TRUE)
lpds0 <- 0
ylim0 <- c(-1390, -1220)
xlim0 <- c(0, 60)
at2 <- seq(-1400, -1220, 10)
at1 <- seq(0, 60, 5)
par(mfrow = c(1, 2),
    mar = c(0.5, 0.1, 0.5, 0.5),
    oma = c(3.5, 3.8, 0.5, 0),
    las = 1, ps = 10, cex = 1, cex.main = 1,
    cex.lab = 1, cex.axis = 1)
grid.x <- seq(0, 60, 5)
grid.y <- seq(-1400, -1220, 10)

plot(lpds$Knots, lpds$s_fixed-lpds0, type = "l",
     lwd = 1.5, col = "blue", xlim = xlim0,
     ylim = ylim0, axes=FALSE, xlab = "")
title(xlab = "No. of surface knots", ylab = "LPDS",  main =
     "Surface component model", xpd = NA)

lines(lpds$Knots, lpds$s_moving-lpds0, type = "o", col = "red", pch = 1)
## grid(col = "black")
abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5)
axis(2, at = at2, lwd = 0.5)

plot(lpds$Knots, lpds$a_fixed-lpds0, type = "l", lwd = 1.5, axes=FALSE, col = "blue", xlim =
     xlim0, ylim = ylim0, xlab = "", ylab = "", main = "")
title(xlab = "No. of additive knots",  main =
     "Additive component model", xpd = NA)

lines(lpds$Knots, lpds$a_moving-lpds0, type = "o", col = "red", pch = 1)
abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5)
legend(-30, -1370, c("Free knots", "Fixed knots"), lty = c(1, 1), pch = c(1, NA),
       col = c("red", "blue"), lwd = c(1.5, 1.5), box.lwd = 0.5, bg = "white", xpd = NA, ncol = 2)


##----------------------------------------------------------------------------------------
## Part2
##----------------------------------------------------------------------------------------
lpds <- read.csv("LPDS_Rajan.csv", header = TRUE)
lpds0 <- 0
ylim0 <- c(-1320, -1210)
maxknots <- 60
xlim0 <- c(0, maxknots)
at2 <- seq(-1320, -1210, 10)
at1 <- seq(0, maxknots, 5)
par(mfrow = c(3, 2),
    mar = c(1.5, 0.2, 0.8, 0.2),
    oma = c(6, 3.7, 0.1, 0.1), las = 1, ps = 10, cex = 1,
    cex.main = 1, cex.lab = 1, cex.axis = 1)
grid.x <- seq(0, maxknots, 5)
grid.y <- seq(-1320, -1210, 10)

## 2 additive
plot(lpds$Knots, lpds$s_fixed_plus_a_fixed_2-lpds0,  type = "l", lwd = 1.5, col = "blue", xlim =
     xlim0, ylim = ylim0, xlab = "", ylab = "",  axes=FALSE, main = "")
title(xlab = "", ylab = "LPDS", main =
     "Surface + 2 fixed additive knots", xpd = NA)
lines(lpds$Knots, lpds$s_moving_plus_a_fixed_2-lpds0, type = "o", col = "red", pch = 20)
lines(lpds$Knots, lpds$s_moving-lpds0, type = "o", col = "purple", pch = 1)

abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5, labels = FALSE)
axis(2, at = at2, lwd = 0.5)


plot(lpds$Knots, lpds$s_fixed_plus_a_moving_2-lpds0,  type = "l",   lwd = 1.5, col = "blue", xlim =
     xlim0, ylim = ylim0, xlab ="", ylab = "", axes=FALSE, main =
     "Surface + 2 free additive knots")
lines(lpds$Knots, lpds$s_moving_plus_a_moving_2-lpds0, type = "o", col = "red",
      pch = 20)
lines(lpds$Knots, lpds$s_moving-lpds0, type = "o", col = "purple", pch = 1)

abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5, labels = FALSE)
## legend(-maxknots, -1280, c("Free surface knots", "Fixed surface knots"), lty = c(1, 1), pch = c(20, NA),
##        col = c("red", "blue"), box.lwd = 0.5, xpd = NA, ncol = 2)

## legend(10, -13maxknots, c("Moving surface knots", "Fixed surface knots"), lty = c(1, 1), pch = c(1, NA),
##        col = c("red", "blue"), box.lwd = 0, bg = "white", box.lty = 0)


## 4 additive
plot(lpds$Knots, lpds$s_fixed_plus_a_fixed_4-lpds0,  type = "l", lwd = 1.5, col = "blue", xlim =
     xlim0, ylim = ylim0, xlab = "", ylab = "LPDS", axes=FALSE, main = "")
title(xlab = "", ylab = "LPDS", main =
     "Surface + 4 fixed additive knots", xpd = NA)
lines(lpds$Knots, lpds$s_moving_plus_a_fixed_4-lpds0, type = "o", col = "red", pch = 20)
lines(lpds$Knots, lpds$s_moving-lpds0, type = "o", col = "purple", pch = 1)

abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5, labels = FALSE)
axis(2, at = at2, lwd = 0.5)
## legend(10, -1340, c("Moving surface knots", "Fixed surface knots"), lty = c(1, 1), pch = c(1, NA),
##        col = c("red", "blue"), box.lwd = 0, bg = "white", box.lty = 0)


plot(lpds$Knots, lpds$s_fixed_plus_a_moving_4-lpds0,  type = "l",   lwd = 1.5,
     col = "blue", xlim = xlim0, ylim = ylim0, xlab = "", ylab = "", axes=FALSE, main =
     "Surface + 4 free additive knots")
lines(lpds$Knots, lpds$s_moving_plus_a_moving_4-lpds0, type = "o", col = "red", pch = 20)
lines(lpds$Knots, lpds$s_moving-lpds0, type = "o", col = "purple", pch = 1)

abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5, labels = FALSE)
## legend(10, -1340, c("Moving surface knots", "Fixed surface knots"), lty = c(1, 1), pch = c(1, NA),
##        col = c("red", "blue"), box.lwd = 0, bg = "white", box.lty = 0)

## 8 additive
plot(lpds$Knots, lpds$s_fixed_plus_a_fixed_8-lpds0,  type = "l", lwd = 1.5, col = "blue", xlim =
     xlim0, ylim = ylim0,  axes=FALSE, xlab = "", ylab = "", main = "")
title(xpd = NA,  xlab = "No. of surface knots", ylab = "LPDS", main =
     "Surface + 8 fixed additive knots")
lines(lpds$Knots, lpds$s_moving_plus_a_fixed_8-lpds0, type = "o", col = "red", pch = 20)
lines(lpds$Knots, lpds$s_moving-lpds0, type = "o", col = "purple", pch = 1)

abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5, xpd = NA)
axis(2, at = at2, lwd = 0.5, xpd = NA)
## legend(10, -1340, c("Moving surface knots", "Fixed surface knots"), lty = c(1, 1), pch = c(1, NA),
##        col = c("red", "blue"), box.lwd = 0, bg = "white", box.lty = 0)


plot(lpds$Knots, lpds$s_fixed_plus_a_moving_8-lpds0,  type = "l",  lwd = 1.5, col = "blue", xlim =
     xlim0, ylim = ylim0, xlab = "", ylab = "",  axes=FALSE, main = "")
title(xpd = NA,  xlab = "No. of surface knots", ylab = "", main =
     "Surface + 8 free additive knots")
lines(lpds$Knots, lpds$s_moving_plus_a_moving_8-lpds0, type = "o", col = "red", pch = 20)
lines(lpds$Knots, lpds$s_moving-lpds0, type = "o", col = "purple", pch = 1)

abline(h = grid.y, v = grid.x, col = "black", lty="dotted", lwd = 0.5)
axis(1, at = at1, lwd = 0.5, xpd = NA)
## legend(20, -1290, c("Moving surface knots", "Fixed surface knots"), lty = c(1, 1), pch = c(1, NA),
##        col = c("red", "blue"), box.lwd = 0, bg = "white", box.lty = 0)
legend(-80, -1355,
       c("Free surface knots + additive knots",
         "Fixed surface knots + additive knots",
         "Benchmark: free surface knots only"),
       lty = c(1, 1, 1), pch = c(20, NA, 1),
       col = c("red", "blue", "purple"),
       lwd = c(1.5, 1.5, 1.5), box.lwd = 0.5,
       xpd = NA, ncol = 3, bg = "white")
