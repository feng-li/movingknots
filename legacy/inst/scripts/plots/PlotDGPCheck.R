plot(get("x", envir = .GlobalEnv), get("Y", envir = .GlobalEnv))
data1 <- cbind(get("x", envir = .GlobalEnv), Data.gen[["SurfaceMean"]])
data1 <- data1[order(data1[, 1]), ]

points(data1, col = "red", type = "l")

y0 <- apply(SurfaceMean, c(1), mean)
data2 <- cbind(x, y0)
data2 <- data2[order(data2[, 1]), ]
points(data2, col = "blue", type = "l", lwd = 2)


y0 <- apply(SurfaceMean, c(1), mean)
data2 <- cbind(x, y0)
data2 <- data2[order(data2[, 1]), ]
points(data2, col = "black", type = "l", lwd = 2, lty = "dashed")

##
sum((y0-Data.gen[["SurfaceMean"]])^2)
[1] 58.21999

