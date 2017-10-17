

setwd("~/workplace/MovingKnots/R")
sourceDir("stable")

x <- matrix(rnorm(200), 100, 2)
## x2 <- matrix(rnorm(200, mean = 10), 100, 2)
## x3 <- x1 <- matrix(rnorm(200, mean = 100), 100, 2)
## x <- rbind(x1, x2, x3)

k0 <- 10
method <- "mahalanobis-eball"
RadiusShrink = 0.5

e0 <- uniroot(optim.knots.no, c(0, 10),  x = x, RadiusShrink = RadiusShrink, KnotsNo = k0,  maxiter = 1e5)
e0
e <- e0$root

##optim(0.001,optim.knots.no,, method = "BFGS")
kk <- locate.knots(x = x, e = e, 1)
kk
plot(x)
points(kk$location, col = "red", lwd = 3)
