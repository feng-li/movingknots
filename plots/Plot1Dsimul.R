x <- OUT.Data.gen[[1]]$x
Y <- OUT.Data.gen[[1]]$Y
plot(x, Y, ylim = c(-6, 4))

x.test <- OUT.Data.testing[[1]]
Y.true <- OUT.Data.pred[[1]]$details$SurfaceMean.true
Y.test1 <- OUT.Data.pred[[1]]$details$SurfaceMean.pred
Y.test3 <- OUT.Data.pred[[3]]$details$SurfaceMean.pred

points(sort(x.test), Y.true[order(x.test)], type = "l", col = "red")

points(sort(x.test), Y.test1[order(x.test)], type = "l", col = "blue", lwd = 2, lty = "dotted")
points(sort(x.test), Y.test3[order(x.test)], type = "l", col = "blue", lwd = 2)


x.test <- OUT.Data.testing[[1]]
Y.test2 <- OUT.Data.pred[[2]]$details$SurfaceMean.pred
Y.test4 <- OUT.Data.pred[[4]]$details$SurfaceMean.pred


points(sort(x.test), Y.test2[order(x.test)], type = "l", col = "black", lwd = 2, lty = "dotted")
points(sort(x.test), Y.test4[order(x.test)], type = "l", col = "black", lwd = 2)

legend("top", lwd = 2, col = c("red", "blue", "blue", "black", "black"), legend =
       c("True surface", "5 Moving,  LOSS = 0.01174553", "5 Fixed, LOSS = 0.05019515", "10 Moving, LOSS = 0.001565284", "10 Fixed, LOSS = 0.008570262"), lty = c("solid", "dotted", "solid", "dotted", "solid"))

points(OUT.Data.gen[[1]]$knots$thinplate.s, rep(-1.5, 5), pch = 19, col = "red")



OUT.Params <- OUT.Data.pred[[1]]$details$OUT.Params
OUT.Params.mode <- lapply(OUT.Params,
                          function(x) apply(x[, , (1000+1):nIter,drop=FALSE, ],
                                            c(1, 2, 4), mean))
points(OUT.Params.mode$knots, rep(-2, 5), pch = 15, col = "blue")

OUT.Params <- OUT.Data.pred[[3]]$details$OUT.Params
OUT.Params.mode <- lapply(OUT.Params,
                          function(x) apply(x[, , (1000+1):nIter,drop=FALSE, ],
                                            c(1, 2, 4), mean))
points(OUT.Params.mode$knots, rep(-2.5, 5), pch = 0, col = "blue")


OUT.Params <- OUT.Data.pred[[2]]$details$OUT.Params
OUT.Params.mode <- lapply(OUT.Params,
                          function(x) apply(x[, , (1000+1):nIter,drop=FALSE, ],
                                            c(1, 2, 4), mean))
points(OUT.Params.mode$knots, rep(-3, 10), pch = 15, col = "black")

OUT.Params <- OUT.Data.pred[[4]]$details$OUT.Params
OUT.Params.mode <- lapply(OUT.Params,
                          function(x) apply(x[, , (1000+1):nIter,drop=FALSE, ],
                                            c(1, 2, 4), mean))
points(OUT.Params.mode$knots, rep(-3.5, 10), pch = 0, col = "black")


legend("bottom", lwd = 2, col = c("red", "blue", "blue", "black", "black"), legend =
       c("True knots", "Posterior mean of 5 Moving knots", "Location of 5 fixed knots",
         "Posterior mean of 10 moving knots", "Location of 10 fixed knots"), pch = c(19, 15, 0, 15, 0))
