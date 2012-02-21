a <- matrix(rnorm(100), 10)
b <- matrix(rnorm(81), 9)
c <- matrix(rnorm(64), 8)

d <- matrix(0, 27, 27)
d[1:10, 1:10] <- a
d[11:19, 11:19] <- b
d[20:27, 20:27] <- c


prod(c(det(a), det(b), det(c)))
det(d)
