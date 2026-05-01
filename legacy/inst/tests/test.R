a <- matrix(rnorm(1e3),1)
b <- matrix(rnorm(1e3), 1e3, 1e3)

system.time(a %*% b %*% t(a))
