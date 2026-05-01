Sigma <- matrix(c(1, 0, 0, 2), 2)
nu <- 10

IW <- array(0, c(2, 2, 1000))

for(i in 1:1000)
  {IW[, , i] <- rwishart(nu = nu, V = solve(Sigma*nu))$IW}

apply(IW, c(1, 2), mean)

Sigma*nu/(nu-3)
