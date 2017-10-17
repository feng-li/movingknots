p <- 3
q <- 15

B <- matrix(rnorm(p*q),  q)
mu4B <- matrix(rnorm(p*q),  q)

idx4B <- matrix(1:(p*q), q)

idx4b <- foldMat(idx4B, nfold = 3, byrow = TRUE)

beta <- matrix(B)
mu4beta <- matrix(mu4B)

b <- matrix(beta[idx4b])
mu4b <- matrix(mu4beta[idx4b])

Sigma4beta0 <- matrix(rnorm((p*q)^2), p*q)
Sigma4beta <- (Sigma4beta0 + t(Sigma4beta0))/2

Sigma4b <- Sigma4beta[idx4b, ][, idx4b ]

t(beta-mu4beta) %*% Sigma4beta %*% (beta-mu4beta)
t(b-mu4b) %*% Sigma4b %*% (b-mu4b)
