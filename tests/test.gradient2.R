## This is the hard code for testing gradient

Delta.X4knots <- matrix(0, n*q, length(knots.mat))
Delta.X4knots[(n*q.i[1]+1):(n*q), ] <- delta.knots[[1]]

aa <- Sigma4beta.tilde %*% ((t(Y.Sigma.inv)%x%diag(q)) %*%K(n, q)) %*% Delta.X4knots

cc <- ((matrix(XT.Y.Sigma.inv, 1)%*% Sigma4beta.tilde) %x% Sigma4beta.tilde)
a <- proc.time()
bb = cc %*%
  (diag(p)%x%K(q, p)%x%diag(q))%*%(matrix(Sigma.inv)%x%diag(q^2))%*%
  (diag(q^2)+K(q, q))%*%
  (diag(q)%x%t(X))%*%Delta.X4knots
print(proc.time()-a)

all.equal(bb, gradObs.tmp2.2)
