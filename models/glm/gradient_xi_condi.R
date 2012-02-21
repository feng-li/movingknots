## Gradient w.r.t xi (conditional method)
## Be aware that this will give k--by-1 matrix
## Fri Mar 26 15:39:01 CET 2010 
gradient_xi_condi <- function(B,Sigma,x,xi,l0,l,link,gradient.prior.xi)
{
  p <-dim(Sigma)[1]
  n <- dim(x)[1]

  X <- d.matrix(x,xi,l0,l)
  XB <- X%*%B ## Linear Predictor
  Sigma_1 <- solve(Sigma)
  
  mu <- Mu(X, B, link)
  residual_t<- t(Y - mu) 
  
  grad.xi.out <- -1/2*matrix(diag(p), nrow = 1) %*% (diag(p) %x% (Sigma_1 %*% residual_t) +
                                          K.X(p,p,Sigma_1%x%residual_t,t=FALSE)) %*%
                                            delta.link(X,B,link) %*% (t(B)%x% diag(n)) %*%
                                              delta.xi(x,xi,l0,l) + gradient.prior.xi
    return(t(grad.xi.out))
}
  
