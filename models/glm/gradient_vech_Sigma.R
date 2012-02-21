## The gradient with respect to vech Sigma
## Mon Mar 29 09:17:32 CEST 2010
## p--by--1 matrix

grad_vech_Sigma <- function(B,Sigma,x,xi,l0,l,link,gradient.prior.Sigma)
{
  p <-dim(Sigma)[1]
  n <- dim(x)[1]

  X <- d.matrix(x,xi,l0,l)
  XB <- X%*%B ## Linear Predictor
  Sigma_1 <- solve(Sigma)
  
  mu <- Mu(X, B, link)
  residual<- Y - mu 

  grad.Sigma <- -n*p/2*log(2*pi)*Sigma_1 + 1/2*Sigma_1 %*% t(residual) %*% residual %*% Sigma_1 + gradient.prior.Sigma
  grad.vech.Sigma.out <- matrix(grad.Sigma[!upper.tri(grad.Sigma)],ncol=1)

  return(grad.vech.Sigma.out)
}
  
