## Gradient w.r.t. vecB
## Be aware that this will give a q--by--1 matrix
## Fri Mar 26 13:38:51 CET 2010

gradient_vecB <- function(B,Sigma,x,xi,l0,l,link,gradient.prior.vecB)
  {

    p <-dim(Sigma)[1]

    X <- d.matrix(x,xi,l0,l)
    XB <- X%*%B ## Linear Predictor
    Sigma_1 <- solve(Sigma)

    mu <- Mu(X, B, link)
    residual_t<- t(Y - mu) 

    grad.vecB.out <- -1/2*matrix(diag(p), nrow = 1) %*% (diag(p) %x% (Sigma_1 %*% residual_t) +
                           K.X(p,p,Sigma_1%x%residual_t,t=FALSE)) %*%
                             delta.link(X,B,link) %*% (diag(p)%x% X) + gradient.prior.vecB
    return(t(grad.vecB.out))
  }
