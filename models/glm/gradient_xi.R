## gradient w.r.t. xi
gradient_xi <- function(Y,x,xi,l0,l,n0,S0,B,ka,gradient.prior.xi)
  {

    X <- d.matrix(x,xi,l0,l)
    n <- dim(x)[1]
    p <- dim(Y)[2]
    q <- dim(X)[2]
    
    P <- t(X)%*%X
    P_1 <- solve(P)
    
    XP_1<- X%*%P # 6
    B_tilde <- 1/(1+ka)*P_1%*%(t(X)%*%Y+ka*P%*%M)
    Q_YXB <- t(Y-X%*%B_tilde) # 2
    S_tilde <- Q_YXB%*%t(Q_YXB)/n
    
    B_tilde_M <- B_tilde-M # 3

    S_tilde_S0 <- n0*S0+n*S_tilde+ka*t(B_tilde_M)%*%P%*%B_tilde_M # 
    
    grad.tmp0 <- t(delta.xi(x,xi,l0,l))
    grad.tmp1 <- matrix(XP_1,ncol=1) # vec
    grad.tmp2 <- matrix(solve(S_tilde_S0),ncol=1) # vec

    Q_MB <- t(ka*M-(ka+1)*B_tilde) # 1
    Q_YXMXB <- t(Y+ka*X%*%M-(ka+1)*X%*%B_tilde) # 4
    Q_XYPM <- t(t(X)%*%Y-P%*%M) # 5


    Q.tmp1.0 <-  Q_MB%x%(Q_YXB%*%XP_1%*%t(X))
    Q.tmp1 <- 1/(ka+1)*(Q.tmp1.0 + K.X(p,p,Q.tmp1.0,t=FALSE))

    Q.tmp2.0 <- Q_YXMXB%x%(Q_YXB%*%XP_1)
    Q.tmp2 <- 1/(ka+1)*(K.X(n,q,(Q.tmp2.0+K.X(p,p,Q.tmp2.0,t=FALSE)),t=TRUE))

    Q.tmp3.0 <- (t(Y)-t(X%*%M))%x%t(B_tilde_M)
    Q.tmp3.1 <- t(M)%x%(t(X%*%B_tilde_M))
    Q.tmp3 <- ka/(ka+1)*(K.X(n,q,Q.tmp3.0,t=TRUE)-Q.tmp3.1)

    Q.tmp4.0 <- Q_MB%x%(Q_XYPM%*%t(XP_1))
    Q.tmp4 <- ka/(ka+1)^2*K.X(p,p,Q.tmp4.0,t=FALSE)
    
    Q.tmp5.0 <- (Q_XYPM%*%P_1)%x%Q_YXMXB
    Q.tmp5 <- ka/(ka+1)^2*Q.tmp5.0

    Q.tmp6.0 <- t(B_tilde)%x%Q_YXB
    Q.tmp6 <- Q.tmp6.0+K.X(p,p,Q.tmp6.0,t=FALSE)

    grad.tmpQ <- Q.tmp1 +Q.tmp2+Q.tmp3+Q.tmp4+Q.tmp5+Q.tmp6
    gradient.marginal.xi <- - grad.tmp0%*%grad.tmp1 -
                             (n+n0)/2*grad.tmp0%*%t(grad.tmpQ)%*%grad.tmp2

    gradient.xi <- gradient.marginal.xi + gradient.prior.xi

    return(gradient.xi)
  }
