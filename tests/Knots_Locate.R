Knots_Locate <- function(x,method,K_Knots,P_Spline)
  {
    Length_x <- length(x)
    if (method == "ES") # Equal Spaces
    {
      K_loation <- seq(min(x),max(x),length.out=K_Knots+2)
    }

    Design_Matrix <- matrix(0,nrow=Length_x,ncol=P_Spline+K_Knots+1)
    for(i in 0:P_Spline)
      {
        Design_Matrix[,i+1] <- x^i
      }
    for(i in 1:K_Knots)
      {
        Design_Matrix[,P_Spline+1+i] <- ((x>=K_loation[i+1])*x)^P_Spline
      }
    Design_Matrix
  }
