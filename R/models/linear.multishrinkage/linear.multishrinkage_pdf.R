##' The PDF/CDF in the linear model 
##'
##' Details.
##' @name 
##' @title 
##' @param Params 
##' @param x0 
##' @param x 
##' @param splineArgs 
##' @param CDF 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
linear_pdf <- function(Params, x0, x, splineArgs, CDF = FALSE)
{
  xi <- Params[["xi"]]
  B <- Params[["B"]]
  Sigma <- Params[["Sigma"]]
  X <- d.matrix(x = x, xi = xi, args = splineArgs)

  pred.mean <- X%*%B
  pred.sd <- sqrt(Sigma) 
  
  
  if(CDF == FALSE) # PDF 
    { density <- dmvnorm()}
  else # CDF
    {density <- pmvnorm()}
    
  out <- list(density = density, pred.mean = pred.mean, pred.sd = pred.sd)
  return(out)
}
