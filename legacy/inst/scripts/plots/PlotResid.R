#' Residual check for the moving knots model
#'
#' Details.
#' @name
#' @title
#' @param OUT.Params
#' @return
#' @references
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @note First version: Tue Dec 07 15:18:26 CET 2010;
#'       Current:       Tue Dec 07 15:18:35 CET 2010.
EvalResid <- function(...)
{

  Resid <- array(NA, c(n, p, nIter, nCross))

  ## Make the design matrix
  X <- d.matrix(...)

  ## Get Slopes
  B <- B

  ## Calculate the residuals
  Y.hat <- X %*% B
  E.hat <- Y - Y.hat



  ## Normalize the residual



  ## Plot the QQ plot for residual
  if(qqplot  == TRUE)
    {


    }

  ## Plot the normalized residual against the Y.hat
  if(normplot  == TRUE)
    {


    }


  out <- list(Resid.orig <- E.hat, Resid.normalized)
  return(out)

}
