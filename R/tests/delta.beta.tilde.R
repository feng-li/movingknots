##' Gradient for beta_tilde w.r.t. knots
##' 
##' <details>
##' @title 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Thu Feb 10 17:48:19 CET 2011;
##'       Current:       Thu Feb 10 17:48:27 CET 2011.
delta.beta.tilde <- function()
  {
    ## The first part should be down.
    ## Does not depend on the prior settings
    
    
    ## The second part depends on the prior settings
    ## check if X'X or not
    

    ## Third part. does not depend on prior

    ## Fourth part. Depends on prior
    Mat1 <- Mat.x.DvecA.k.P_stp1(Mat, A, p, q, q_i) 
    out <- Mat.x.DvecA.k.P_stp2(Mat1, X_i, delta_i, p, q, q_i)

    
    
    
    
    return(out)
  }
