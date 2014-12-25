##' Matrix pre-multiply the gradient of the vec matrices
##'
##' <details>
##' @title 
##' @param Mat 
##' @param P 
##' @param args 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
##' not in use
Mat.x.delta.vecP <- function(Mat, P, args)
  {
    P.type <- args$P.type
    ## q1 <- c(1, q.i) #for index convenient when type is "X'X"
    out <- list(NULL) #empty list for storage.

    stop("error")
    for(i in 1:length(P.type))
      {
        ## Diagonal matrix
        if(P.type[i]  == "identity") 
          {
           out[[i]] <- 0 # FIXME: How many zeros?
          }
        else if((P.type[i]  == "X'X"))
          {
            
            ## idx <- sum(q1[1:i]):sum(q.i[1:i])
            ## X4P <- X[, idx] # X matrix for P
            out[[i]] <- crossprod(X4P)
          }
        else
          {
            stop("Wrong type for the P matrices!")
          }
      }
    return(out)

  }
