##' gradient for \sum q_i log|K_i|
##'
##' Details
##' @param q.i NA
##' @param diag.K NA
##' @param args "list" args$subset: the subsets of the diagonal K matrix.
##' @return "matrix" 1-by-
##' @references NA
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @export
delta.sumqlogdetK <- function(q.i, diag.K)
  {
    p <- length(diag.K)/length(q.i)
    q.seq <- rep(q.i, each = p)
    ## subset <- args$subset
    out0 <- q.seq/diag.K # the gradient
    out <- matrix(out0, 1) # format to a matrix
    return(out)
  }
