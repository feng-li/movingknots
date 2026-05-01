#' Extract the subset of hessian matrix.
#'
#' Details are available in the paper.
#' @param hessian  NA
#' @param subset  NA
sub.hessian <- function(hessian, subset)
  {
    out <- hessian[subset, , drop = FALSE][, subset, drop = FALSE]
    return(out)
  }
