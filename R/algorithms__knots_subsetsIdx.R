#' Find the locations of subsets in the knots matrix
#'
#' Details see the paper.
#' @param knots.subsets "list"
#' @param k "integer"
#'         No. of knots in the knots matrix.
#' @param m "integer"
#'         Dimension of knots in the knots matrix.
#' @return "list"
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
knots_subsetsIdx <- function(knots.subsets, k, m)
  {
    knots.matrix.idx <- matrix(1:(k*m), k, m)
    list.out <- list()
    for(i in 1:length(knots.subsets))
      {
        list.out[[i]] <- as.numeric(t(knots.matrix.idx[as.vector(knots.subsets[[i]]), ])) # no dim attribute
      }
    return(list.out)
  }
