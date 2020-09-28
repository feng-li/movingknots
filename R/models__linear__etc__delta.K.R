#' Give the gradient for a diagnoal matrix (K) w.r.t. its diagonal elements.
#'
#' Details are available in the paper.
#' @param diag.K NA
#' @param power "scalar".  The power for K to obtain the gradient for vec(K^(power))
#'     w.r.t. diag(K)'.
#' @param K "matrix".  The diagonal matrix. p-by-p
#' @return "matrix".  The gradient, pp-by-p
#'
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
delta.K <- function(diag.K,power)
{
  p <- length(diag.K)
  out <- matrix(0, p^2, p)
  delta.tmp <- power*diag.K^(power-1)
  idx <- seq(1, p^3, length.out = p)
  out[idx] <- delta.tmp
  return(out)
}
