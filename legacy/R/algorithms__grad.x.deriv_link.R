#' Gradient derivative for x wrt link function
#'
#' Just the chain rule.
#' @param grad "matrix" gradient
#' @param link NA
#' @param Param "matrix" should always be a matrix
#' @return NA
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
grad.x.deriv_link <- function(grad, Param, link)
  {
    if(tolower(link) == "identity")
      {
        out <- grad # unchanged
      }
    else if(tolower(link) == "log")
      {
        dim.grad <- dim(grad)

        chain2 <- matrix(Param,dim.grad[1],dim.grad[2], byrow = TRUE)
        out <- grad*chain2
      }
    return(out)
  }
