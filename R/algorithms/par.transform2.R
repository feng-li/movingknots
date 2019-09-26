##' Transform the parameters from the original scale to the new scale (the final scale) wrt the linkages
##'
##' Reverse step of par.transfrom()
##' @param par  NA
##' @param method  NA
##' @return NA
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @export
par.transform2 <- function(par, method)
  {
    if(tolower(method) == "identity")
      {
        out <- par
      }
    else if(tolower(method) == "log")
      {
        out <- log(par)
      }
    return(out)
  }
