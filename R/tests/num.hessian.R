##' The accurate hessian matrix for given function expression.
##'
##' This approximation may be slow but can give consistent hessian approximations. The
##' purpose of this function is to temporarily replace the hessian matrix when other
##' rought approximations is ill behaved(e.g. sigular, negtive inverse hessian not
##' positive definite). 
##' @name 
##' @title 
##' @param fun 
##' @param funArgs 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
num.hessian <- function(fun, funArgs)
{

}

