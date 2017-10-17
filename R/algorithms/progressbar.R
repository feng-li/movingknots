##' Very Simple progress bar for "for loops"
##'
##' Call this function within for loop. Not work with while loop
##' Have not test with windows Gui
##' @title
##' @param iIter
##' @param nIter
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Wed Jan 27 00:33:39 CST 2016; Current: Wed Jan 27 00:33:35 CST 2016.
progressbar <- function(iIter, nIter)
{
  setTxtProgressBar(txtProgressBar(min = 0, max = nIter, style = 3), iIter)
  if(iIter == nIter) cat("\n")
}
