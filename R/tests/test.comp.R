require("compiler")

testfun <- function(x)
  {
    browser()
    x2 <- x^2
    return(x2)
  }
testfunc <- cmpfun(testfun)

## testfun(2)

testfunc(2)
