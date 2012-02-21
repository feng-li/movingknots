##' Setup cross-validation 
##'
##' Details.
##' @title 
##' @param n.obs 
##' @param crossvalidArgs 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
set.crossvalid <- function(n.obs, crossvalidArgs)
{
  ## The predictive subsets. If no cross-validation. Just predictive all the insample data.
  Data.testing.sub <- data.partition(n.obs = n.obs, args = crossvalidArgs)
  
  ## The testing datasets (the whole dataset by taking away the predictive datasets)
  if(cross.validation$N.subsets == 1) # If no cross-validation,  use full sample data
    { 
      Data.training.sub <- list(1:n.obs)
      Data.testing.sub <- list(1:n.obs)
    }
  else
    { Data.training.sub <- lapply(Data.testing.sub, function(x) (1:n.obs)[-x])}

  ## Do cross-validation also include a full run.
  if(crossvalidArgs$full.run == TRUE && cross.validation$N.subsets != 1)
    {
      Data.training.sub[[cross.validation$N.subsets+1]] <-  1:n.obs
      Data.testing.sub[[cross.validation$N.subsets+1]] <-  1:n.obs
    }

  out <- list(training = Data.training.sub, testing = Data.testing.sub)
  return(out)
}
