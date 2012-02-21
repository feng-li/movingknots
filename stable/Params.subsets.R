##' Organize the subsets of the parameters by taking away the fixed parameters. 
##'
##' <details>
##' @title 
##' @param p 
##' @param splineArgs 
##' @param Params_Fixed 
##' @param Params_subsetsArgs 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
Params.subsets <- function(p, splineArgs, Params_Fixed, Params_subsetsArgs)
  {
    out <- list()
##----------------------------------------------------------------------------------------
    ## knots
##----------------------------------------------------------------------------------------
    s.dim <- splineArgs$thinplate.s.dim 
    a.locate <- splineArgs$thinplate.a.locate

    knots.maxlen <- prod(s.dim) + sum(a.locate) # The maximum number of knots parameters
                                        # maybe used 

    ## The index matrix
    knots.idx <- knots.mat2list(1:knots.maxlen, splineArgs)
    knots.comp <- names(knots.idx)

    knots.partiArgs <- Params_subsetsArgs[["knots"]]

    knots.subsets <- list()
    
    ## Split the surface part(if exists)
    if("thinplate.s" %in% knots.comp)
      {
        knots.s.idx <- knots.idx[["thinplate.s"]]
        knots.s <- 1:s.dim[1]
        fixed.s <- Params_Fixed[["knots"]]$thinplate.s
        knots.s.remain <- knots.s[! knots.s  %in%  fixed.s]
        length.s.remain <- length(knots.s.remain)
        if(length.s.remain != 0) # not all are fixed
          {
            s.subsets.idx <- data.partition(length.s.remain, knots.partiArgs)
            
            for(i in 1:length(s.subsets.idx))
              {
                knots.subsets[["thinplate.s"]][[i]] <-
                  as.vector(t(knots.s.idx[as.vector(knots.s.remain), , drop =
                                          FALSE][as.vector(s.subsets.idx[[i]]), ]))        
              }
          }
        else
          {
            knots.subsets[["thinplate.s"]] <- NULL
          }
        
      }
    ## Split the additive part(if exists)
    if(("thinplate.a" %in% knots.comp))
      {
        knots.a.idx <- knots.idx[["thinplate.a"]]
        knots.a <- 1:sum(a.locate)
        fixed.a <- Params_Fixed[["knots"]]$thinplate.a
        knots.a.remain <- knots.a[! knots.a  %in%  fixed.a]
        length.a.remain <- length(knots.a.remain)
        if(length.a.remain  != 0)
          {
            a.subsets.idx <- data.partition(length.a.remain, knots.partiArgs)
            
            for(i in 1:length(a.subsets.idx))
              {
                knots.subsets[["thinplate.a"]][[i]] <-
                  as.vector(knots.a.idx[as.vector(knots.a.remain)][as.vector(a.subsets.idx[[i]])]) 
              }
          }
        else
          {
            knots.subsets[["thinplate.a"]] <- NULL
          }
      }
    
    ## Merge or split
    knots.split <- Params_subsetsArgs[["knots"]]$split
    if(knots.split == TRUE) # split
      {
       out.knots <- unlist(knots.subsets, recursive = FALSE)
       names(out.knots) <- NULL
      }
    else # merge
      {
        knots.N.subsets <- knots.partiArgs$N.subsets
        out.knots <- list()
        for(i in 1:knots.N.subsets)
          {
            out.knots[[i]] <- c(knots.subsets[["thinplate.s"]][[i]],
                                knots.subsets[["thinplate.a"]][[i]]) 
          }
      }

    ## output
    if(length(out.knots) == 0)
      {
        out[["knots"]] <- list(NULL)
      }
    else
      {
        out[["knots"]] <- out.knots
      }
        
##----------------------------------------------------------------------------------------
    ## shrinkages
##----------------------------------------------------------------------------------------   

    ## The remaining knots in each part
    shrinkages.fixed <- Params_Fixed[["shrinkages"]]
    model.comp <- splineArgs[["comp"]][ "intercept" != splineArgs[["comp"]] ]
    ncomp <- length(model.comp)
    shrinkages.idx <- 1:(p*ncomp)
    
    shrinkages.remain <- shrinkages.idx[! shrinkages.idx %in% shrinkages.fixed]
    shrinkages.remain.len <- length(shrinkages.remain)

    if(shrinkages.remain.len != 0)
      {
        shrinkages.parti.idx <- data.partition(shrinkages.remain.len,
                                               Params_subsetsArgs[["shrinkages"]])
        out.shrinkages <- list()
        
        for(i in 1:length(shrinkages.parti.idx))
          {
            out.shrinkages[[i]] <-
              shrinkages.idx[as.vector(shrinkages.remain)][as.vector(shrinkages.parti.idx[[i]])] 
          }
        out[["shrinkages"]] <- out.shrinkages
      }
    else
      {
        out[["shrinkages"]] <- list(NULL)
      }
##----------------------------------------------------------------------------------------
    ## coefficients
##----------------------------------------------------------------------------------------   

    out[["coefficients"]] <- list(NULL) # been integrated out
##----------------------------------------------------------------------------------------
    ## covariance 
##----------------------------------------------------------------------------------------   
    ## FIXME: PARTITION this
    out[["covariance"]] <- list(1:(p*(p+1)/2))
    return(out)
  }
  
##----------------------------------------------------------------------------------------
## TESTS: PASSED
##----------------------------------------------------------------------------------------
## Params.subsets(p, splineArgs, Params_Fixed, Params_subsetsArgs)
