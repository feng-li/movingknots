##' <description>
##'
##' <details>
##' @title 
##' @param aT 
##' @param B 
##' @param Sigma.inv 
##' @param Xmats.delta.knots 
##' @param P.type 
##' @param p 
##' @param q 
##' @param q.i 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Tue Feb 01 10:25:02 CET 2011;
##'       Current:       Tue Feb 01 10:25:08 CET 2011.
## aT.k.B.x.DvecSigma4beta.inv <- function(aT, B, Sigma.inv, delta.K.list, Xmats.delta.knots,
##                                         P.type, p, q, q.i, n.par4knots) 
##   {


    
##     ## basic information
##     n.spline.comp <- length(Xmats.delta.knots) ## number of spline components
##     name.spline.comp <- names(Xmats.delta.knots) ## the name of spline components 
##     q.i.cumsum<- cumsum(q.i) ## The artificial index

##     ## data manipulate
    
##     ## The output matrix
##     out.lst <- list()

##     ## Loop over the knots components see Li's notes
##     for(i in 1:n.spline.comp)
##       {
##         ## Check if the prior of P_i,  and have the gradient respectively.
##         if(P.type[i+1] == "X'X") ## Situation that P.i  = "X'X"
##           {

##             ## Calculate the "A" matrix. See Li's notes.
##             idx4alphaT <- (p*(q.i.cumsum[i])+ 1):(p*(q.i.cumsum[i+1]))
##             alphaT <- aT[, idx4alphaT, drop = FALSE]
##             A.i.full <- alphaT %x% B # The full matrix

##             idx4A.i <- matrix(FALSE, p*q, p*q.i[i+1]) # Initial index
##             idxnoempty <- idx4alphaT # Just exactly is the previous index,  use directly 
##             idx4A.i[idxnoempty, ] <- TRUE # keep these cols in A.i
            
##             A.i <- A.i.full[, idx4A.i, drop = FALSE] # A.i taking away 
##             ## Calculate the last part

##             KSigmaK.inv <- diag(1/sqrt(diag.K.list[[i+1]]), ncol =
##                                 length(diag.K.list[[i+1]])) %d*d% Sigma.inv
            
##             Mat1 <- Mat.x.DvecA.k.P_stp1(A.i, KSigmaK.inv, p, q, q.i[i+1])
##             Mat2 <- Mat.x.DvecA.k.P_stp2(Mat1, Xmats.delta.knots[[i]], p, q, q.i[i+1])
            
##             out.lst[[name.spline.comp[i]]] <- Mat2
##           }
##         else ## return zero matrix
##           {
##            out.lst[[name.spline.comp[i]]] <- matrix(0, p*q,  n.par4knots[i]) ## 
##           }
##       }

##     ## combine the output,  
##     out.mat <- matrix(unlist(out.lst), p*q) # pq-by-unknown

##     return(out.mat)
##   }

##----------------------------------------------------------------------------------------
## TESTS:
##----------------------------------------------------------------------------------------
## out <- aT.k.B.x.DvecSigma4beta.inv(a, B, Sigma.inv, P.mats, X.mats, delta.xi, P.type, p, q, q.i)
