#' A collection of gradient for common priors.
#'
#' The parameters after "..." should be matched exactly.
#' @param B "matrix".  The paramter that need to be added with a prior. The gradient and
#'     hessian are calculated conditional on B. B should be always an one-column matrix,
#' @param priorArgs "list".  priorArgs$prior_type: when prior_type is set to "mvnorm",
#'     you have to provide priorArgs$mean: "matrix", the mean of parameter, mu0 should be
#'     always an one-column matrix; priorArgs$covariance: "matrix", the covariance
#'     matrix. A g-prior can be constructed by setting it to X'X, where X is the
#'     covariates matrix.; priorArgs$shrinkage: "numeric", the shrinkage for the
#'     covariance.
#'
#' @param hessMethod NA
#' @return "list". The gradient and hessian matrix, see below.  `gradObsPri`
#'     "matrix". One-colunm. `hessObsPri` "matrix". A squre matrix. Dimension same as
#'     prior_type$Sigma0.
#'
#' @references NA
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
deriv_prior <- function(B, priorArgs, hessMethod)
{
  if (tolower(priorArgs$prior_type) == "mvnorm") # vecB ~ N(mean, shrinkage*covariance)
    {
      mean <- priorArgs$mean
      covariance <- priorArgs$covariance
      shrinkage <- priorArgs$shrinkage

      gradient.out <- (- 1/shrinkage * ginv(covariance) %*% (B-mean))  # TODO:
      ## if(is(gradient.out, "try-error")) browser()
      if(tolower(hessMethod) == "exact")
      {
          hessian.out <- - 1/shrinkage * ginv(covariance)
      }
      else
        {
            hessian.out = NA
        }

    }#

  out <- list(gradObsPri = gradient.out, hessObsPri = hessian.out)
  return(out)
}
