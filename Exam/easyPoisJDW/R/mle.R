#' Maximum Likelihood Estimation
#'
#' Calculates the Maximum Likelihood Estimator (MLE) for observed data
#'
#' @param y The vector of observed data
#'
#' @return A single numeric output:
#'     \item{MLE}{The MLE for the observed data.}
#'
#' @author Jordan Duffin Wong: \email{jordan.d.wong@@wustl.edu}
#' @seealso \code{\link{logLik}}, \code{\link{standardError}}, \code{\link{estimatePoisson}}
#' @examples
#' set.seed(666)
#' y <- sample(x = 1:50, size = 25, replace = TRUE)
#' mle(y)
#' @rdname mle
#' @aliases MLE
#' @include logLik.R
#' @import methods
#' @export

# Creating the generic
setGeneric(name = "mle",
           def = function(y)
             {standardGeneric("mle")}
)

# Defining the method
setMethod(f = "mle",
          definition = function(y){
            # n is the number of observations
            n <- length(y)

            # The mathematical expression for the MLE
            # Note this is just the arithmatic mean
            MLE <- sum(y) / n

            # Return MLE
            return(MLE)
          })
