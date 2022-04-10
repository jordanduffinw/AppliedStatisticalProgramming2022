#' Log-Likelihood
#'
#' Calculates the Log-Likelihood for observed data
#'
#' @param y The vector of observed data
#' @param lambda the assumed value of \eqn{\lambda} lambda
#'
#' @return A single numeric output:
#'     \item{log_likelihood}{The log-likelihood for the observed data conditioning on assumed value of lambda.}
#'
#' @author Jordan Duffin Wong: \email{jordan.d.wong@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standarderror}}, \code{\link[PoisMLE]{estimatePoisson}}
#' @examples
#' set.seed(666)
#' y <- sample(x = 1:50, size = 25, replace = TRUE)
#' logLik(y, lambda = 3)
#' @rdname logLik
#' @include logLik.R
#' @import methods
#' @export

# Creating the generic
setGeneric(name = "logLik",
           def = function(y, lambda)
             {standardGeneric("logLik")}
)

# Defining the method
setMethod(f = "logLik",
          definition = function(y, lambda){
            # n is the number of observations
            n <- length(y)

            # The mathematical expression for the log-likelihood
            LL <- -1*n*lambda - sum(log(factorial(y))) + log(lambda) * sum(y)

            # Return LL
            return(LL)
          })
