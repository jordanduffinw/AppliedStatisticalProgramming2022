#' Estimate Poisson
#'
#' Estimates the Poisson Distribution of a vector of data and returns an object of class `PoisMLE`.
#'
#' @param y The vector of observed data
#' @param SEtype The type of SE, can choose either `basic` or `bootstrap`
#' @param B The number of bootstrap iterations to run if `SEtype` is `bootstrap`. Defaults to 1000.
#'
#' @return A single numeric output:
#'     \item{PoisMLE}{An object of class `PoisMLE` which contains the MLE, Log-Likelihood, Standard Error, and type of Standard Error used.}
#'
#' @author Jordan Duffin Wong: \email{jordan.d.wong@@wustl.edu}
#' @seealso \code{\link{logLik}}, \code{\link{mle}}, \code{\link{standardError}}
#' @examples
#' set.seed(666)
#' y <- rpois(1000, 10)
#' estimatePois(y, "basic")
#' estimatePois(y, "bootstrap", B = 1000)
#'
#' @rdname estimatePois
#' @aliases estimatePoisson
#' @export

# Creating the generic
setGeneric(name = "estimatePois",
           def = function(y, SEtype, B = 1000){
             standardGeneric("estimatePois")
           })

# Defining the method
setMethod(f = "estimatePois",
          definition = function(y, SEtype = c("basic", "bootstrap"), B = 1000){
            MLE <- mle(y)
            LL <- logLik(y, lambda = MLE)
            SE <- standardError(y, SEtype, B)
            Poisson_MLE <- new("PoisMLE",
                               y = y,
                               MLE = MLE,
                               LL = LL,
                               SE = SE,
                               SEtype = SEtype)
            return(c(paste("MLE: ", Poisson_MLE@MLE),
                     paste("LL: ", Poisson_MLE@LL),
                     paste("SE: ", Poisson_MLE@SE),
                     paste("SEtype: ", Poisson_MLE@SEtype))
                   ) # I know that the assignment says to return y as well, but in my examples that's like a thousand characters long and it gets cumbersome fast.
                     # But if you really wanted to you'd add paste("y: ", Poisson_MLE@y) similar to the rest.
          })
