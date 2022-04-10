#' Standard Error
#'
#' Calculates the standard error for the data using either a basic or bootstrap method
#'
#' @param y The vector of observed data
#' @param SEtype The type of SE, can choose either `basic` or `bootstrap`
#' @param B The number of bootstrap iterations to run if `SEtype` is `bootstrap`. Defaults to 1000.
#'
#' @return A single numeric output:
#'     \item{SE}{The standard error for the observed data.}
#'
#' @author Jordan Duffin Wong: \email{jordan.d.wong@@wustl.edu}
#' @seealso \code{\link{logLik}}, \code{\link{mle}}, \code{\link{estimatePoisson}}
#' @examples
#' set.seed(666)
#' y <- sample(x = 1:50, size = 25, replace = TRUE)
#' standardError(y, "basic")
#' standardError(y, "bootstrap", B = 1000)
#'
#' @rdname standardError
#' @aliases SE
#' @import methods
#' @import stats
#' @export

# Creating the generic
setGeneric(name = "standardError",
           def = function(y, SEtype, B = 1000)
             {standardGeneric("standardError")}
)

# Defining the method
setMethod(f = "standardError",
          definition = function(y, SEtype = c("basic", "bootstrap"), B){
            if(SEtype == "basic"){
              StdE <- sqrt(mle(y) / length(y))
            }
            else if(SEtype == "bootstrap"){
              # Making sure B is actually doable -- i.e., an integer
              if(!is.numeric(B)){
                stop("B must be a positive integer greater than 1!")
              }
              if(B <= 1){
                stop("B must be a positive integer greater than 1!")
              }

              # Creating our bootstrap
              booterrors <- NULL
              for (i in 1:B) {
                bootdata <- sample(y, length(y), replace = T)
                booterrors[i] <- sd(bootdata)
              }
              bootStandardError <- mean(booterrors)
              return(bootStandardError)
            }
            else{stop("SEtype must be `basic` or `bootstrap`")}
          }
)
