#' Define the Poisson MLE Class
#'
#' An Object of class \code{PoisMLE} can be created by the \code{mlePois()} function.
#'
#'
#' An Object of the class `PoisMLE` has the following slots:
#' \itemize{
#' \item \code{y} The original data, which is count data.
#' \item \code{MLE} The maximum likelihood estimator (MLE) for this data.
#' \item \code{LL} The natural logarithm of the MLE.
#' \item \code{SE} The standard error for the MLE.
#' \item \code{SEtype} The method used to calculate the standard error.
#' }
#'
#' @author Jordan Duffin Wong: \email{jordan.d.wong@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLikelihood}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePoisson}}
#' @rdname class_PoisMLE
#' @include class_PoisMLE.R
#' @import methods
#' @export

### Creating the PoisMLE Class
setClass(Class = "PoisMLE",
         representation = representation(
           y = "numeric",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = numeric(),
           MLE = numeric(),
           LL = numeric(),
           SE = numeric(),
           SEtype = character()
         )
)


### And validating that objects are indeed `PoisMLE`
setValidity("PoisMLE", function(object){
  # Since this is count data, `y` must be a nonnegative integer
  y_testClass <- is.numeric(object@y)
  if(!y_testClass){
    stop("y must be a numeric!")
  }
  y_testNeg <- any(object@y >= 0)
  if(!y_testNeg){
    stop("Every y must be a nonnegative integer!")
  }

  # The MLE can be any number valid under the Poisson distribution, but must be exactly length 1
  MLE_testClass <- is.numeric(object@MLE)
  if(!MLE_testClass){
    stop("MLE must be a numeric!")
  }
  MLE_testLength <- length(object@MLE) != 1
  if(!MLE_testLength){
    stop("MLE must have length 1!")
  }

  # This also holds for the Log-Likelihood and the Standard Error.
  LL_testClass <- is.numeric(object@LL)
  if(!LL_testClass){
    stop("LL must be a numeric!")
  }
  LL_testLength <- length(object@LL) != 1
  if(!LL_testClass){
    stop("LL must have length 1!")
  }

  # SE testing
  SE_testClass <- is.numeric(object@SE)
  if(!SE_testClass){
    stop("SE must be a numeric!")
  }
  SE_testLength <- length(object@SE) != 1
  if(!SE_testLength){
    stop("SE must have length 1!")
  }

  # Finally, `SEtype` must be a character
  SEtype_testClass <- is.character(object@SEtype)
  if(!SEtype_testClass){
    stop("SEtype must be a character!")
  }
}
)

### And creating our method to initialize
setMethod("initialize", "PoisMLE",
          function(.Object, ...){
            value = callNextMethod()
            return(value)
          })

