#' A squared value object 
#' 
#' Object of class \code{SquaresPack} are created by the \code{addSquares} and \code{subtractSquares} functions
#'
#' 
#' An object of the class `SquaresPack' has the following slots:
#' \itemize{
#' \item \code{square} The added or subtracted squared values
#' \item \code{x} The first input
#' \item \code{y} the second input 
#' }
#'
#' @author Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @aliases Squares-class initialize,Squares-method getSquares,Squares-method 
#' @rdname Squares
#' @export
setClass(Class="Squares", 
     representation = representation(
          square = "numeric",
          x = "numeric",
          y = "numeric"
          ),
     prototype = prototype(
          square = c(),
          x = c(),
          y = c()
          )
     )

#' @export
setMethod("initialize", "Squares", 
     function(.Object, ...){
           value=callNextMethod()
           return(value)
         }
         ) 

#' @rdname Squares
#' @export 
setGeneric("getSquares",
     function(object="Squares")  {
         standardGeneric("getSquares")
       }
       )

#' @export
setMethod("getSquares", "Squares",
     function(object){ 
          return(object@square)
        }
        )
