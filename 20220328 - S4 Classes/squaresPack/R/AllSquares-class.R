#' A squared value object 
#' 
#' Object of class \code{AllSquares} as created by the \code{allSquares} functions
#'
#' 
#' An object of the class `Squares' has the following slots:
#' \itemize{
#' \item \code{addSquare} The added or subtracted squared values
#' \item \code{subtractSquare} The added or subtracted squared values
#' \item \code{x} The first input
#' \item \code{y} the second input 
#' }
#'
#' @author Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @aliases AllSquares-class initialize,AllSquares-method getSquares,AllSquares-method 
#' @rdname Squares
#' @export
setClass(Class="AllSquares",
         contains="Squares",
         representation = representation(
           addSquare = "numeric",
           subtractSquare = "numeric"
           ),
         prototype = prototype(
           addSquare = numeric(),
           subtractSquare=numeric(),
           x = numeric(),
           y = numeric(),
           square=numeric()
           )
         )

#' @export
setMethod("initialize", "AllSquares", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
          ) 

#' @export
setMethod("getSquares", "AllSquares",
          function(object){
            out <- cbind(object@addSquare, object@subtractSquare)
            colnames(out) <- c("Added", "Subtracted")
            return(out)
                 }
            )
          



##
setAs(from="Squares", to="AllSquares",
      def=function(from){
        new("AllSquares",
            x=from@x,
            y=from@y,
            square=from@square
            )
      }
      )

