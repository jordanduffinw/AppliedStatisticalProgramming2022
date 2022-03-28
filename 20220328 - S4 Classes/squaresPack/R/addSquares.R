#' Adding squared values
#'
#' Finds the sum of squared numbers
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#'
#' @return An object of class Squares containing
#'  \item{squares}{The sum of the squared values}
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#' @author Jacob M. Montgomery
#' @note This is a very simple function
#' @examples
#' 
#' myX <- c(20, 3) 
#' myY <- c(-2, 4.1) 
#' addSquares(myX, myY)
#' @seealso \code{\link{subtractSquares}}
#' @rdname addSquares
#' @aliases addSquares,ANY-method
#' @export
setGeneric(name="addSquares",
           def=function(x, y, ...)
           {standardGeneric("addSquares")}
           )

#' @export
setMethod(f="addSquares",
          definition=function(x, y, ...){
            return(new("Squares", square=(x^2 + y^2), x = x, y = y))
          }
          )


