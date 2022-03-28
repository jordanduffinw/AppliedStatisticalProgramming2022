#' Calculate all squares
#'
#' Calculates both added and subtracted squares
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#'
#' @return An object of class AllSquares containing
#'  \item{addSquares}{The sum of the squared values}
#'  \item{subtractSquares}{The difference of the squared values}
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#' @author Jacob M. Montgomery
#' @note This produces an object of a new class
#' @examples
#' 
#' myX <- c(20, 3) 
#' myY <- c(-2, 4.1) 
#' allSquares(myX, myY)
#' @seealso \code{\link{subtractSquares}}, \code{\link{addSquares}}
#' @rdname allSquares
#' @aliases allSquares,ANY-method
#' @export
setGeneric(name="allSquares",
           def=function(x, y, ...)
           {standardGeneric("allSquares")}
           )

#' @export
setMethod(f="allSquares",
          definition=function(x, y, ...){
            .add <- getSquares(addSquares(x, y))
            .subtract <- getSquares(subtractSquares(x, y))
            return(new("AllSquares", addSquare=.add, subtractSquare=.subtract, x = x, y = y))
          }
          )


