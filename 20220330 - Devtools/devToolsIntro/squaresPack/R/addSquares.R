#' Adding squared values
#'
#' Finds the squared sum of numbers
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#'
#' @return A list with the elements
#'  \item{squares}{The sum of the squared values}
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#' @author Jacob M. Montgomery<\email{jacob.montgomery@@wustl.edu}>
#' @note This is a very simple function
#' @examples
#' 
#' myX <- c(20, 3) 
#' myY <- c(-2, 4.1) 
#' addSquares(myX, myY)
#' @seealso subtractSquares
#' @rdname addSquares
#' @export
addSquares <- function(x, y){
  return(list(square=(x^2 + y^2), x=x, y=y))
}