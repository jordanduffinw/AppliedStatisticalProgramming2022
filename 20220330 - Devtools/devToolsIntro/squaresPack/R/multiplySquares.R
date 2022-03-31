#' Multiplying squared values
#'
#' Finds the product of squared of numbers
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#'
#' @return A list with the elements
#'  \item{squares}{The product of the squared values}
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#' @author JDW<\email{jordanduffinw@@gmail.com}>
#' @note This is a very simple function
#' @examples
#' 
#' myX <- c(20, 3) 
#' myY <- c(-2, 4.1) 
#' subtractSquares(myX, myY)
#' @seealso addSquares
#' @rdname multiplySquares
#' @include multiplySquares.R
#' @export
multiplySquares <- function(x, y){
  return(list(square=(x^2 * y^2), x=x, y=y))
}