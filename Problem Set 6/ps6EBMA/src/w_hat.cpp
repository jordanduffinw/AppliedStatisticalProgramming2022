#include <Rcpp.h> 
using namespace Rcpp;

//' Solving Equation 3 of the EM algorithm by computing our weights
//' 
//' @param z The matrix of prodicted z values
//' 
//' @useDynLib ps6EBMA
//' 
//' @export
// [[Rcpp::export]]

NumericVector w_hat(NumericMatrix z) {
  // Initializing variables
  int rows = z.nrow();
  int cols = z.ncol();
  NumericVector w_Out(cols);
  
  
  // We calculate the weights by summing the z_hat values over columns
  // and subsequently dividing by the number of rows in that matrix
  for (int j = 0; j < cols; ++j) {
    
    double newWeight = 0;
    for (int i = 0; i < rows; ++i) {
      newWeight += z(i,j);
    }
    w_Out[j] = newWeight / rows;
  }
  return w_Out;
}