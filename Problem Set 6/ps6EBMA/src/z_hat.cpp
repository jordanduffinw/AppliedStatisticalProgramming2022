#include <Rcpp.h> 
using namespace Rcpp;

//' Solving Equation 2 of the EM Algorithm for Z_Hat
//' 
//' @param y A vector of values for each observation
//' @param x The Numeric Matrix of X values for the models
//' @param weights The vector of weights for each model
//' @param sd Standard deviation
//' 
//' @useDynLib ps6EBMA
//' 
//' @export
// [[Rcpp::export]]

NumericMatrix z_hat(NumericMatrix x, NumericVector y, NumericVector weights, double sd){
  
  // Initializing Variables
  int rows = x.nrow();
  NumericMatrix dNormal(x.nrow(), x.ncol());
  NumericMatrix out(x.nrow(), x.ncol());
    
  // Calculate the dnorm() for each value in our matrix
  for (int i = 0; i<x.nrow(); ++i) {
    for (int j = 0; j<x.ncol(); ++j) {
      double datNumber = R::dnorm(y[i], x(i,j), sd, FALSE);
      dNormal(i,j) = datNumber; //
    }
  }
    
  // Creating the row sums, where every observation in the row is weighted appropriately
  NumericVector sums(rows);
  for (int i = 0; i < rows; ++i) {
    double rowSum = 0;
    for (int j = 0; j < dNormal.ncol(); ++j) {
      rowSum += weights[j] * dNormal(i,j);
    }
    sums[i] = rowSum;
  }
  
  // Calculating z_hat by 1) multiplying the dNorm by its approprate weight and
  // then 2) dividing it by its appropriate value
  for (int i = 0; i < x.nrow(); ++i) {
    for (int j = 0; j < x.ncol(); ++j) {
      out(i,j) = weights[j] * dNormal(i,j) / sums[i]; //
    }
  }
  
  return out;
}

