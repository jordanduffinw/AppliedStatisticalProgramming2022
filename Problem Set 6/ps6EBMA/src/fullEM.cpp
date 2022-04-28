#include <Rcpp.h> 
#include "funs.hpp"
using namespace Rcpp;

//' Doing the full EM Algorithm
//' 
//' @param y A vector of values for each observation
//' @param x The Numeric Matrix of X values for the models
//' @param weights The vector of weights for each model
//' @param sd Standard deviation
//' @param tolerance The level of chnage between iterations where the model will stop
//' 
//' @useDynLib ps6EBMA
//' 
//' @export
// [[Rcpp::export]]

NumericVector fullEM(NumericMatrix x, NumericVector y, NumericVector weights, double sd, double tolerance) {
  
  // Initializing variables
  bool threshold = FALSE;
  int iterations = 0;
  
  // So before we tell things to stop:
  while (threshold == FALSE) {
    // Getting our z_hat
    NumericMatrix z_hats = z_hat(x, y, weights, sd);
    NumericVector new_weights = w_hat(z_hats);
    
    // Checking that the difference between the old and new weights
    // doesn't exceed the tolerance. If they don't, we keep going
    // until we meet that threshold
    
    LogicalVector test = (abs((weights - new_weights)) < tolerance);
    
    if (is_true ( all(test) )) {
      threshold = TRUE;
    } else {
      weights = new_weights;
    }
    
    iterations = iterations + 1;
  }
  
  return weights;
}