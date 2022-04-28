#include <Rcpp.h>
using namespace Rcpp;
NumericMatrix z_hat(NumericMatrix x, NumericVector y, NumericVector weights, double sd);
NumericVector w_hat(NumericMatrix z_hat);