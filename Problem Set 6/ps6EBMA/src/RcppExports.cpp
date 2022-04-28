// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fullEM
NumericVector fullEM(NumericMatrix x, NumericVector y, NumericVector weights, double sd, double tolerance);
RcppExport SEXP _ps6EBMA_fullEM(SEXP xSEXP, SEXP ySEXP, SEXP weightsSEXP, SEXP sdSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(fullEM(x, y, weights, sd, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// w_hat
NumericVector w_hat(NumericMatrix z);
RcppExport SEXP _ps6EBMA_w_hat(SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(w_hat(z));
    return rcpp_result_gen;
END_RCPP
}
// z_hat
NumericMatrix z_hat(NumericMatrix x, NumericVector y, NumericVector weights, double sd);
RcppExport SEXP _ps6EBMA_z_hat(SEXP xSEXP, SEXP ySEXP, SEXP weightsSEXP, SEXP sdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    rcpp_result_gen = Rcpp::wrap(z_hat(x, y, weights, sd));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ps6EBMA_fullEM", (DL_FUNC) &_ps6EBMA_fullEM, 5},
    {"_ps6EBMA_w_hat", (DL_FUNC) &_ps6EBMA_w_hat, 1},
    {"_ps6EBMA_z_hat", (DL_FUNC) &_ps6EBMA_z_hat, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_ps6EBMA(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
