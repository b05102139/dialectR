#ifndef VC_LEVEN_H
#define VC_LEVEN_H

#include <Rcpp.h>
Rcpp::NumericVector vc_leven(Rcpp::StringVector vec1, Rcpp::StringVector vec2, bool alignment_normalization, Rcpp::Nullable<std::string> delim);

#endif