#ifndef LEVEN_H
#define LEVEN_H

#include <Rcpp.h>
Rcpp::NumericVector leven(Rcpp::StringVector vec1, Rcpp::StringVector vec2, bool alignment_normalization, Rcpp::Nullable<std::string> delim);

#endif