// Modified. Source of code: https://github.com/Bijaelo/RcppTinyUTF8/blob/master/src/printUTF8.cpp 

#ifndef TINYUTF8_TYPES_H
#define TINYUTF8_TYPES_H

#include <RcppCommon.h>
#include "tinyutf8.h"
template<> tiny_utf8::string Rcpp::as<tiny_utf8::string>(SEXP);

#include <Rcpp.h>

namespace Rcpp {
template<> tiny_utf8::string as<tiny_utf8::string>(SEXP x){
  if(TYPEOF(x) != STRSXP || Rf_xlength(x) > 1)
    Rcpp::stop("Unable to convert SEXP of type %s to utf8str", Rf_type2char(TYPEOF(x)));
  tiny_utf8::string xx(Rcpp::as<const char*>(x));
  return xx;
}
}

#endif