// Modified for string data. Source of original: https://stackoverflow.com/questions/24352208/best-way-to-convert-dataframe-to-matrix-in-rcpp

#include <Rcpp.h>
using namespace Rcpp;

Rcpp::StringMatrix df2Mat(DataFrame x){
  int nRows=x.nrows();  
  Rcpp::StringMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(_,i)=Rcpp::StringVector(x[i]);
  }  
  return y;
}
