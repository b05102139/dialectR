#include <Rcpp.h>
using namespace Rcpp;

Rcpp::StringMatrix df2Mat(DataFrame df){
  int rowNum=df.nrows();  
  Rcpp::StringMatrix mat(rowNum,df.size());
  for (int i=0; i<df.size();i++) {
    mat(_,i)=Rcpp::StringVector(df[i]);
  }  
  return mat;
}
