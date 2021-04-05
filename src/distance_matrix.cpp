#include <Rcpp.h>
#include "df2Mat.h"
#include "checkVowelConsonant.h"
#include "vc_leven.h"
//#include "leven.h"
using namespace Rcpp;

typedef Rcpp::NumericVector (*funcPtr)(Rcpp::StringVector vec1, Rcpp::StringVector vec2, bool alignment_normalization, Rcpp::Nullable<std::string> delim_);

XPtr<funcPtr> putFunPtrInXPtr(std::string fstr) {
  if (fstr == "vc_leven")
    return(XPtr<funcPtr>(new funcPtr(&vc_leven)));
//  else if (fstr == "vc_leven")
//    return(XPtr<funcPtr>(new funcPtr(&vc_leven)));
  else
    return XPtr<funcPtr>(R_NilValue); // runtime error as NULL no XPtr
}

// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
// [[Rcpp::export]]
Rcpp::NumericMatrix distance_matrix(Rcpp::DataFrame dialect_data, std::string funname, bool alignment_normalization = false, Rcpp::Nullable<std::string> delim_ = R_NilValue){
  XPtr<funcPtr> xpfun = putFunPtrInXPtr(funname);
  funcPtr fun = *xpfun;
  Rcpp::StringMatrix dialect_matrix=df2Mat(dialect_data);
  int n = dialect_matrix.nrow();
  Progress p(sum(seq(1,n-1)),true);
  NumericMatrix d = no_init_matrix(n, n);
    for(int i=0; i<n;i++){
      for(int j=i+1; j<n;j++){
        d(i,j)=sum(na_omit(fun(dialect_matrix(i,_), dialect_matrix(j,_), alignment_normalization, delim_)));
        d(j,i)=d(i,j);
        p.increment();
      }
      d(i,i)=0;
    }
    rownames(d)=as<StringVector>(dialect_data.attr("row.names"));
    colnames(d)=as<StringVector>(dialect_data.attr("row.names"));
    return d;
}
