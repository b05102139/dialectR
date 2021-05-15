#include <progress.hpp>
#include <progress_bar.hpp>
#include "df2Mat.h"
#include "vc_leven.h"
#include "leven.h"
#include "checkVowelConsonant.h"

using namespace Rcpp;

typedef Rcpp::NumericVector (*funcPtr)(Rcpp::StringVector vec1, Rcpp::StringVector vec2, bool alignment_normalization, Rcpp::Nullable<std::string> delim);
XPtr<funcPtr> putFunPtrInXPtr(std::string fstr) {
  if (fstr == "leven")
    return(XPtr<funcPtr>(new funcPtr(&leven)));
  else if (fstr == "vc_leven")
    return(XPtr<funcPtr>(new funcPtr(&vc_leven)));
  else
    return XPtr<funcPtr>(R_NilValue);
}

//' Distance matrix for Dialectometry
//'
//' Computes a distance matrix between dialect varieties, the results of which may be used for further analyses and plotting.
//'
//' @param dialect_data A dataframe of dialect data, transcribed in the International Phonetic Alphabet.
//' @param funname The distance metric to be used. This can be chosen from the following: "leven", "vc_leven".
//' @param alignment_normalization A logical value, indicating whether or not the distance scores should be normalized by alignment length.
//' @param delim An optional delimiter, in situations where multiple responses exist in the data.
//' @return A distance matrix, where the values are the difference between dialects based on edit distance.
//' @examples
//' data(Dutch)
//' Dutch <- Dutch[1:3,1:3]
//' distance_matrix(Dutch, funname = "vc_leven", alignment_normalization = TRUE)
// [[Rcpp::export]]
Rcpp::NumericMatrix distance_matrix(Rcpp::DataFrame dialect_data, std::string funname, bool alignment_normalization = false, Rcpp::Nullable<std::string> delim = R_NilValue){
  XPtr<funcPtr> xpfun = putFunPtrInXPtr(funname);
  funcPtr fun = *xpfun;
  Rcpp::StringMatrix dialect_matrix=df2Mat(dialect_data);
  int n = dialect_matrix.nrow();
  Progress p(sum(seq(1,n-1)),true);
  NumericMatrix d = no_init_matrix(n, n);
    for(int i=0; i<n;i++){
      for(int j=i+1; j<n;j++){
        d(i,j)=sum(na_omit(fun(dialect_matrix(i,_), dialect_matrix(j,_), alignment_normalization, delim)));
        d(j,i)=d(i,j);
        p.increment();
      }
      d(i,i)=0;
    }
    rownames(d)=as<StringVector>(dialect_data.attr("row.names"));
    colnames(d)=as<StringVector>(dialect_data.attr("row.names"));
    return d;
}
