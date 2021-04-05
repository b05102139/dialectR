#include <Rcpp.h>
#include "checkVowelConsonant.h"
#include "tinyutf8.h"

using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List vc_leven_align(Rcpp::StringVector vec1, Rcpp::StringVector vec2){
  int vec1Size=vec1.size();
  int vec2Size=vec2.size();
  if(vec1Size!=vec2Size) Rcpp::stop("The two vector inputs are not of same length.");
  tiny_utf8::string str1;
  tiny_utf8::string str2;
  int lenStr1;
  int lenStr2;
  int cost;
  NumericVector tmp(3);
  Rcpp::List res(vec1Size);
  NumericMatrix d;
  for(int i=0;i<vec1Size;i++){
    StringVector align1;
    StringVector align2;
    str1=tiny_utf8::string(as<std::string>(vec1(i)));
    str2=tiny_utf8::string(as<std::string>(vec2(i)));
    lenStr1=str1.length();
    lenStr2=str2.length();
    NumericMatrix d(lenStr1+1,lenStr2+1);
    for(int i=0;i<lenStr1+1;i++)d(i,0)=i;
    for(int j=0;j<lenStr2+1;j++)d(0,j)=j;
    cost=0;
    for(int i=1;i<lenStr1+1;i++){
      for(int j=1;j<lenStr2+1;j++){
        if(str1[i-1]==str2[j-1]) cost=0;
        else if(checkVowelConsonant(str1[i-1], str2[j-1])) cost=1;
        else cost=2;
        tmp[0]=d(i-1,j)+1;
        tmp[1]=d(i,j-1)+1;
        tmp[2]=d(i-1,j-1)+cost;
        d(i,j)=min(tmp);
      }
    }
    while (lenStr1 > 0 | lenStr2 > 0){
      int diaCell=0;
      int leftCell=0;
      int upCell=0;
      if (lenStr1-1>=0&lenStr2-1>=0){
        diaCell = d(lenStr1-1,lenStr2-1);
      } else if(lenStr1-1<0|lenStr2-1<0){
        diaCell=std::numeric_limits<int>::max();
      }
      if (lenStr2-1>=0){
        leftCell = d(lenStr1,lenStr2-1);
      } else if (lenStr2-1<0){
        leftCell=std::numeric_limits<int>::max();
      }
      if (lenStr1-1>=0){
        upCell = d(lenStr1-1,lenStr2);
      } else if (lenStr1-1<0){
        upCell=std::numeric_limits<int>::max();
      }
      IntegerVector directionVec = IntegerVector::create(diaCell, leftCell, upCell);
      int direction;
      direction = which_min(directionVec);
      if(direction==0){
        align1.push_front(str1.substr(lenStr1-1,1).c_str());
        align2.push_front(str2.substr(lenStr2-1,1).c_str());
        lenStr1 -= 1;
        lenStr2 -= 1;
      }
      else if(direction==1){
        align1.push_front('-');
        align2.push_front(str2.substr(lenStr2-1,1).c_str());
        lenStr2 -= 1;
      }
      else if(direction==2){
        align1.push_front(str1.substr(lenStr1-1,1).c_str());
        align2.push_front('-');
        lenStr1 -= 1;
      }
    }
    StringMatrix bothAlign(align1.size(), 2);
    bothAlign(_,0)=align1;
    bothAlign(_,1)=align2;
    res(i)=bothAlign;
  }
  return res;
}
