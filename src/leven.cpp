#include <Rcpp.h>
#include "split.h"
#include "tinyutf8.h"

using namespace Rcpp;

//' Edit distance for Dialectometry
//'
//' An edit distance for use in Dialectometry. Allows for normalization by dividing alignment length, and for accommodating multiple responses with Bilbao distance.
//'
//' @param vec1 A vector of words.
//' @param vec2 A vector of words to be compared against.
//' @param alignment_normalization A logical value, indicating whether or not the difference scores are to be normalized by alignment length.
//' @param delim An optional delimiter, in situations where multiple responses exist in the data.
//' @return A number indicating the number of operations to transform a string to the other, which optionally may undergo length normalization.
//' @examples
//' leven("hit", "hot/hit", alignment_normalization = TRUE, delim = "/")
// [[Rcpp::export]]
Rcpp::NumericVector leven(Rcpp::StringVector vec1, Rcpp::StringVector vec2, bool alignment_normalization = false, Rcpp::Nullable<std::string> delim = R_NilValue){
  int vec1Size=vec1.size();
  int vec2Size=vec2.size();
  if(vec1Size!=vec2Size) Rcpp::stop("The two vector inputs are not of same length.");
  tiny_utf8::string str1;
  tiny_utf8::string str2;
  int lenStr1;
  int lenStr2;
  int cost;
  NumericVector tmp(3);
  Rcpp::NumericVector res(vec1Size);
  int inCounter;
  int delCounter;
  NumericMatrix d;
  if(delim.isNull()){
    for(int i=0;i<vec1Size;i++){
      inCounter=0;
      delCounter=0;
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
          else cost=1;
          tmp(0)=d(i-1,j)+1;
          tmp(1)=d(i,j-1)+1;
          tmp(2)=d(i-1,j-1)+cost;
          d(i,j)=min(tmp);
        }
      }
      if (alignment_normalization){
        int tmpLenStr1=lenStr1;
        int tmpLenStr2=lenStr2;
        while ((tmpLenStr1>0)|(tmpLenStr2>0)){
          int diaCell;
          int leftCell;
          int upCell;
          if ((tmpLenStr1-1>=0)&(tmpLenStr2-1>=0)){
            diaCell = d(tmpLenStr1-1,tmpLenStr2-1);
          } else if((tmpLenStr1-1<0)|(tmpLenStr2-1<0)){
            diaCell=std::numeric_limits<int>::max();
          }
          if (tmpLenStr2-1>=0){
            leftCell = d(tmpLenStr1,tmpLenStr2-1);
          } else if (tmpLenStr2-1<0){
            leftCell=std::numeric_limits<int>::max();
          }
          if (tmpLenStr1-1>=0){
            upCell = d(tmpLenStr1-1,tmpLenStr2);
          } else if (tmpLenStr1-1<0){
            upCell=std::numeric_limits<int>::max();
          }
          IntegerVector directionVec = IntegerVector::create(diaCell, leftCell, upCell);
          int direction = which_min(directionVec);
          if(direction==0){
            tmpLenStr1 -= 1;
            tmpLenStr2 -= 1;
          }
          else if(direction==1){
            inCounter += 1;
            tmpLenStr2 -= 1;
          }
          else if(direction==2){
            delCounter += 1;
            tmpLenStr1 -= 1;
          }
        }
        if (any(is_na(as<StringVector>(vec1[i])))|any(is_na(as<StringVector>(vec2[i])))){
          res(i)=NA_REAL;
        } else{
          res(i)=d(lenStr1,lenStr2) / (max(NumericVector::create(inCounter, delCounter))+std::min(lenStr1, lenStr2));
        }
      } else if (!alignment_normalization){
        if (any(is_na(as<StringVector>(vec1[i])))|any(is_na(as<StringVector>(vec2[i])))){
          res(i)=NA_REAL;
        } else{
          res(i)=d(lenStr1,lenStr2);
        }
      }
    }
  } else if(delim.isNotNull()){
    std::string delim_ = String(delim);
    NumericMatrix bilbaoMatrix;
    int arr1Size;
    int arr2Size;
    double bilbaoNumerator;
    for(int k=0;k<vec1Size;k++){
      str1=tiny_utf8::string(as<std::string>(vec1(k)));
      str2=tiny_utf8::string(as<std::string>(vec2(k)));
      StringVector arr1=split(str1.c_str(), delim_);
      StringVector arr2=split(str2.c_str(), delim_);
      arr1Size=arr1.size();
      arr2Size=arr2.size();
      NumericMatrix bilbaoMatrix(arr1Size, arr2Size);
      for (int i=0;i<arr1Size;i++){
        for (int j=0;j<arr2Size;j++){
          str1=tiny_utf8::string(as<std::string>(arr1(i)));
          str2=tiny_utf8::string(as<std::string>(arr2(j)));
          bilbaoNumerator=0;
          inCounter=0;
          delCounter=0;
          int lenStr1=str1.length();
          int lenStr2=str2.length();
          NumericMatrix d(lenStr1+1,lenStr2+1);
          for(int i=0;i<lenStr1+1;i++)d(i,0)=i;
          for(int j=0;j<lenStr2+1;j++)d(0,j)=j;
          cost=0;
          for(int i=1;i<lenStr1+1;i++){
            for(int j=1;j<lenStr2+1;j++){
              if(str1[i-1]==str2[j-1]) cost=0;
              else cost=1;
              tmp(0)=d(i-1,j)+1;
              tmp(1)=d(i,j-1)+1;
              tmp(2)=d(i-1,j-1)+cost;
              d(i,j)=min(tmp);
            }
          }
          if (alignment_normalization){
            int tmpLenStr1=lenStr1;
            int tmpLenStr2=lenStr2;
            while ((tmpLenStr1>0)|(tmpLenStr2>0)){
              int diaCell;
              int leftCell;
              int upCell;
              if ((tmpLenStr1-1>=0)&(tmpLenStr2-1>=0)){
                diaCell = d(tmpLenStr1-1,tmpLenStr2-1);
              } else if((tmpLenStr1-1<0)|(tmpLenStr2-1<0)){
                diaCell=std::numeric_limits<int>::max();
              }
              if (tmpLenStr2-1>=0){
                leftCell = d(tmpLenStr1,tmpLenStr2-1);
              } else if (tmpLenStr2-1<0){
                leftCell=std::numeric_limits<int>::max();
              }
              if (tmpLenStr1-1>=0){
                upCell = d(tmpLenStr1-1,tmpLenStr2);
              } else if (tmpLenStr1-1<0){
                upCell=std::numeric_limits<int>::max();
              }
              IntegerVector directionVec = IntegerVector::create(diaCell, leftCell, upCell);
              int direction = which_min(directionVec);
              if(direction==0){
                tmpLenStr1 -= 1;
                tmpLenStr2 -= 1;
              }
              else if(direction==1){
                inCounter += 1;
                tmpLenStr2 -= 1;
              }
              else if(direction==2){
                delCounter += 1;
                tmpLenStr1 -= 1;
              }
            }
            bilbaoMatrix(i,j)=d(lenStr1,lenStr2) / (max(NumericVector::create(inCounter, delCounter))+std::min(lenStr1, lenStr2));
          } else if (!alignment_normalization){
            bilbaoMatrix(i,j)=d(lenStr1,lenStr2);
          }
        }
      }
      for (int i=0;i<arr1Size;i++){
        bilbaoNumerator+=min(bilbaoMatrix(i,_));
      }
      for (int j=0;j<arr2Size;j++){
        bilbaoNumerator+=min(bilbaoMatrix(_,j));
      }
      
      if (any(is_na(as<StringVector>(vec1[k])))|any(is_na(as<StringVector>(vec2[k])))){
        res(k)=NA_REAL;
      } else{
        res(k)=bilbaoNumerator/(arr1Size+arr2Size);
      }
    }
  }
  return res;
}
