#include <Rcpp.h>
#include "split.h"
#include "checkVowelConsonant.h"
#include "tinyutf8.h"
#include <string>

using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
Rcpp::NumericVector vc_leven(Rcpp::StringVector vec1, Rcpp::StringVector vec2, bool alignment_normalization = false, Rcpp::Nullable<std::string> delim = R_NilValue){
  if (all(is_na(vec1))) return R_NilValue;
  if (all(is_na(vec2))) return R_NilValue;
  int vec1Size=vec1.size();
  int vec2Size=vec2.size();
  if(vec1Size!=vec2Size) Rcpp::stop("The two vector inputs are not of same length.");
  tiny_utf8::string str1;
  tiny_utf8::string str2;
  int lenStr1;
  int lenStr2;
  double cost;
  //double inCost;
  //double delCost;
  NumericVector tmp(3);
  Rcpp::NumericVector res(vec1Size);
  int inCounter;
  int delCounter;
  //NumericMatrix forbid;
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
      NumericMatrix forbid(lenStr1+1,lenStr2+1);
      for(int i=0;i<lenStr1+1;i++)d(i,0)=i;
      for(int j=0;j<lenStr2+1;j++)d(0,j)=j;
      cost=0;
      //if (segment_weights.isNull()){
        for(int i=1;i<lenStr1+1;i++){
          for(int j=1;j<lenStr2+1;j++){
            if(str1[i-1]==str2[j-1]){
              cost=0;
            }
            else if (checkVowelConsonant(str1[i-1], str2[j-1])){
              cost=1;
            } else{
              cost=2;
            }
            tmp[0]=d(i-1,j)+1;
            tmp[1]=d(i,j-1)+1;
            tmp[2]=d(i-1,j-1)+cost;
            /*
            if(!checkVowelConsonant(str1[i-1], str2[j-1])){
              forbid(i,j)=1;
            }
            */
            d(i,j)=min(tmp);
          }
        }
        //}
        /*
      else if (!segment_weights.isNull()){
        NumericMatrix segment_weights2 = NumericMatrix(segment_weights);
        StringVector segment_weight_rownames = rownames(segment_weights2);
        for(int i=1;i<lenStr1+1;i++){
          for(int j=1;j<lenStr2+1;j++){
            int str1_index;
            int str2_index;
            int indel_index;
            NumericVector typeChange;
            typeChange.push_back(str1[i-1]);
            typeChange.push_back(str2[j-1]);
            for (int k=0; k<segment_weight_rownames.length(); k++){
              //int myA = std::stoi(as<std::string>(segment_weight_rownames[k]));
              if (typeChange(0)==std::stoi(as<std::string>(segment_weight_rownames[k]))) str1_index = k;
            }
            for (int k=0; k<segment_weight_rownames.length(); k++){
              //std::string my2 = as<std::string>(segment_weight_rownames[k]);
              //int myB = std::stoi(as<std::string>(segment_weight_rownames[k]));
              if (typeChange(1)==std::stoi(as<std::string>(segment_weight_rownames[k]))) str2_index = k;
            }
            // find 45
            for (int k=0; k<segment_weight_rownames.length(); k++){
              if (45==std::stoi(as<std::string>(segment_weight_rownames[k]))) indel_index = k;
            }
            cost = segment_weights2(str1_index, str2_index);
            inCost = segment_weights2(indel_index, str2_index);
            delCost = segment_weights2(str1_index, indel_index);
            //Rcout << cost << " " << inCost << " " << delCost << std::endl;
            //tmp[0]=d(i-1,j)+1;
            //tmp[1]=d(i,j-1)+1;
            tmp[0]=d(i-1,j)+inCost;
            tmp[1]=d(i,j-1)+delCost;
            tmp[2]=d(i-1,j-1)+cost;
            if(!checkVowelConsonant(str1[i-1], str2[j-1])){
              forbid(i,j)=1;
            }
            d(i,j)=min(tmp);
          }
        }
      }
       */
      //Rcout << d << std::endl;
      //forbid(lenStr1,lenStr2)=0;
      //for (int m=1; m<lenStr1+1; m++){
      //  for (int n=1; n<lenStr2+1; n++){
      //    if (forbid(m,n)==1) d(m,n) = std::numeric_limits<int>::max();
      //  }
      //}
      //forbid(lenStr1+1,lenStr2+1) = std::numeric_limits<int>::max();
      if (alignment_normalization){
        int tmpLenStr1=lenStr1;
        int tmpLenStr2=lenStr2;
        while ((tmpLenStr1>0)||(tmpLenStr2>0)){
          int diaCell;
          int leftCell;
          int upCell;
          if (tmpLenStr1-1>=0&tmpLenStr2-1>=0){
          /*
            if (forbid(lenStr1,lenStr2)==1){
              d(tmpLenStr1-1,tmpLenStr2-1) = std::numeric_limits<int>::max()-1;
            }
          */ 
            diaCell = d(tmpLenStr1-1,tmpLenStr2-1);
          } else if(tmpLenStr1-1<0|tmpLenStr2-1<0){
            diaCell=std::numeric_limits<int>::max();
          } else{
            diaCell = d(tmpLenStr1-1,tmpLenStr2-1);
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
          //if (segment_weights.isNull()){
            for(int i=1;i<lenStr1+1;i++){
              for(int j=1;j<lenStr2+1;j++){
                if(str1[i-1]==str2[j-1]){
                  cost=0;
                }
                else if (checkVowelConsonant(str1[i-1],str2[j-1])){
                  cost=1;
                } else{
                  cost=2;
                }
                tmp[0]=d(i-1,j)+1;
                tmp[1]=d(i,j-1)+1;
                tmp[2]=d(i-1,j-1)+cost;
                /*
                if(!checkVowelConsonant(str1[i-1], str2[j-1])){
                  forbid(i,j)=1;
                }
                */
                d(i,j)=min(tmp);
              }
            }
          //} 
          /*
          else if (!segment_weights.isNull()){
            NumericMatrix segment_weights2 = NumericMatrix(segment_weights);
            StringVector segment_weight_rownames = rownames(segment_weights2);
            for(int i=1;i<lenStr1+1;i++){
              for(int j=1;j<lenStr2+1;j++){
                int str1_index;
                int str2_index;
                NumericVector typeChange;
                typeChange.push_back(str1[i-1]);
                typeChange.push_back(str2[j-1]);
                for (int k=0; k<segment_weight_rownames.length(); k++){
                  std::string my1 = as<std::string>(segment_weight_rownames[k]);
                  int myA = std::stoi(my1);
                  if (typeChange(0)==myA){
                    //Rcout << myA << std::endl;
                    //Rcout << typeChange(0) << std::endl;
                  }
                  if (typeChange(0)==myA) str1_index = k;
                }
                for (int k=0; k<segment_weight_rownames.length(); k++){
                  std::string my2 = as<std::string>(segment_weight_rownames[k]);
                  int myB = std::stoi(my2);
                  if (typeChange(1)==myB){
                    //Rcout << myB << std::endl;
                    //Rcout << typeChange(1) << std::endl;
                  }
                  if (typeChange(1)==myB) str2_index = k;
                }
                cost = segment_weights2(str1_index, str2_index);
                tmp[0]=d(i-1,j)+1;
                tmp[1]=d(i,j-1)+1;
                tmp[2]=d(i-1,j-1)+cost;
                if(!checkVowelConsonant(str1[i-1], str2[j-1])){
                  forbid(i,j)=1;
                }
                d(i,j)=min(tmp);
              }
            }
          }
           */
          if (alignment_normalization){
            int tmpLenStr1=lenStr1;
            int tmpLenStr2=lenStr2;
            while (lenStr1 > 0 || lenStr2 > 0){
              int diaCell=0;
              int leftCell=0;
              int upCell=0;
              if (lenStr1-1>=0&lenStr2-1>=0){
                /*
                if (forbid(lenStr1,lenStr2)==1){
                  d(lenStr1-1,lenStr2-1) = std::numeric_limits<int>::max()-1;
                }
                */
                diaCell = d(lenStr1-1,lenStr2-1);
              } else if(lenStr1-1<0|lenStr2-1<0){
                diaCell=std::numeric_limits<int>::max();
              } else{
                diaCell = d(lenStr1-1,lenStr2-1);
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
