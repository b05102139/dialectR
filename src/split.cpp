#include <Rcpp.h>
using namespace Rcpp;

StringVector split(std::string s, std::string delimiter) {
  size_t posStart = 0, posEnd, delimLen = delimiter.length();
  String word;
  StringVector res;
  
  while ((posEnd = s.find (delimiter, posStart)) != std::string::npos) {
    word = s.substr (posStart, posEnd - posStart);
    posStart = posEnd + delimLen;
    res.push_back(word);
  }
  
  res.push_back(s.substr(posStart));
  return res;
}
