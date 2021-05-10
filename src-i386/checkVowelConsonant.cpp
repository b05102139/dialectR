#include <Rcpp.h>

using namespace Rcpp;

bool checkVowelConsonant(int w1, int w2){
  bool res = false;
  IntegerVector vowel = {105, 121, 616, 649, 623, 117, 618, 655, 650, 101, 248, 600, 629, 612, 111, 601, 603, 339, 604, 606, 652, 596, 592, 230, 97, 630, 593, 594, 650};
  IntegerVector consonant = {112, 98, 116, 100, 648, 598, 99, 607, 107, 103, 113, 610, 660, 109, 625, 110, 627, 626, 331, 628, 665, 114, 640, 11377, 638, 637, 632, 946, 102, 118, 952, 240, 115, 122, 643, 658, 642, 656, 231, 669, 120, 611, 967, 641, 295, 661, 104, 614, 620, 622, 651, 633, 635, 106, 624, 108, 621, 654, 671};
  if(((std::find(vowel.begin(), vowel.end(), w1) != vowel.end()) & (std::find(vowel.begin(), vowel.end(), w2) != vowel.end())) | ((std::find(consonant.begin(), consonant.end(), w1) != consonant.end()) & (std::find(consonant.begin(), consonant.end(), w2) != consonant.end()))){
    res = true;
  }
  return res;
}
