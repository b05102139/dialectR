vowels <- c("i", "y", "ɨ", "ʉ",
            "ɯ", "u", "ɪ", "ʏ",
            "ʊ", "e", "ø", "ɘ",
            "ɵ", "ɤ", "o", "ə",
            "ɛ", "œ", "ɜ", "ɞ",
            "ʌ", "ɔ", "ɐ", "æ",
            "a", "ɶ", "ɑ", "ɒ",
            "ʊ")

consonants <- c("p", "b", "t", "d",
                "ʈ", "ɖ", "c", "ɟ",
                "k", "g", "q", "ɢ",
                "ʔ", "m", "ɱ", "n",
                "ɳ", "ɲ", "ŋ", "ɴ",
                "ʙ", "r", "ʀ", "ⱱ",
                "ɾ", "ɽ", "ɸ", "β",
                "f", "v",	"θ", "ð",
                "s", "z", "ʃ", "ʒ",
                "ʂ", "ʐ", "ç", "ʝ",
                "x", "ɣ", "χ", "ʁ",
                "ħ", "ʕ", "h", "ɦ",
                "ɬ", "ɮ", "ʋ", "ɹ",
                "ɻ", "j", "ɰ", "l",
                "ɭ", "ʎ", "ʟ")

check_vowel_consonant <- function(w1, w2){
  if ((w1 %in% vowels && w2 %in% vowels) || (w1 %in% consonants && w2 %in% consonants))
  {return(TRUE)}
  else
  {return(FALSE)}
}

vc_leven_align <- function(w1, w2){
  w1_len <- nchar(w1)
  w2_len <- nchar(w2)

  if (!is.na(w1_len) && !is.na(w2_len)){

    lev_table <- matrix(nrow = nchar(w1)+1,
                        ncol = nchar(w2)+1)
    lev_table[,1] <- seq(from = 0, to = nchar(w1), by = 1)
    lev_table[1,] <- seq(from = 0, to = nchar(w2), by = 1)

    for (j in 1:w2_len){
      for (i in 1:w1_len){
        if(substr(w1,i,i) == substr(w2,j,j)){
          sub_cost <- 0
        }
        else if(check_vowel_consonant(substr(w1,i,i),
                                      substr(w2,j,j)) == TRUE){
          sub_cost <- 1
        } else{
          sub_cost <- 2
        }
        lev_table[i+1,j+1] <- min(
          lev_table[i,j+1]+1,
          lev_table[i+1,j]+1,
          lev_table[i,j]+sub_cost
        )
      }
    }
    i<-w1_len+1
    j<-w2_len+1
    align1 <- vector()
    align2 <- vector()
    while (i > 0 && j > 0){
      dia_cell<-lev_table[i-1,j-1]
      left_cell<-lev_table[i,j-1]
      up_cell<-lev_table[i-1,j]
      if(identical(dia_cell,numeric(0))){
        dia_cell <- NA
      }
      if(identical(left_cell,numeric(0))){
        left_cell <- NA
      }
      if(identical(up_cell,numeric(0))){
        up_cell <- NA
      }
      direction <- which.min(c(dia_cell, left_cell, up_cell))
      if(direction == 1){
        align1 <- c(align1, substr(w1, i-1,i-1))
        align2 <- c(align2, substr(w2, j-1,j-1))
        i<-i-1
        j<-j-1
      }
      else if(direction==2){
        align1 <- c(align1, "-")
        align2 <- c(align2, substr(w2, j-1,j-1))
        j<-j-1
      }
      else if(direction==3){
        align1 <- c(align1, substr(w1, i-1,i-1))
        align2 <- c(align2, "-")
        i<-i-1
      }
      if(i==1 && j==1){break}
    }
    matrix(c(align1 %>% rev,
             align2 %>% rev), ncol = 2)
  }
}

vc_leven_align <- Vectorize(vc_leven_align,
                            vectorize.args = c("w1", "w2"),
                            SIMPLIFY = F,
                            USE.NAMES = F)

