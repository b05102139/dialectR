#' Distance matrix for Dialectometry
#'
#' Computes a distance matrix between dialect varieties, the results of which may be used for further analyses and plotting.
#'
#' @param dialect_data A dataframe of dialect data, transcribed in the International Phonetic Alphabet.
#' @param funname The distance metric to be used. This can be chosen from the following: "leven", "vc_leven".
#' @param alignment_normalization A logical value, indicating whether or not the distance scores should be normalized by alignment length.
#' @param delim An optional delimiter, in situations where multiple responses exist in the data.
#' @return A distance matrix, where the values are the difference between dialects based on edit distance.
#' @examples
#' data(Dutch)
#' Dutch <- Dutch[1:3,1:3]
#' distance_matrix(Dutch, alignment_normalization = TRUE)
distance_matrix <- function(dialect_data, funname, alignment_normalization = FALSE, delim = NULL){
  if(!is.matrix(dialect_data)){
    dialect_data <- as.matrix(dialect_data)
  }
  res <- .distance_matrix_internal(dialect_data, funname, alignment_normalization, delim)
  original_rownames <- rownames(dialect_data)
  rownames(res) <- original_rownames
  colnames(res) <- original_rownames
  res
}
