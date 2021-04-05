vc_leven_align_df <- function(dialect_data, remove.duplicates = FALSE){
  dialect_mat <- pbapply::pblapply(seq(from = 1,
                                       to = length(dialect_data[,1])-1),
                                   function(x){lapply(seq(from = x+1,
                                                          to = length(dialect_data[,1])),
                                                      function(y){
                                                        vc_leven_align(dialect_data[x,],
                                                                       dialect_data[y,])})})

  align_results <- unlist(dialect_mat, recursive = F) %>%
    unlist(recursive = F) %>%
    do.call(rbind, .) %>%
    data.frame(stringsAsFactors = F)

  colnames(align_results) <- c("W1", "W2")

  if (isTRUE(remove.duplicates)){
    align_results  %>%
      dplyr::filter(W1 != W2)
  } else{
    align_results
  }
}
