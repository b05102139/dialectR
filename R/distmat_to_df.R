distmat_to_df <- function(dist_matrix){
  data.frame(row=rownames(dist_matrix)[row(dist_matrix)[upper.tri(dist_matrix)]],
             col=colnames(dist_matrix)[col(dist_matrix)[upper.tri(dist_matrix)]],
             sim=dist_matrix[upper.tri(dist_matrix)])
}

