#' Convert a dialectometric distance matrix to a dataframe
#'
#' Input a distance matrix, with which a dataframe will be returned with the three columns of "row", "col", and "dist". The first two correspond with the rows and columns in the distance matrix, and the last refers to their crossing point, where the distance between them is given.
#'
#' @param dist_matrix A distance matrix.
#'
#' @return A dataframe with the columns "row", "col", and "dist".
#' @export
#'
#' @examples
#' # Example 1: Dutch distance matrix to Dutch dataframe
#' data(distDutch)
#' distmat_to_df(distDutch)
distmat_to_df <- function(dist_matrix){
  data.frame(row=rownames(dist_matrix)[row(dist_matrix)[upper.tri(dist_matrix)]],
             col=colnames(dist_matrix)[col(dist_matrix)[upper.tri(dist_matrix)]],
             dist=dist_matrix[upper.tri(dist_matrix)])
}

