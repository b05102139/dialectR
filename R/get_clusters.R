#' Clustering groups returned as dataframe
#'
#' Input a distance matrix and returns a dataframe with two columns: area and clustering grouping, where a choice of clustering method is provided.
#'
#' @param dist_mat A distance matrix.
#' @param cluster_num Number of clusters.
#' @param method The agglomeration method that is passed to \code{\link[stats]{hclust}}. This can be chosen from the following: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#' @return A map upon which dialect areas are clustered.
#'
#' @return A dataframe with the two columns area and (clustering) grouping.
#' @export
#'
#' @examples
#' # Example 1:
#' data(distDutch)
#' get_clusters(distDutch, 5 ,"ward.D2")
get_clusters <- function(dist_mat, cluster_num, method, validation_method = c("none", "bootstrap", "noise"), resampling_runs = NULL){
  validation_method <- match.arg(validation_method)
  if (validation_method=="bootstrap"){
    #dist_mat[upper.tri(dist_mat)] <- NA
    #dist_mat <- stats::as.dist(dist_mat)
    clustered_dist <- fpc::clusterboot(dist_mat,
                                       B=resampling_runs,
                                       bootmethod = "boot",
                                       clustermethod = hclustCBI,
                                       k=cluster_num,
                                       method = method)
    cluster_groups <- tibble::rownames_to_column(as.data.frame(clustered_dist$partition))
  } else if (validation_method=="noise"){
    #dist_mat[upper.tri(dist_mat)] <- NA
    #dist_mat <- stats::as.dist(dist_mat)
    clustered_dist <- fpc::clusterboot(dist_mat,
                                       B=resampling_runs,
                                       bootmethod = "noise",
                                       clustermethod = hclustCBI,
                                       k=cluster_num,
                                       method = method)
    cluster_groups <- tibble::rownames_to_column(as.data.frame(clustered_dist$partition))
  }
  else if (validation_method=="none"){
    dist_mat[upper.tri(dist_mat)] <- NA
    dist_mat <- stats::as.dist(dist_mat)
    clustered_dist <- stats::hclust(dist_mat, method = method)
    cluster_groups <- tibble::rownames_to_column(
      as.data.frame(stats::cutree(clustered_dist, k = cluster_num)))
  }
  colnames(cluster_groups) <- c("area", "grouping")
  cluster_groups
}
