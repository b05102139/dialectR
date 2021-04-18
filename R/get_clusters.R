get_clusters <- function(dist_mat, cluster_num, method){
  dist_mat[upper.tri(dist_mat)] <- NA
  dist_mat <- as.dist(dist_mat)
  clustered_dist <- hclust(dist_mat, method = method)
  cluster_groups <- tibble::rownames_to_column(
    as.data.frame(cutree(clustered_dist, k = cluster_num)))
  colnames(cluster_groups) <- c("area", "grouping")
  cluster_groups
}
