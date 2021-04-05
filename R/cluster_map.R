#' Title
#'
#' @param dist_mat 
#' @param kml_points 
#' @param kml_polygon 
#' @param cluster_num 
#' @param method 
#' @importFrom magrittr %>%
#' @import ggplot2
#' @return
#' @export
#'
#' @examples
cluster_map <- function(dist_mat, kml_points, kml_polygon, cluster_num, method){

  cluster_groups <- get_clusters(dist_mat, cluster_num, method)

  full_df <- merge(cluster_groups,
                kml_points,
                by.x="area",
                by.y="name")

  kml_points <- kml_points %>%
    dplyr::select(longitude, latitude)
  colnames(full_df) <- c("area", "grouping", "x", "y")
  colnames(kml_polygon) <- c("longitude", "latitude", "group")
  unique_indices <- !deldir::duplicatedxy(full_df[,3:4])
  full_df <- full_df[unique_indices,]

  ggplot(data = full_df,
         aes(x = x,
             y = y)) +
    ggvoronoi::geom_voronoi(
      mapping = aes(fill = as.factor(grouping)),
      alpha = 0.8,
      outline = kml_polygon,
      color = "grey") +
    theme(legend.position = "none") +
    xlab("longitude") +
    ylab("latitude")
}


