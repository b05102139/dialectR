#' Visualize dialect groups with clustering methods
#'
#' Input a distance matrix, upon which clustering will be performed and projected unto a map.
#'
#' @param dist_mat A distance matrix.
#' @param kml_points A dataframe of kml (Keyhole Markup Language) points, as retrieved by \code{\link{get_points}}.
#' @param kml_polygon A dataframe of kml polygons, as retrieved by \code{\link{get_polygons}}.
#' @param cluster_num Number of clusters.
#' @param method The agglomeration method that is passed to \code{\link[stats]{hclust}}. This can be chosen from the following: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#' @return A map upon which dialect areas are clustered.
#' @export
#'
#' @examples
#' # Example 1: A cluster map of Dutch dialects
#' data(distDutch)
#' dutch_points <- get_points(system.file("extdata", "DutchKML.kml", package="dialectR"))
#' dutch_polygons <- get_polygons(system.file("extdata", "DutchKML.kml", package="dialectR"))
#' cluster_map(distDutch[1:100,1:100], dutch_points, dutch_polygons, 5, "ward.D2")
cluster_map <- function(dist_mat, kml_points, kml_polygon, cluster_num, method, validation_method = c("none", "bootstrap", "noise"), resampling_runs = NULL){
  validation_method <- match.arg(validation_method)
  cluster_groups <- get_clusters(dist_mat, cluster_num, method, validation_method, resampling_runs)

  full_df <- merge(cluster_groups,
                kml_points,
                by.x="area",
                by.y="name")

  kml_points <- dplyr::select(kml_points, longitude, latitude)
  colnames(full_df) <- c("area", "grouping", "x", "y")
  colnames(kml_polygon) <- c("group", "longitude", "latitude")
  kml_polygon <-  dplyr::select(kml_polygon, longitude, latitude, group)
  unique_indices <- !deldir::duplicatedxy(full_df[,3:4])
  full_df <- full_df[unique_indices,]

  ggplot2::ggplot(data = full_df,
                  ggplot2::aes(x = x,
                               y = y)) +
    ggvoronoi::geom_voronoi(
      mapping = ggplot2::aes(fill = as.factor(grouping)),
      alpha = 0.8,
      outline = kml_polygon,
      color = "grey") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("longitude") +
    ggplot2::ylab("latitude")
}
