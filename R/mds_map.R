#' Visualize dialect continua with MDS maps
#'
#' Input a distance matrix and kml data, where multidimensional scaling will be applied on the former and projected onto a map.
#'
#' @param dist_mat A distance matrix.
#' @param kml_points A dataframe of kml points, as retrieved by \code{\link{get_points}}.
#' @param kml_polygon A dataframe of kml polygons, as retrieved by \code{\link{get_polygons}}.

#' @return A map upon which the results of multidimensional scaling are projected upon.
#' @export
#'
#' @examples
#' # Example 1: An MDS map of Dutch dialects
#' data(distDutch)
#' dutch_points <- get_points(system.file("extdata", "DutchKML.kml", package="dialectR"))
#' dutch_polygons <- get_polygons(system.file("extdata", "DutchKML.kml", package="dialectR"))
#' mds_map(distDutch, dutch_points, dutch_polygons)
mds_map <- function(dist_mat, kml_points, kml_polygon){
  dist_mds <- stats::cmdscale(dist_mat, k = 3)
  x <- dist_mds[,1]
  y <- dist_mds[,2]
  z <- dist_mds[,3]

  x_scaled <- sapply(seq_along(x), function(i){
    x_diff <- max(x) - min(x)
    ((x[i] - min(x)) / x_diff)
  })
  y_scaled <- sapply(seq_along(y), function(i){
    y_diff <- max(y) - min(y)
    ((y[i] - min(y)) / y_diff)
  })
  z_scaled <- sapply(seq_along(z), function(i){
    z_diff <- max(z) - min(z)
    ((z[i] - min(z)) / z_diff)
  })

  xyz_color <- sapply(seq_along(x_scaled), function(i){
    grDevices::rgb(x_scaled[i], y_scaled[i], z_scaled[i], max=1)
  })

  mds_color_df <- cbind(kml_points, xyz_color)
  kml_points <- dplyr::select(kml_points, longitude, latitude)
  colnames(kml_points) <- c("x", "y")
  colnames(kml_polygon) <- c("group", "longitude", "latitude")
  kml_polygon <- dplyr::select(kml_polygon, longitude, latitude, group)
  unique_indices <- !deldir::duplicatedxy(kml_points)

  mds_color_df <- mds_color_df[unique_indices,]

  ggplot2::ggplot(data = mds_color_df,
                  ggplot2::aes(x = longitude,
                               y = latitude)) +
    ggvoronoi::geom_voronoi(
      mapping = ggplot2::aes(fill = xyz_color),
      alpha = 0.85,
      outline = kml_polygon,
      #color = "white",
      size = 0.2
    ) +
    ggplot2::scale_fill_identity()
}
