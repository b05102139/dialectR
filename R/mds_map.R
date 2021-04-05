mds_map <- function(dist_mat, kml_points, kml_polygon){
  dist_mds <- cmdscale(dist_mat, k = 3)
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
    rgb(x_scaled[i], y_scaled[i], z_scaled[i], max=1)
  })

  mds_color_df <- cbind(kml_points, xyz_color)
  kml_points <- kml_points %>% dplyr::select(longitude, latitude)
  colnames(kml_points) <- c("x", "y")
  colnames(kml_polygon) <- c("longitude", "latitude", "group")
  unique_indices <- !deldir::duplicatedxy(kml_points)

  mds_color_df <- mds_color_df[unique_indices,]

  ggplot(data = mds_color_df,
         aes(x = longitude,
         y = latitude)) +
    ggvoronoi::geom_voronoi(
      mapping = aes(fill = xyz_color),
                            alpha = 0.85,
                            outline = kml_polygon,
                            #color = "white",
                            size = 0.2
      ) +
    scale_fill_identity()
}

# give a mid-way function for the mds values as well
