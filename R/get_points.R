#' Get KML points from KML data
#'
#' Input a KML file path to get KML points data
#'
#' @param kml_file_path A file path to a KML file.
#'
#' @return A dataframe with the columns (area) name, longitude, and latitude.
#' @export
#'
#' @examples
#' dutch_points <- get_points(system.file("extdata", "DutchKML.kml", package="dialectR"))
#' dutch_points
get_points <- function(kml_file_path){
  kml_object <- sf::st_read(kml_file_path, quiet = TRUE)
  kml_attributes <- attributes(kml_object$geometry)
  kml_points <- kml_attributes$classes
  kml_names <- kml_object$Name
  kml_points_index <- kml_points %in% "POINT"
  kml_points <- kml_object$geometry[kml_points_index]
  kml_points <- t(sapply(kml_points, function(x){x[1:2]}))
  kml_names <- kml_names[kml_points_index]
  res <- cbind(kml_names, kml_points)
  res <- data.frame(res)
  colnames(res) <- c("name", "longitude", "latitude")
  res[,2] <- as.numeric(res[,2])
  res[,3] <- as.numeric(res[,3])
  res
}
