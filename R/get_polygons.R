#' Get KML polygon from KML data
#'
#' Input a KML file path to get KML polygon data
#'
#' @param kml_file_path A file path to a KML file.
#'
#' @return A dataframe with the columns (area) name, longitude, and latitude.
#' @export
#'
#' @examples
#' dutch_polygons <- get_polygons(system.file("extdata", "DutchKML.kml", package="dialectR"))
#' dutch_polygons
get_polygons <- function(kml_file_path){
  kml_object <- sf::st_read(kml_file_path, quiet = TRUE)
  kml_attributes <- attributes(kml_object$geometry)
  kml_polygons <- kml_attributes$classes
  kml_names <- kml_object$Name
  kml_polygons_index <- kml_polygons %in% "POLYGON"
  kml_polygons <- kml_object$geometry[kml_polygons_index]
  kml_polygons_all <- do.call(rbind, sapply(kml_polygons, function(x){x[[1]][,1:2]}))
  kml_names <- kml_names[kml_polygons_index]
  kml_names <- rep(kml_names, sapply(kml_polygons, function(x){length(x[[1]][,1])}))
  res <- cbind(kml_names, kml_polygons_all)
  colnames(res) <- c("name", "longitude", "latitude")
  res
}
