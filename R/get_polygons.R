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
  polygons <- tidykml::kml_polygons(kml_file_path)
  polygonsSelect <- dplyr::select(polygons, name, longitude, latitude)
  as.data.frame(polygonsSelect)
}
