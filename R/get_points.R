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
  points <- tidykml::kml_points(kml_file_path)
  pointsSelect <- dplyr::select(points, name, longitude, latitude)
  as.data.frame(pointsSelect)
}
