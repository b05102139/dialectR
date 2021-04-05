get_polygons <- function(kml_file_path){
  tidykml::kml_polygons(kml_file_path) %>%
    dplyr::select(name, longitude, latitude) %>%
    as.data.frame()
}