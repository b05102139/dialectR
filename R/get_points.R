get_points <- function(kml_file_path){
  tidykml::kml_points(kml_file_path) %>%
    dplyr::select(name, longitude, latitude) %>%
    as.data.frame()
}