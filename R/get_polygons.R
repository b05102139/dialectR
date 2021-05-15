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
  
  if (sum(kml_polygons_index) == 1){
    kml_polygons <- kml_object$geometry[kml_polygons_index]
    kml_polygons_all <- kml_polygons[[1]][[1]][,1:2]
    kml_names <- kml_names[kml_polygons_index]
    kml_names <- rep(kml_names, sapply(kml_polygons, function(x){length(x[[1]][,1])}))
    res_poly <- cbind(kml_names, kml_polygons_all)
    res_poly <- data.frame(res_poly)
    colnames(res_poly) <- c("name", "longitude", "latitude")
    res_poly[,2] <- as.numeric(res_poly[,2])
    res_poly[,3] <- as.numeric(res_poly[,3])
    res_poly
  } else if (sum(kml_polygons_index) > 1){
    kml_polygons <- kml_object$geometry[kml_polygons_index]
    kml_polygons_all <- do.call(rbind, sapply(kml_polygons, function(x){x[[1]][,1:2]}))
    kml_names <- kml_names[kml_polygons_index]
    kml_names <- rep(kml_names, sapply(kml_polygons, function(x){length(x[[1]][,1])}))
    res_poly <- cbind(kml_names, kml_polygons_all)
    res_poly <- data.frame(res_poly)
    colnames(res_poly) <- c("name", "longitude", "latitude")
    res_poly[,2] <- as.numeric(res_poly[,2])
    res_poly[,3] <- as.numeric(res_poly[,3])
    res_poly
  }
  kml_polygons_index <- kml_polygons %in% "MULTIPOLYGON"
  if (sum(kml_polygons_index) == 1){
    kml_polygons <- kml_object$geometry[kml_polygons_index]
    kml_polygons_all <-kml_polygons[[1]][[1]][[1]][,1:2]
    kml_names <- kml_names[kml_polygons_index]
    kml_names <- rep(kml_names, sapply(kml_polygons, function(x){length(x[[1]][[1]][,1])}))
    res_multi <- cbind(kml_names, kml_polygons_all)
    res_multi <- data.frame(res_multi)
    colnames(res_multi) <- c("name", "longitude", "latitude")
    res_multi[,2] <- as.numeric(res_multi[,2])
    res_multi[,3] <- as.numeric(res_multi[,3])
    res_multi
  } else if (sum(kml_polygons_index) > 1){
    kml_polygons <- kml_object$geometry[kml_polygons_index]
    kml_polygons_all <- do.call(rbind, sapply(kml_polygons, function(x){x[[1]][[1]][,1:2]}))
    kml_names <- kml_names[kml_polygons_index]
    kml_names <- rep(kml_names, sapply(kml_polygons, function(x){length(x[[1]][[1]][,1])}))
    res_multi <- cbind(kml_names, kml_polygons_all)
    res_multi <- data.frame(res_multi)
    colnames(res_multi) <- c("name", "longitude", "latitude")
    res_multi[,2] <- as.numeric(res_multi[,2])
    res_multi[,3] <- as.numeric(res_multi[,3])
    res_multi
  }
  if (exists("res_poly") & exists("res_multi")){
    res <- rbind(res_poly, res_multi)
  } else if (exists("res_poly")){
    res <- res_poly
  } else if (exists("res_multi")){
    res <- res_multi
  }
  if (!exists("res")){
    res <- NULL
  }
  res
}
