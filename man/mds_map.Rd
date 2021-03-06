% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mds_map.R
\name{mds_map}
\alias{mds_map}
\title{Visualize dialect continua with MDS maps}
\usage{
mds_map(dist_mat, kml_points, kml_polygon)
}
\arguments{
\item{dist_mat}{A distance matrix.}

\item{kml_points}{A dataframe of kml points, as retrieved by \code{\link{get_points}}.}

\item{kml_polygon}{A dataframe of kml polygons, as retrieved by \code{\link{get_polygons}}.}
}
\value{
A map upon which the results of multidimensional scaling are projected upon.
}
\description{
Input a distance matrix and kml data, where multidimensional scaling will be applied on the former and projected onto a map.
}
\examples{
# Example 1: An MDS map of Dutch dialects
data(distDutch)
dutch_points <- get_points(system.file("extdata", "DutchKML.kml", package="dialectR"))
dutch_polygons <- get_polygons(system.file("extdata", "DutchKML.kml", package="dialectR"))
mds_map(distDutch, dutch_points, dutch_polygons)
}
