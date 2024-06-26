% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_results.R
\name{f_get_admin2_boundaries}
\alias{f_get_admin2_boundaries}
\title{Get admin2 polygons from API}
\usage{
f_get_admin2_boundaries(ISO3, simplify = TRUE, dTolerance = 500)
}
\arguments{
\item{ISO3}{ISO3 code of country}

\item{simplify}{Logical: whether to simplify or not}

\item{dTolerance}{parameter passed to \code{\link[sf:geos_unary]{sf::st_simplify()}}}
}
\value{
sf object
}
\description{
Queries the gis.unhcr.org sever to return admin2 shape files for a specific
country. Some of the files returned are heavy, and the server also seems to
fail or time out every now and then. Also, some admin2 codes are not in the
expected format.
}
\details{
This function also optionally tries to simplify the geometry using the sf
package, this doesn't always work though.
}
