% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_ui.R
\name{country_dropdown}
\alias{country_dropdown}
\title{Generate country dropdown menu}
\usage{
country_dropdown(id, label)
}
\arguments{
\item{id}{ID to assign}

\item{label}{Label}
}
\value{
HTML
}
\description{
Generates a dropdown menu of countries where we have available admin2 geometry
stored in inst/geom. Adds flags.
}
