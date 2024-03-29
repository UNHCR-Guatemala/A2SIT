% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_results.R
\name{f_rebuild_index}
\alias{f_rebuild_index}
\title{Regenerate results}
\usage{
f_rebuild_index(coin, w = NULL, agg_method)
}
\arguments{
\item{coin}{The coin}

\item{w}{can either be a named list with names as iCodes and values
as the new weights, OR as a data frame with columns "iCode" and "Weight" with
codes and corresponding weights.}

\item{agg_method}{One of \code{c("a_amean", "a_gmean", "a_hmean")} currently supported.}
}
\value{
Updated coin
}
\description{
Regenerates results based on specified weights and aggregation method.
}
\details{
The weights \code{w} can either be a named list with names as iCodes and values
the new weights, OR as a data frame with columns "iCode" and "Weight" with
codes and corresponding weights.
In both cases a subset of weight-code pairs can be specified.
E.g. \code{list("Salud" = 0.5, Amenazas = 0.8)}.

OR set \code{w = "equal"} for equal weights everywhere, or \code{w = "original"} to use the
weights that were input with the input data.

Remember that weights are relative within aggregation groups.
}
