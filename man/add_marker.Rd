% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marker.R
\name{add_marker}
\alias{add_marker}
\title{Choropleth world maps, add secondary country markers}
\usage{
add_marker(iso3 = NA_character_, shape = 17, col = "red", size = 3, lab = "")
}
\arguments{
\item{iso3}{A vector of ISO3 country codes.}

\item{shape}{Marker shape.}

\item{col}{Marker colour.}

\item{size}{Marker size.}

\item{lab}{Marker label.}
}
\value{
A ggplot2 function.
}
\description{
`add_marker()` is used in combination with `whomap()` and adds a secondary
marker located at the centroid of listed countries and territories.
}
\examples{
brics <- data.frame(iso3=c('BRA','CHN','IND','RUS','ZAF'), var=1:5)
whomap(brics, legend.title='BRICS', legend.pos=c(0.14, 0.34)) +
   add_marker(c('BRA','RUS'), lab='Subnational\ndata')

}
\author{
Philippe Glaziou.
}
