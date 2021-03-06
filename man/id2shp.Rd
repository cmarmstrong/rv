% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/id2shp.R, R/main.R
\docType{package}
\name{id2shp}
\alias{id2shp}
\title{id2shp: Retrieve vector data given ID strings}
\usage{
id2shp(
  country = NULL,
  hasc = NULL,
  fips = NULL,
  cb = TRUE,
  resolution = "20m",
  buffer = 0,
  returnclass = "sp",
  scale = "large",
  ...
)
}
\arguments{
\item{country}{A country name identified in naturalearth country names}

\item{hasc}{A HASC}

\item{fips}{A FIPS code}

\item{cb}{Logical.  Applies only to FIPS codes.  If true will return a
generalized resolution cartographic boundary vector, otherwise returns a
higher resolution vector.  Default TRUE.}

\item{resolution}{The resolution if cb is TRUE.  Applies only with argument
`FIPS'.  Options are 500k, 5m, and 20m for 1:500k, 1:5 million, and 1:20
million.}

\item{buffer}{Integer.  If greater than zero will buffer the vector data to
the value of this argument.  Will not work with `returnclass' set to 'sf'.}

\item{returnclass}{set the class of the returned geometry.  Accepts a value
of either `sf' or `sp'.}

\item{scale}{The scale of the vector data.  Applies only with arguments
`country' or `hasc'.  Options are 'small', 'medium', or 'large'.}

\item{...}{Additional arguments, unused.}
}
\value{
A vector for the administrative boundary identified by the location
  code
}
\description{
This package downloads administrive boundaries and other vector data from
Natural Earth and TIGER

Accepts a place code and returns an administrative boundary vector.  This
function will service place codes from rnaturalearth place names, Federal
Information Processing Standard (FIPS) codes, and hierarchical administrative
subdivision codes (HASC).  If no names or codes are given, returns an
\code{\link[raster]{extent}} for the globe in longitude/latitude.
}
\details{
Vector data is downloaded using rnaturalearth (place names and HASC) or
tigris (FIPS codes)
}
\section{Functions}{

\code{\link{id2shp}}
}

\seealso{
\code{\link[rnaturalearth]{ne_countries}}
  \code{\link[rnaturalearth]{ne_states}} \code{\link[tigris]{counties}}
  \code{\link[countrycode]{countrycode}}
}
