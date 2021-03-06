% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCGLS-functions.R
\name{ncvar_get_CGSL}
\alias{ncvar_get_CGSL}
\title{Read netcdf CGLS data}
\usage{
ncvar_get_CGSL(date, product, resolution, version, variable)
}
\arguments{
\item{date}{Date of interest, for example for 13 june 2019: 2019-06-13}

\item{product}{Product name: fapar, fcover, lai, ndvi,  ss, swi, lst, ...}

\item{resolution}{1km, 300m or 100m}

\item{version}{Version number: v1, v2, v3,...}

\item{variable}{FAPAR_ERR, FAPAR_QFLAG... Also see https://land.copernicus.eu/global/products/}
}
\value{
CGLS data Large matrix of a specific variable in environment, coordinates adjusted.
}
\description{
Read single layers of Copernicus Global Land Service (CGLS) data and adjusts coordinates for R.
}
\details{
Adjusting coordinates is a necessary step to use the data because Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner. This function opens the data without any corrections.
}
\examples{
\dontrun{
#' library(ncdf4)
DATE <- "2019-06-13" #Date of interest, for example for 13 june 2019: 2019-06-13
PROD <- "fapar" #Product name: fapar, fcover, lai, ndvi,  ss, swi, lst, ...
RES  <- "1km" #1km, 300m or 100m
V    <- "v1" #Version number: v1, v2, v3, ...
VAR  <- "FAPAR" #FAPAR_ERR, FAPAR_QFLAG... Also see https://land.copernicus.eu/global/products/

nc_data <- ncvar_get_CGSL (date=DATE, product=PROD, resolution=RES, version=V, variable=VAR)
}
}
