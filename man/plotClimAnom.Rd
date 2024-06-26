% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotClimAnom.R
\name{plotClimAnom}
\alias{plotClimAnom}
\title{plotClimAnom: Plot climate anomalies from baseline}
\usage{
plotClimAnom(
  park = "all",
  years = 1895:format(Sys.Date(), "\%Y"),
  months = 1:12,
  parameter = "tmean",
  averages = "norm20cent",
  legend_position = "none",
  title_type = "UnitCode",
  gridlines = "none"
)
}
\arguments{
\item{park}{Combine data from all parks or one or more parks at a time. Valid inputs:
\describe{
\item{"all"}{Includes all parks in the network}
\item{"LNETN"}{Includes all parks but ACAD}
\item{"ACAD"}{Acadia NP only}
\item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
\item{"MIMA"}{Minute Man NHP only}
\item{"MORR"}{Morristown NHP only}
\item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
\item{"SAGA"}{Saint-Gaudens NHP only}
\item{"SAIR"}{Saugus Iron Works NHS only}
\item{"SARA"}{Saratoga NHP only}
\item{"WEFA"}{Weir Farm NHP only}}}

\item{years}{Numeric. Years to query. Accepted values start at 1895.}

\item{months}{Numeric. Months to query by number. Accepted values range from 1:12.
If specifying new months not yet included in NETN_clim_annual dataset, will
download months that are available from NOAA.}

\item{parameter}{Specify the monthly averaged parameter to plot. Acceptable values are
\describe{
\item{"tmean"}{Plot mean temperature comparisons (in C).}
\item{"tmax"}{Plot max temperature comparisons (in C).}
\item{"tmin"}{Plot min temperature comparisons (in C).}
\item{"ppt"}{Plot precipitation comparisons (in mm).}
}}

\item{averages}{Specify averages to plot. By default, the 20th century normal (1901-2000) plots.
Other options include:
\describe{
\item{"norm20cent"}{Plots the 20th century normal (1901 - 2000)}
\item{"norm1990"}{Plots the 30-year norm from 1991 - 2020}
}}

\item{legend_position}{Specify location of legend. To turn legend off, use legend_position = "none" (Default).
Other options are "top", "bottom", "left", "right".

#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")}

\item{title_type}{Specify whether to label facets with 4-letter UnitCode (default) or full UnitName.
Options are c("UnitCode", "UnitName").}
}
\value{
Returns a ggplot object of specified climate trends
}
\description{
This function produces a trend plot filtered on park, year, month, and
climate parameter and shows anomalies from chosen baseline (1901 - 2000 or 1991 - 2020)
as red for above and blue for below average using gridded NOAA NClim data. If a year x month
combination is specified that doesn't occur yet in NETN_clim_annual dataset, it will be
downloaded if available. New months are typically available within a few weeks of the month end.
If multiple parks are specified, they can either be plotted on the same figure or separate
figures. If multiple parameters are specified, they will be plotted on separate figures.
}
\examples{
\dontrun{


}

}
