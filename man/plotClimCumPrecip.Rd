% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotClimCumPrecip.R
\name{plotClimCumPrecip}
\alias{plotClimCumPrecip}
\title{plotClimCumPrecip: Plot cumulative precip relative to historic}
\usage{
plotClimCumPrecip(
  park = "all",
  years = format(Sys.Date(), "\%Y"),
  months = 1:12,
  averages = "norm20cent",
  plot_title = TRUE,
  title_type = "UnitCode",
  legend_position = "right",
  numcol = 3,
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

\item{years}{Numeric. Years to plot separately. Accepted values start at 2006.If multiple years
specified, will facet results on year. Default is current year.}

\item{averages}{Specify averages to plot. By default, the 20th century normal (1901-2000) plots.
Other options include:
\describe{
\item{"norm20cent"}{Plots the 20th century normal (1901 - 2000)}
\item{"norm1990"}{Plots the 30-year norm from 1991 - 2020}
}}

\item{plot_title}{Logical. If TRUE (default) prints park name at top of figure. If FALSE,
does not print site name. Only enabled when one park is selected.}

\item{title_type}{Specify whether to label facets with 4-letter UnitCode (default) or full UnitName.
Options are c("UnitCode", "UnitName").}

\item{legend_position}{Specify location of legend. To turn legend off, use legend_position =
"none" (Default). Other options are "top", "bottom", "left", "right".}

\item{numcol}{Specify number of columns in the facet wrap, which is only enabled when either multiple years
are specified or multiple parks. Default is 3.}

\item{gridlines}{Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")}
}
\value{
Returns a ggplot object of specified climate trends
}
\description{
This function plots cumulative monthly precipitation for a given year compared
to either the 19th century or 30-year normal. If multiple parks or years are specified, resulting
plots will be faceted on those variables.
}
\examples{
\dontrun{


}

}
