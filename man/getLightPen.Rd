% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLightPen.R
\name{getLightPen}
\alias{getLightPen}
\title{getLightPen: query NETN light penetration data}
\usage{
getLightPen(
  park = "all",
  site = "all",
  event_type = "VS",
  years = 2006:format(Sys.Date(), "\%Y"),
  active = TRUE,
  months = 5:10,
  output = c("short", "verbose")
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

\item{site}{Filter on 6-letter SiteCode (e.g., "ACANTB", "WEFAPA", etc.). Easiest way to pick a site. Defaults to "all".}

\item{event_type}{Select the event type. Options available are below Can only choose one option.
\describe{
\item{"all"}{All possible sampling events.}
\item{"VS"}{Default. NETN Vital Signs monitoring events, which includes Projects named 'NETN_LS' and 'NETN+ACID'.}
\item{"acid"}{Acidification monitoring events in Acadia.}
\item{"misc"}{Miscellaneous sampling events.}
}}

\item{years}{Numeric. Years to query. Accepted values start at 2006.}

\item{active}{Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.}

\item{months}{Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
events are between months 5 and 10, and these are set as the defaults.}

\item{output}{Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)}
}
\value{
Data frame of Light Penetration data.
}
\description{
Queries NETN light penetration data by site, year, month. Designed to only work with site_type = 'lake'.
}
\examples{
\dontrun{
importData()

# get light penetration for the Pogue in MABI from 2021-2023
mabi <- getLightPen(site = "MABIPA", years = 2021:2023)

# get light penetration for all ACAD lakes sampled in August
ACAD_lake <- getLightPen(park = 'ACAD', months = 8)

}
}
