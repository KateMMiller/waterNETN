% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDischarge.R
\name{getDischarge}
\alias{getDischarge}
\title{getDischarge: query NETN water discharge data}
\usage{
getDischarge(
  park = "all",
  site = "all",
  event_type = "VS",
  years = 2006:format(Sys.Date(), "\%Y"),
  months = 5:10,
  active = TRUE,
  method = "all",
  rating = "all",
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

\item{site}{Filter on 6-letter SiteCode (e.g., "ACABIN", "MORRSA", etc.). Easiest way to pick a site. Defaults to "all".}

\item{event_type}{Select the event type. Options available are below Can only choose one option.
\describe{
\item{"all"}{All possible sampling events.}
\item{"VS"}{Default. NETN Vital Signs monitoring events, which includes Projects named 'NETN_LS' and 'NETN+ACID'.}
\item{"acid"}{Acidification monitoring events in Acadia.}
\item{"misc"}{Miscellaneous sampling events.}
}}

\item{years}{Numeric. Years to query. Accepted values start at 2006.}

\item{months}{Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
events are between months 5 and 10, and these are set as the defaults.}

\item{active}{Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.}

\item{method}{Query data by discharge method. Accepted values are c("Flowtracker", "Pygmy",
"Flume", "No Measurement", "Rating curve estimate", "Timed float",
"USGS Gage", "Visual estimate", "Volumetric")}

\item{rating}{Filter on measurement rating. Can choose multiple. Default is all.
\describe{
\item{"all"}{All measurements}
\item{"E"}{Excellent}
\item{"G"}{Good}
\item{"F"}{Fair}
\item{"P"}{Poor}
}}

\item{output}{Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)}
}
\value{
Data frame of Discharge data.
}
\description{
Queries NETN water discharge data by site, year, month. Only works with site_type = 'stream'.
}
\examples{
\dontrun{
importData()

# get discharge for all sites in ROVA from 2021-2023
mabi <- getDischarge(park = "ROVA", years = 2021:2023)

# get discharge for ACAD streams in July 2023
sara <- getDischarge(park = "ACAD", years = 2023, months = 7)

# get discharge measured with Flowtracker
flow <- getDischarge(method = c("Flowtracker"))

# get excellent rated measurements only
exc <- getDischarge(rating = "E")
}
}
