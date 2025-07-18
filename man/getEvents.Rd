% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEvents.R
\name{getEvents}
\alias{getEvents}
\title{getEvents: query NETN water data by site and event}
\usage{
getEvents(
  park = "all",
  site = "all",
  site_type = c("all", "lake", "stream"),
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

\item{site}{Filter on 6-letter SiteCode (e.g., "ACABIN", "MORRSA", etc.). Easiest way to pick a site. Defaults to "all".}

\item{site_type}{Combine all site types, lakes or streams. Not needed if specifying particular sites.
\describe{
\item{"all"}{Default. Includes all site types, unless site or site_name select specific site types.}
\item{"lake"}{Include only lakes.}
\item{"stream"}{Include streams only.}
}}

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
Data frame of event info
}
\description{
Queries NETN water data by site and event. Returned data frame can include lakes or streams, but only includes
columns that the Site_Lake and Site_Stream views have in common.
}
\examples{
\dontrun{
importData()

# get events for all sites in MABI from 2021-2023
mabi <- getEvents(park = "MABI", years = 2021:2023)

# get events for SARA sites sampled in 2019 and 2023
sara <- getEvents(park = "SARA", years = c(2019, 2023))

# get events for MIMA and SAIR
ma_parks <- getEvents(park = c("SAIR", "MIMA"))

# get info for all ACAD lakes sampled in April
ACAD_lake4 <- getEvents(park = 'ACAD', site_type = 'lake', months = 4, event_type = 'all')

# get site info for 2 streams in MORR with full output
morr_sites <- getEvents(site = c("MORRSA", "MORRSB"), output = 'verbose')
}
}
