#' @title getEvents: query NETN water data by site and event
#'
#' @description Queries NETN water data by site and event. Returned data frame can include lakes or streams, but only includes
#' columns that the Site_Lake and Site_Stream views have in common.
#'
#' @importFrom dplyr filter
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"ACAD"}{Acadia NP only}
#' \item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
#' \item{"MIMA"}{Minute Man NHP only}
#' \item{"MORR"}{Morristown NHP only}
#' \item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
#' \item{"SAGA"}{Saint-Gaudens NHP only}
#' \item{"SAIR"}{Saugus Iron Works NHS only}
#' \item{"SARA"}{Saratoga NHP only}
#' \item{"WEFA"}{Weir Farm NHP only}}
#'
#' @param site Filter on 6-letter SiteCode (e.g., "ACABIN", "MORRSA", etc.). Easiest way to pick a site. Defaults to "all".
#'
#' @param site_type Combine all site types, lakes or streams. Not needed if specifying particular sites.
#' \describe{
#' \item{"all"}{Default. Includes all site types, unless site or site_name select specific site types.}
#' \item{"lake"}{Include only lakes.}
#' \item{"stream"}{Include streams only.}
#' }
#'
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#'
#' @return Data frame of event info
#'
#'@examples
#' \dontrun{
#' importData()
#'
#' # get events for all sites in MABI from 2021-2023
#' mabi <- getEvents(park = "MABI", years = 2021:2023)
#'
#' # get events for SARA sites sampled in 2019 and 2023
#' sara <- getEvents(park = "SARA", years = c(2019, 2023))
#'
#' # get events for MIMA and SAIR
#' ma_parks <- getEvents(park = c("SAIR", "MIMA"))
#'
#' # get info for all ACAD lakes sampled in April
#' ACAD_lake4<- getEvents(park = 'ACAD', site_type = 'lake', months = 4)
#'
#' # get site info for 2 streams in MORR with full output
#' morr_sites <- getEvents(site = c("MORRSA", "MORRSB"), output = 'verbose')
#' }
#' @export

getEvents <- function(park = "all", site = "all",
                     site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"),
                     months = 5:10, output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  output <- match.arg(output)

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({events <- get("Event_Info", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Add year, month and day of year column to dataset
  events$year <- as.numeric(substr(events$EventDate, 1, 4))
  events$month <- as.numeric(substr(events$EventDate, 6, 7))
  events$doy <- as.numeric(strftime(events$EventDate, format = "%j"))

  # Check that a valid site code was used
  site_check <- getSites()[, c("SiteCode")]
  site <- tryCatch(match.arg(site, several.ok = TRUE, c("all", unique(site_check))),
                   error = function(e){stop(
                     paste0(
                     "Specified site does not match an accepted site code.
                     Acceptable codes are: ",
                     "\n",
                     paste0(sort(site_check), collapse = "\n")))}
                   )

  #-- Filter site info --
  # make vector of sites to filter on from getSites
  sites <- getSites(park = park, site = site, site_type = site_type)[,"SiteCode"]
  evs1 <- filter(events, SiteCode %in% sites)

  # filter by years and months
  evs2 <- filter(evs1, year %in% years)
  evs3 <- filter(evs2, month %in% months)

  # data cleanup
  evs3$IsEventCUI <- as.logical(evs3$IsEventCUI)
  evs3$SiteLatitude <- as.numeric(evs3$SiteLatitude)
  evs3$SiteLongitude <- as.numeric(evs3$SiteLongitude)

  # finalize output
  evs_final <- if(output == "short"){
    evs3[,c("UnitCode", "SiteCode", "SiteName", "SiteType",
            "SiteLatitude", "SiteLongitude",
            "EventDate", "year", "month", "doy")]
    } else {evs3}


  if(nrow(evs_final) == 0){stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(evs_final)

  }



