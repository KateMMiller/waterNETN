#' @include getSites.R
#' @include getEvents.R
#'
#' @title getSecchi: query NETN Secchi data
#'
#' @description Queries NETN Secchi data by site, year, month, and observer type. Data are stacked by
#' observer. Only records with observations are returned. Only runs for site_type = 'lake'.
#'
#' @importFrom dplyr all_of filter left_join
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"LNETN"}{Includes all parks but ACAD}
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
#' @param event_type Select the event type. Options available are below Can only choose one option.
#' \describe{
#' \item{"all"}{All possible sampling events.}
#' \item{"VS"}{Default. NETN Vital Signs monitoring events, which includes Projects named 'NETN_LS' and 'NETN+ACID'.}
#' \item{"acid"}{Acidification monitoring events in Acadia.}
#' \item{"misc"}{Miscellaneous sampling events.}
#' }
#'
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @param observer_type Return data from all observers, or only return first observer.
#' Accepted values are c("all", "first", "second"), with "first" being the default.
#'
#' @return Data frame of Secchi data in long form (ie observers stacked).
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # get Secchi depth the Pogue from 2021-2023, first observer only
#' mabi <- getSecchi(site = "MABIPA", years = 2021:2023, observer_type = "first")
#'
#' # get Secchi depth for all ACAD lakes sampled in July for all observers
#' ACAD_lake <- getSecchi(park = 'ACAD', months = 7)
#'
#'}
#' @export

getSecchi <- function(park = "all", site = "all",
                      event_type = "VS",
                      years = 2006:format(Sys.Date(), "%Y"),
                      months = 5:10, active = TRUE,
                      observer_type = "first",
                      output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  event_type <- match.arg(event_type, c("all", "VS", "acid", "misc"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  observer_type <- match.arg(observer_type, c("all", "first", "second"))
  output <- match.arg(output)

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({sec <- get("Secchi_Data", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Make data long, so observations are stacked in single columns
  start_cols <-  c("GroupCode", "GroupName", "UnitCode", "UnitName",
                   "SubUnitCode", "SubUnitName", "SiteCode", "SiteName",
                   "EventDate", "EventCode", "IsEventCUI")

  # Add year, month and day of year column to dataset and fix data types
  sec$year <- as.numeric(substr(sec$EventDate, 1, 4))
  sec$month <- as.numeric(substr(sec$EventDate, 6, 7))
  sec$doy <- as.numeric(strftime(sec$EventDate, format = "%j"))
  sec$IsEventCUI <- as.logical(sec$IsEventCUI)
  sec$Value <- as.numeric(gsub("NA", NA_real_, sec$Value))
  sec$ObsInit <- gsub("NA", NA_character_, sec$ObsInit)
  sec$SD_HitBottom <- as.logical(sec$SD_HitBottom)
  sec$Observer <- as.numeric(gsub('[A-z]', "", sec$Parameter))
  sec$Parameter = "SecchiDepth_m"

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = 'lake', active = active))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = 'lake', active = active, event_type = event_type,
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode, Project)

  sec2 <- sec |> filter(SiteCode %in% sites)
  sec3 <- left_join(evs, sec2, by = c("SiteCode", "EventDate", "EventCode", "Project", "SiteType"))

  sec4 <- if(observer_type == "all"){sec3
  } else if(observer_type == "first"){filter(sec3, Observer %in% 1)
  } else if(observer_type == "second"){filter(sec3, Observer %in% 2)}

  sec5 <- sec4 |> filter(!is.na(Value))

  sec6 <-
  if(output == "short"){sec5[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode",
                                "EventDate", "EventCode", "Project",
                                "year", "month", "doy", "Parameter", "Value",
                                "SD_HitBottom", "Observer")]
    } else {sec4}

  if(nrow(sec6) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(sec6))

  }



