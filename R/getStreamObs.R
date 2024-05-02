#' @title getStreamObs: query NETN stream observations
#'
#' @description Queries NETN stream observations by park, site, year and month.
#'
#' @importFrom dplyr filter
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
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @return Data frame of stream observations
#'
#'@examples
#' \dontrun{
#' importData()
#'
#' # get stream observations for Pogue Stream all years
#' mabi <- getStreamObs(park = "MABI")
#'
#' # get observations for all streams in ACAD May 2023
#' ACAD_streams <- getStreamObs(park = 'ACAD', years = 2023, months = 5)
#'
#' }
#' @export

getStreamObs <- function(park = "all", site = "all", years = 2006:format(Sys.Date(), "%Y"),
                         months = 5:10, output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  output <- match.arg(output)

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({strobs <- get("StreamSite_Observations", envir = env)
           },
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Fix data types
  # character fixes
  chr_cols <- c("SubUnitCode", "SubUnitName", "WaterClarity", "WaterColor", "WaterCondition",
                "AlgaeSlimeExtent", "AlgaeSlimeAmount", "AlgaeSlimeColor", "AlgaeFilExtent",
                "AlgaeFilColor", "AlgaeClumpsExtent", "AlgaeClumpsColor", "Algae_Notes")
  strobs[,chr_cols][strobs[,chr_cols] == "NA"] <- NA_character_

  # logic fixes
  strobs$IsEventCUI <- as.logical(strobs$IsEventCUI)

  #Add year, month and day of year column to dataset
  strobs$year <- as.numeric(substr(strobs$EventDate, 1, 4))
  strobs$month <- as.numeric(substr(strobs$EventDate, 6, 7))
  strobs$doy <- as.numeric(strftime(strobs$EventDate, format = "%j"))

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = site_type))$SiteCode

  strobs2 <- strobs |> filter(SiteCode %in% sites)
  strobs3 <- strobs2 |> filter(year %in% years) |> filter(month %in% months)

  strobs4 <-
    if(output == "short"){
      strobs3[,c("SiteCode", "UnitCode", "SubUnitCode", "EventDate", "EventCode",
                 "year", "month", "doy", "WaterClarity", "WaterColor",
                 "WaterCondition", "AlgaeSlimeExtent", "AlgaeSlimeAmount",
                 "AlgaeSlimeColor", "AlgaeFilExtent", "AlgaeFilColor", "AlgaeClumpsExtent",
                 "AlgaeClumpsColor", "Algae_Notes")]
    } else {strobs3}

  return(data.frame(strobs4))
  }



