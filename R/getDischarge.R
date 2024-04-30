#' @title getDischarge: query NETN water discharge data
#'
#' @description Queries NETN water discharge data by site, year, month. Only works with site_type = 'stream'.
#'
#' @importFrom dplyr filter left_join
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
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param method Query data by discharge method. Accepted values are c("ACAD Flowtracker", "ACAD Pygmy",
#' "Flume", "LNETN Flowtracker", "LNETN Pygmy", "No Measurement", "Rating curve estimate", "Timed float",
#' "USGS Gage", "Visual estimate", "Volumetric")
#'
#' @param rating Filter on measurement rating.
#' \describe{
#' \item{"E"}{Excellent}
#' \item{"G"}{Good}
#' \item{"F"}{Fair}
#' \item{"P"}{Poor}
#' }
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @return Data frame of Discharge data.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # get discharge for all sites in ROVA from 2021-2023
#' mabi <- getDischarge(park = "ROVA", years = 2021:2023)
#'
#' # get discharge for ACAD streams in July 2023
#' sara <- getDischarge(park = "ACAD", years = 2023, months = 7)
#'
#' # get discharge measured with flowtracker
#' flow <- getDischarge(method = c("ACAD Flowtracker", "LNETN Flowtracker"))
#'
#' # get excellent rated measurements only
#' exc <- getDischarge(rating = "E")
#'
#' @export

getDischarge <- function(park = "all", site = "all",
                     #site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"),
                     months = 5:10, method = 'all',
                     rating = c('all', "E", "G", "F", "P"),
                     output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  #site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  method <- match.arg(method, several.ok = TRUE,
                      c("all", "ACAD Flowtracker", "ACAD Pygmy", "Flume", "LNETN Flowtracker",
                        "LNETN Pygmy", "No Measurement", "Rating curve estimate", "Timed float",
                        "USGS Gage", "Visual estimate", "Volumetric"))
  rating <- match.arg(rating, several.ok = T)
  output <- match.arg(output)


  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({dis <- get("Discharge_Data", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Add year, month and day of year column to dataset and fix data types
  dis$year <- as.numeric(substr(dis$EventDate, 1, 4))
  dis$month <- as.numeric(substr(dis$EventDate, 6, 7))
  dis$doy <- as.numeric(strftime(dis$EventDate, format = "%j"))
  dis$IsEventCUI <- as.logical(dis$IsEventCUI)
  dis$TotalArea_sqft <- as.numeric(gsub("NA", NA_real_, dis$TotalArea_sqft))
  dis$AvgVel_fs <- as.numeric(gsub("NA", NA_real_, dis$AvgVel_fs))
  dis$Discharge_cfs <- as.numeric(gsub("NA", NA_real_, dis$Discharge_cfs))

  # Fidis# Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = 'stream'))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = 'stream',
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode)

  dis2 <- dis |> filter(SiteCode %in% sites)
  dis3 <- left_join(evs, dis2, by = c("SiteCode", "EventDate", "EventCode"))

  # filter by method
  dis4 <-
  if(any(method == "all")){dis3
  } else {filter(dis3, DischargeMethod %in% method)}

  dis5 <-
  if(output == "short"){dis4[,c("SiteCode", "UnitCode", "SubUnitCode", "EventDate",
                                "year", "month", "doy", "ReachType", "FlowStatus",
                                "DischargeMethod", "TotalArea_sqft", "AvgVel_fs",
                                "VelocityFlag", "Discharge_cfs", "DischargeFlag",
                                "MeasurementRating", "Comments")]
    } else {dis4}

  dis6 <-
    if(any(rating == "all")){dis5
      } else {filter(dis5, MeasurementRating %in% rating)}

  if(nrow(dis6) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(dis6))

  }



