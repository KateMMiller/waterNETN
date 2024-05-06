#' @title getDischarge: query NETN water discharge data
#'
#' @description Queries NETN water discharge data by site, year, month. Only works with site_type = 'stream'.
#'
#' @importFrom dplyr filter left_join
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
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param method Query data by discharge method. Accepted values are c("Flowtracker", "Pygmy",
#' "Flume", "No Measurement", "Rating curve estimate", "Timed float",
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
#'}
#' @export

getDischarge <- function(park = "all", site = "all",
                     #site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"),
                     months = 5:10, active = TRUE, method = 'all',
                     rating = c('all', "E", "G", "F", "P"),
                     output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  #site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  method <- match.arg(method, several.ok = TRUE,
                      c("all", "Flowtracker", "Pygmy", "Flume", "Flowtracker",
                        "Pygmy", "No Measurement", "Rating curve estimate", "Timed float",
                        "USGS Gage", "Visual estimate", "Volumetric"))
  rating <- match.arg(rating, several.ok = T)
  output <- match.arg(output)

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({dis <- get("Discharge_Data", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # fix data types
  # char fixes
  chr_cols <- c("SubUnitCode", "SubUnitName", "ReachType", "FlowStatus", "DischargeMethod",
                "VelocityFlag", "DischargeFlag", "MeasurementRating", "Comments")
  dis[,chr_cols][dis[,chr_cols] == "NA"] <- NA_character_

  # numeric fixes
  num_cols <- c("TotalArea_sqft", "AvgVel_fs", "Discharge_cfs")
  dis[,num_cols][dis[,num_cols] == "NA"] <- NA_real_
  dis[,num_cols] <- apply(dis[,num_cols], 2, function(x) as.numeric(x))

  # logic fixes
  dis$IsEventCUI <- as.logical(dis$IsEventCUI)

  # Add year, month and day of year column to dataset
  dis$year <- as.numeric(substr(dis$EventDate, 1, 4))
  dis$month <- as.numeric(substr(dis$EventDate, 6, 7))
  dis$doy <- as.numeric(strftime(dis$EventDate, format = "%j"))

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = 'stream', active = active))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = 'stream', active = active,
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode)

  dis2 <- dis |> filter(SiteCode %in% sites)
  dis3 <- left_join(evs, dis2, by = c("SiteCode", "EventDate", "EventCode"))

  dis3$DischargeMethod[dis3$DischargeMethod %in% c("ACAD Pygmy", "LNETN Pygmy")] <- "Pygmy"
  dis3$DischargeMethod[dis3$DischargeMethod %in% c("ACAD Flowtracker", "LNETN Flowtracker")] <- "Flowtracker"

  # filter by method
  dis4 <-
  if(any(method == "all")){dis3
  } else {filter(dis3, DischargeMethod %in% method)}

  dis5 <-
  if(output == "short"){dis4[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode",
                                "EventDate","EventCode",
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



