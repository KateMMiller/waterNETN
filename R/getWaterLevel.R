#' @include getSites.R
#'
#' @title getWaterLevel: query NETN water level data
#'
#' @description Queries NETN water level data by site, year, and month.
#'
#' @importFrom dplyr  filter inner_join left_join
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
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @return Data frame of water level data
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # Get water level data for Bubble Pond.
#' bubl <- getWaterLevel(site = "ACBUBL", years = 2013:2023)
#'
#' Get water level data for Weir Pond in August.
#' weir <- getWaterLevel(site = "WEFAPA", months = 8)
#'
#'}
#' @export

getWaterLevel <- function(park = "all", site = "all",
                     site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"), active = TRUE,
                     months = 5:10, output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  output <- match.arg(output)
  stopifnot(class(active) == "logical")

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({stage <- get("StageDatum_Info", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  tryCatch({wl <- get("WaterLevel_Data", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Add year, month and day of year column to dataset and fix data types
  # will fix the parameter types later after they're pivoted long

  # find special characters forcing chr instead of num
  # values = sort(unique(stage$DatumElevation_ft))
  # values[which(!grepl('^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$',values))]

  # stage character fixes
  chr_cols <- c("SubUnitCode", "SubUnitName", "DatumName", "DatumType", "GeodeticDatum",
                "XYAccuracy", "ElevationType", "ElevationSource", "AccNotes", "LastSurveyDate",
                "Comments")
  stage[,chr_cols][stage[,chr_cols] == "NA"] <- NA_character_

  # stage numeric fixes
  num_cols <- c("DatumLatitude", "DatumLongitude", "DatumElevation_ft", "EstVError_ft")
  stage[,num_cols][stage[,num_cols] == "NA"] <- NA_real_

  stage$DatumElevation_ft <- gsub(",", "", stage$DatumElevation_ft)
  stage[,num_cols] <- apply(stage[,num_cols], 2, function(x) as.numeric(x))

  # stage logic fixes
  log_cols <- c("Active", "IsPointCUI")
  stage[,log_cols] <- apply(stage[,log_cols], 2, function(x) as.logical(x))

  # wl char fixes
  chr_colw <- c("SubUnitCode", "SubUnitName", "DatumName", "TU-TD", "StageMethod",
                "StageNotes")
  wl[,chr_colw][wl[,chr_colw] == "NA"] <- NA_character_

  # wl numeric fixes
  num_colw <- c("GageReadingFeet", "DatumElevationFeet", "WaterLevelFeet")
  wl$DatumElevationFeet <- gsub(",", "", wl$DatumElevationFeet)
  wl$WaterLevelFeet <- gsub(",", "", wl$WaterLevelFeet)
  wl[,num_colw][wl[,num_colw] == "NA"] <- NA_real_
  wl[,num_colw] <- apply(wl[,num_colw], 2, function(x) as.numeric(x))

  # wl logic fixes
  wl$IsEventCUI <- as.logical(wl$IsEventCUI)

  # merge stage and water level data
  wlcomb <- full_join(stage, wl,
                      by = c("GroupCode", "GroupName", "UnitCode", "UnitName",
                             "SubUnitCode", "SubUnitName", "SiteCode", "SiteName",
                             "DatumName"))
  wlcomb$df_dif <- round(wlcomb$DatumElevation_ft - wlcomb$DatumElevationFeet, 3)

  # Add year, month and day of year column to dataset and fix data types
  wlcomb$year <- as.numeric(substr(wlcomb$EventDate, 1, 4))
  wlcomb$month <- as.numeric(substr(wlcomb$EventDate, 6, 7))
  wlcomb$doy <- as.numeric(strftime(wlcomb$EventDate, format = "%j"))

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = site_type, active = active))$SiteCode

  wl2 <- wlcomb |> filter(SiteCode %in% sites)
  wl3 <- wl2 |> filter(year %in% years) |> filter(month %in% months)
  wl3$WaterLevel_m <- wl3$WaterLevelFeet * 0.3048

  wl4 <-
  if(output == "short"){wl3[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode", "EventDate", "EventCode",
                                "year", "month", "doy", "DatumName", "DatumType", "DatumFunction",
                                "Active", "TU-TD", "StageMethod", "DatumLatitude", "DatumLongitude",
                                "DatumElevation_ft", "DatumElevationFeet",
                                "GageReadingFeet", "WaterLevelFeet", "WaterLevel_m")]
    } else {wl3}

  if(nrow(wl4) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}



  return(data.frame(wl4))

  }



