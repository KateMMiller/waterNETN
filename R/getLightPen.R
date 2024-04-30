#' @title getLightPen: query NETN light penetration data
#'
#' @description Queries NETN light penetration data by site, year, month
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
#' @return Data frame of Light Penetration data.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # get events for all sites in MABI from 2021-2023
#' mabi <- getDischarge(park = "MABI", years = 2021:2023)
#'
#' # get events for SARA sites sampled in 2019 and 2023
#' sara <- getDischarge(park = "SARA", years = c(2019, 2023))
#'
#' # get events for MIMA and SAIR
#' ma_parks <- getDischarge(park = c("SAIR", "MIMA"))
#'
#' # get info for all ACAD lakes sampled in April
#' ACAD_lake4<- getDischarge(park = 'ACAD', site_type = 'lake', months = 4)
#'
#' # get site info for 2 streams in MORR with full output
#' morr_sites <- getDischarge(site = c("MORRSA", "MORRSB"), output = 'verbose')
#' }
#' @export

getLightPen <- function(park = "all", site = "all",
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

  tryCatch({lpen <- get("Light_Penetration_Data", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Add year, month and day of year column to dataset
  lpen$year <- as.numeric(substr(lpen$EventDate, 1, 4))
  lpen$month <- as.numeric(substr(lpen$EventDate, 6, 7))
  lpen$doy <- as.numeric(strftime(lpen$EventDate, format = "%j"))
  lpen$IsEventCUI <- as.logical(lpen$IsEventCUI)
  lpen$MeasurementDepth_m <- as.numeric(gsub("NA", NA_real_, lpen$MeasurementDepth_m))
  lpen$LightDeck <- as.numeric(gsub("NA", NA_real_, lpen$LightDeck))
  lpen$LightUW <- as.numeric(gsub("NA", NA_real_, lpen$LightUW))
  lpen$PenetrationRatio <- as.numeric(gsub("NA", NA_real_, lpen$PenetrationRatio))

  # Fidis# Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = site_type))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = site_type,
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode)

  lpen2 <- lpen |> filter(SiteCode %in% sites)
  lpen3 <- left_join(evs, lpen2, by = c("SiteCode", "EventDate", "EventCode"))

  lpen4 <-
  if(output == "short"){lpen3[,c("SiteCode", "UnitCode", "SubUnitCode", "EventDate",
                                "year", "month", "doy", "MeasurementTime", "MeasurementDepth_m",
                                "LightDeck", "LightUW", "PenetrationRatio")]
    } else {lpen3}

  if(nrow(lpen4) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(lpen4))

  }



