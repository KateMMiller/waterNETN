#' @include getSites.R
#' @include getEvents.R
#'
#' @title getLightPen: query NETN light penetration data
#'
#' @description Queries NETN light penetration data by site, year, month. Designed to only work with site_type = 'lake'.
#'
#' @importFrom dplyr filter inner_join left_join
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
#' @param site Filter on 6-letter SiteCode (e.g., "ACANTB", "WEFAPA", etc.). Easiest way to pick a site. Defaults to "all".
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
#' @return Data frame of Light Penetration data.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # get light penetration for the Pogue in MABI from 2021-2023
#' mabi <- getLightPen(site_code = "MABIPA", years = 2021:2023)
#'
#' # get light penetration for all ACAD lakes sampled in August
#' ACAD_lake <- getLightPen(park = 'ACAD', months = 8)
#'
#' }
#' @export

getLightPen <- function(park = "all", site = "all", event_type = "VS",
                        years = 2006:format(Sys.Date(), "%Y"), active = TRUE,
                        months = 5:10, output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  event_type <- match.arg(event_type, c("all", "VS", "acid", "misc"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  output <- match.arg(output)
  stopifnot(class(active) == "logical")

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({lpen <- get("Light_Penetration_Data", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Fix data issues
  # char fixes
  chr_cols <- c("SubUnitCode", "SubUnitName")
  lpen[,chr_cols][lpen[,chr_cols] == "NA"] <- NA_character_

  # numeric fixes
  num_cols <- c("MeasurementDepth_m", "LightDeck", "PenetrationRatio")
  lpen[,num_cols][lpen[,num_cols] == "NA"] <- NA_real_
  lpen[,num_cols] <- apply(lpen[,num_cols], 2, function(x) as.numeric(x))

  # logic fixes
  lpen$IsEventCUI <- as.logical(lpen$IsEventCUI)

  # Add year, month and day of year column to dataset
  lpen$year <- as.numeric(substr(lpen$EventDate, 1, 4))
  lpen$month <- as.numeric(substr(lpen$EventDate, 6, 7))
  lpen$doy <- as.numeric(strftime(lpen$EventDate, format = "%j"))

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = 'lake', active = active))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = 'lake', active = active, event_type = event_type,
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode, Project)

  lpen2 <- lpen |> filter(SiteCode %in% sites)
  lpen3 <- inner_join(evs, lpen2, by = c("SiteCode", "EventDate", "EventCode"))

  lpen4 <-
  if(output == "short"){lpen3[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode",
                                 "EventDate", "EventCode", "Project",
                                 "year", "month", "doy", "MeasurementTime", "MeasurementDepth_m",
                                 "LightDeck", "LightUW", "PenetrationRatio")]
    } else {lpen3}

  if(nrow(lpen4) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(lpen4))

  }



