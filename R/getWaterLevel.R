#' @title getWaterLevel: query NETN water level data
#'
#' @description Queries NETN water level data by site, year, and month.
#'
#' @importFrom dplyr  filter left_join
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
#' @param observer_type Return data from all observers, or only return first observer.
#' Accepted values are c("all", "first", "second"), with "all" being the default.
#'
#' @return Data frame of Secchi data in long form (ie observers stacked).
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # get Secchi depth the Pogue from 2021-2023, first observer only
#' mabi <- getSecchi(sitecode = "MABIPA", years = 2021:2023, observer_type = "first")
#'
#' # get secchi for all ACAD lakes sampled in July for all observers
#' ACAD_lake <- getSecchi(park = 'ACAD', months = 7)
#'
#'}
#' @export

getWaterLevel <- function(park = "all", site = "all",
                     site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"),
                     months = 5:10, output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  observer_type <- match.arg(observer_type)
  output <- match.arg(output)

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

  stage$DatumName <- gsub("NA", NA_character_, stage$DatumName)
  stage$DatumType <- gsub("NA", NA_character_, stage$DatumType)
  stage$DatumLatitude <- as.numeric(gsub("NA", NA_real_, stage$DatumLatitude))
  stage$DatumLongitude <- as.numeric(gsub("NA", NA_real_, stage$DatumLongitude))

  # find special characters forcing chr instead of num
  # values = sort(unique(stage$DatumElevation_ft))
  # values[which(!grepl('^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$',values))]
  #++++ ENDED HERE ++++
  stage$DatumElevation_ft <- gsub(",", "", stage$DatumElevation_ft)
  stage$DatumElevation_ft <- as.numeric(gsub("NA", NA_real_, stage$DatumElevation_ft))


  stage$XYAccuracy <- gsub("NA", NA_character_, stage$XYAccuracy)
  stage$Datum <- gsub("NA", NA_character_, stage$Datum)
  stage$IsPointCUI <- as.logical(stage$IsPointCUI)

  str(stage)

  intersect(names(stage), names(wl))

  wlcomb <- full_join(stage, wl,
                      by = c("GroupCode", "GroupName", "UnitCode", "UnitName",
                             "SubUnitCode", "SubUnitName", "SiteCode", "SiteName",
                             "DatumName"))

  str(wlcomb)
  # Add year, month and day of year column to dataset and fix data types
  sec_long$year <- as.numeric(substr(sec_long$EventDate, 1, 4))
  sec_long$month <- as.numeric(substr(sec_long$EventDate, 6, 7))
  sec_long$doy <- as.numeric(strftime(sec_long$EventDate, format = "%j"))
  sec_long$IsEventCUI <- as.logical(sec_long$IsEventCUI)
  sec_long$SDepth_m <- as.numeric(gsub("NA", NA_real_, sec_long$SDepth_m))
  sec_long$SecchiObs <- gsub("NA", NA_character_, sec_long$SecchiObs)
  sec_long$Bot_SD <- gsub("NA", NA_character_, sec_long$Bot_SD)

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = 'lake'))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = 'lake',
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode)

  sec2 <- sec_long |> filter(SiteCode %in% sites)
  sec3 <- left_join(evs, sec2, by = c("SiteCode", "EventDate", "EventCode"))

  sec4 <- if(observer_type == "all"){sec3
  } else if(observer_type == "first"){filter(sec3, Observer %in% 1)
  } else if(observer_type == "second"){filter(sec3, Observer %in% 2)}

  sec5 <- sec4 |> filter(!is.na(SDepth_m))

  sec6 <-
  if(output == "short"){sec5[,c("SiteCode", "UnitCode", "SubUnitCode", "EventDate",
                                "year", "month", "doy", "SDepth_m", "SecchiObs", "Bot_SD", "Observer")]
    } else {sec4}

  if(nrow(sec6) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(sec6))

  }



