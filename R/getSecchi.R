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
#' mabi <- getSecchi(sitecode = "MABIPA", years = 2021:2023, observer_type = "first")
#'
#' # get secchi for all ACAD lakes sampled in July for all observers
#' ACAD_lake <- getSecchi(park = 'ACAD', months = 7)
#'
#'}
#' @export

getSecchi <- function(park = "all", site = "all",
                     #site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"),
                     months = 5:10, active = TRUE,
                     observer_type = "first",
                     output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  #site_type <- match.arg(site_type)
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
  sec1 <- sec |> select(all_of(start_cols),
                        SDepth_m = SDepth1_m, SecchiObs = SecchiObs1, Bot_SD = Bot_SD1) |>
                 mutate(Observer = 1)
  sec2 <- sec |> select(all_of(start_cols),
                        SDepth_m = SDepth2_m, SecchiObs = SecchiObs2, Bot_SD = Bot_SD2) |>
                 mutate(Observer = 2)

  sec_long <- rbind(sec1, sec2)
  sec_long <- sec_long |> arrange(SiteCode, EventDate, Observer)

  # Add year, month and day of year column to dataset and fix data types
  sec_long$year <- as.numeric(substr(sec_long$EventDate, 1, 4))
  sec_long$month <- as.numeric(substr(sec_long$EventDate, 6, 7))
  sec_long$doy <- as.numeric(strftime(sec_long$EventDate, format = "%j"))
  sec_long$IsEventCUI <- as.logical(sec_long$IsEventCUI)
  sec_long$SDepth_m <- as.numeric(gsub("NA", NA_real_, sec_long$SDepth_m))
  sec_long$SecchiObs <- gsub("NA", NA_character_, sec_long$SecchiObs)
  sec_long$Bot_SD <- gsub("NA", NA_character_, sec_long$Bot_SD)

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = 'lake', active = active))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = 'lake', active = active,
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode)

  sec2 <- sec_long |> filter(SiteCode %in% sites)
  sec3 <- left_join(evs, sec2, by = c("SiteCode", "EventDate", "EventCode"))

  sec4 <- if(observer_type == "all"){sec3
  } else if(observer_type == "first"){filter(sec3, Observer %in% 1)
  } else if(observer_type == "second"){filter(sec3, Observer %in% 2)}

  sec5 <- sec4 |> filter(!is.na(SDepth_m))

  sec6 <-
  if(output == "short"){sec5[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode", "EventDate", "EventCode",
                                "year", "month", "doy", "SDepth_m", "SecchiObs", "Bot_SD", "Observer")]
    } else {sec4}

  if(nrow(sec6) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(sec6))

  }



