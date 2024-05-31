#' @include getSites.R
#'
#' @title getClimDrought: Download weekly county-level drought index
#'
#' @description This function downloads weekly drought index at the county level for each specified park, site or
#' weather station nearest to a specified site. If downloading for multiple sites and multiple weeks/years, function
#' may be slow. Returned data frame includes percent of county area in 5 levels of drought, with D0 = Abnormally Dry,
#' D1 = Moderate Drought, D2 = Severe Drought, D3 = Extreme Drought, and D4 = Exceptional Drought. Also returned is
#' the Drought Severity and Coverage Index, which ranges from 0 to 500, and is a weighted sum of area within each
#' of the drought categories. A score of 500 indicates the entire area is in exceptional drought (D4).
#'
#' @importFrom dplyr case_when filter select
#' @importFrom purrr list_rbind map
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
#' @param years Vector of years to download drought index for, will start with 01/01/year and end with 12/31/year.
#'
#' @param week_start Quoted start of week formatted as "mm/dd/yyyy". If specified, will return drought index for
#' given week. If blank, will return drought index for all weeks available in specified years. Note that weekly
#' data are available every Tuesday. If the week_start specified is not a Tuesday, code will shift the date to the
#' Tuesday of the specified start week.
#'
#' @param weather_station Logical. If TRUE, will return county-level data for coordinates of nearest weather station to a park.
#' If FALSE (default), returns county-level drought data for water monitoring sites. In most cases, the results are the same.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @return Data frame of weather station daily climate data for each specified site.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # Get drought info for MABI and SAGA sites for first week of May
#' mabisaga <- getClimDrought(park = c("MABI", "SAGA"), week_start = "05/01/2024")
#'
#' # Get drought info for Lower FDR Brook in ROVA for 2023
#' rovasa <- getClimDrought(site = "ROVASA", years = 2023)
#'
#'}
#'
#' @export

getClimDrought <- function(park = "all", site = "all",
                           site_type = c("all", "lake", "stream"),
                           years = c(2006:2023), week_start = NA,
                           active = TRUE,
                           weather_station = FALSE){

  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1980)
  stopifnot(class(active) == "logical")
  stopifnot(class(weather_station) == "logical")
  if(!is.na(week_start)){
    date_check <- as.Date(week_start, format = "%m/%d/%Y")
    if(is.na(date_check)){stop("Wrong date format specified. Must be formatted as 'mm/dd/yyyy'.")}
  }
  # Check that suggested package required for this function are installed
  if(!requireNamespace("jsonlite", quietly = TRUE)){
    stop("Package 'jsonlite' needed to download weather station data. Please install it.", call. = FALSE)
  }

  sites <- force(getSites(park = park, site = site, site_type = site_type, active = active)
                 )[, c("UnitCode", "SiteCode", "SiteName")]

  data("closest_WS")

  # Set up start and end dates and FIPS based on specified arguments
  current_year <- format(Sys.Date(), "%Y")
  stopifnot(max(years) <= current_year) # bug handling

  start_day <-
    if(is.na(week_start)){
      format(as.Date(paste0("01/01/", min(years)), format = "%m/%d/%Y"), "%m/%d/%Y")
    } else if(!is.na(week_start)){
        format(as.Date(week_start, format = "%m/%d/%Y"), "%m/%d/%Y")
    }

  dow_shift <- as.POSIXlt(start_day, format = "%m/%d/%Y")$wday

  start_tues <- if(dow_shift > 3){
    format(as.Date(start_day, format = "%m/%d/%Y") - dow_shift + 2, "%m/%d/%Y")
  } else if(dow_shift < 3){
    format(as.Date(start_day, format = "%m/%d/%Y") + dow_shift, "%m/%d/%Y")
    }

  end_day <-
    if(max(years) == current_year & is.na(week_start)){
      format(Sys.Date(), "%m/%d/%Y")
    } else if(max(years) < current_year & is.na(week_start)){
      format(as.Date(paste0("12/31/", max(years)), format = "%m/%d/%Y"), "%m/%d/%Y")
      } else if(!is.na(week_start)){format(as.Date(start_tues, format = "%m/%d/%Y") + 6, "%m/%d/%Y")}

  fips <- if(weather_station == TRUE){"WStnFIPS"} else {"ParkFIPS"}
  aoi <- filter(closest_WS, SiteCode == sites$SiteCode)[,fips]
  area = "CountyStatistics"

  getDSCI <- function(sitecode){
    fips <- if(weather_station == TRUE){"WStnFIPS"} else {"ParkFIPS"}
    aoi <- filter(closest_WS, SiteCode == sitecode)[,fips]
    url_dsci <- paste0("https://usdmdataservices.unl.edu/api/CountyStatistics",
                       "/GetDSCI?aoi=", aoi,
                       "&startdate=", start_tues, "&enddate=", end_day,
                       "&statisticsType=1")

    dsci <- fromJSON(url_dsci) # DSCI ranges from 0 to 500 with 500 being most extreme drought
    dsci$SiteCode <- sitecode
    return(dsci)
    }

  getDrought <- function(sitecode){
    fips <- if(weather_station == TRUE){"WStnFIPS"} else {"ParkFIPS"}
    aoi <- filter(closest_WS, SiteCode == sitecode)[,fips]

    url_drght <- paste0("https://usdmdataservices.unl.edu/api/CountyStatistics",
                        "/GetDroughtSeverityStatisticsByArea?aoi=", aoi,
                        "&startdate=", start_tues, "&enddate=", end_day,
                        "&statisticsType=1")

    drgt <- fromJSON(url_drght)
    drgt$SiteCode <- sitecode
    return(drgt)
  }

  # Iterate if multiple sites
  dsci_full <- if(length(sites$SiteCode) > 1){
    map(sites$SiteCode, \(x) getDSCI(x)) |> list_rbind()
    } else {getDSCI(sites$SiteCode)}

  drgt_full <- if(length(sites$SiteCode) > 1){
    map(sites$SiteCode, \(x) getDrought(x)) |> list_rbind()
  } else {getDrought(sites$SiteCode)}

  drgt_comb <- full_join(drgt_full, dsci_full, by = c("MapDate", "FIPS", "County", "State", "SiteCode")) |>
    data.frame()

  num_cols <- c("None", "D0", "D1", "D2", "D3", "D4", "DSCI")
  drgt_comb[,num_cols] <- as.numeric(unlist(drgt_comb[, num_cols]))

  return(drgt_comb)
  }
