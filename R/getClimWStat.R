#' @include getSites.R
#'
#' @include getSites.R
#'
#' @title getClimWStat: Download daily data from nearest weather station
#'
#' @description This function downloads daily weather data from data.rcc-acis.org from the nearest
#' weather station with the longest period of record dating through 2024 for each selected NETN water
#' monitoring site based on its lat/long coordinates. Data include daily total precipitation (mm),
#' max. temperature (C), min temperature (C) and binds each site's data into a single dataframe. Final dataframe
#' can also be written to disk (export = T). If downloading for multiple sites and multiple years, function
#' may be slow, particularly ACAD. Note that the McFarland Hill weather station precip. data is not available
#' through typical weather station sources. Instead, hourly precip data from NADP are downloaded and summarized
#' to daily value. ACAD NADP Precip data only go back to 2008.
#'
#' @importFrom dplyr arrange filter group_by left_join select summarize
#' @importFrom purrr map_dfr reduce
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
#' @param years Vector of years to download weather station data for, will start with 01/01/year and end with 12/31/year.
#' Note that not all weather stations have complete a complete period of record from 2006 to current.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param export Logical. If TRUE, will export a CSV of the compiled weather data with a date stamp. Must supply
#' a filepath to write output to. If FALSE (Default), will only return results to R environment.
#'
#' @param filepath Quoted path to save files to. If not specified, will save to working directory.
#'
#' @return Data frame of weather station daily climate data for each specified site.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # get weather data for ROVA sites from 2020-2023
#' rova <- getClimWStat(park = "ROVA", years = 2020:2023)
#'
#' # get weather data for the Pogue (if can't remember the sitecode)
#' mabi <- getClimWStat(park = "MABI", site_type = "lake")
#'
#' # get weather data for 2023 for all lakes in ACAD (this will be slow)
#' acad_lakes <- getClimWStat(park = "ACAD", years = 2023, site_type = 'lake')
#'
#' # get weather data for Aunt Betty Inlet for 2022 and 2023
#' abin <- getClimWStat(site = "ACABIN", years = 2022:2023)
#'
#'}
#'
#' @export

getClimWStat <- function(park = "all", site = "all",
                         site_type = c("all", "lake", "stream"),
                         years = c(2006:2023), active = TRUE,
                         filepath = NA,
                         export = FALSE){

  # Check that suggested package required for this function are installed
  if(!requireNamespace("jsonlite", quietly = TRUE)){
    stop("Package 'jsonlite' needed to download weather station data. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("httr", quietly = TRUE)){
    stop("Package 'httr' needed to download weather station data. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("XML", quietly = TRUE)){
    stop("Package 'XML' needed to download weather station data. Please install it.", call. = FALSE)
  }

  stopifnot(class(years) %in% c("numeric", "integer"))
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"))

  stopifnot(class(active) == "logical")

  if(export == TRUE){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when export = TRUE"))
    } else if(!file.exists(filepath)){
      stop("Specified file path does not exist.")}

    if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")} # add / to end of filepath if doesn't exist
  }

  # Test connection to API
  if(httr::http_error("http://data.rcc-acis.org")){
    stop("Unable to connect to data.rcc-acis.org to download weather station data.")}

  # Combine sites with nearest weather station
  sites <- force(getSites(park = park, site = site, site_type = site_type, active = active)) |>
    select(SiteCode, SiteLatitude, SiteLongitude, UnitCode)

  data("closest_WS")
  sites_ws <- left_join(sites, closest_WS, by = c("SiteCode", "UnitCode"))
  parks_ws <- unique(sites_ws[,c("UnitCode", "id")])

  startdate <- paste0(min(years), "-01-01")
  enddate <- paste0(max(years), "-12-31")

  # function to downoad precip and temp weather station data from rcc-acis
  get_wdat <- function(field, stn, reduce = c("sum", "mean"), park){#, site){

  urlbase <- "http://data.rcc-acis.org/StnData?params=%s"
    elem_template <- function(field) {
      list(name = field,
           interval = "dly",
           duration = "dly")}

    params <-
      list(sid   = stn,
           sdate = startdate, #"por", # change to follow years
           edate = enddate,#"por", # change to follow years
           elems = lapply(field, elem_template))

    query <- sprintf(urlbase, URLencode(jsonlite::toJSON(params, auto_unbox = TRUE)))

    response <- httr::GET(query) |> httr::content(as = "text", encoding = "UTF-8") |> jsonlite::fromJSON()

    wdat <-
      response$data |> as.data.frame(stringsAsFactors = FALSE)

    names(wdat) <- c("Date", field)
    wdat[,field] <- gsub("A", "", wdat[,field])
    wdat[,field][wdat[,field] == "T"] <- 0
    wdat[,field][wdat[,field] %in% c("M", "S")] <- NA_real_ # replace M with NA for missing data
    wdat[,field] <- as.numeric(wdat[,field])

    UnitCode <- rep(park, nrow(wdat))
    SiteCode <- rep(site, nrow(wdat))
    wdat2 <- cbind(UnitCode, wdat)

    return(wdat2)
  }

  park_list <- sort(unique(parks_ws$UnitCode))

  if(length(park_list) > 1){
  ws_comb <- map_dfr(seq_along(park_list),

    function(x){
      station = parks_ws$id[[x]]
      parkc = parks_ws$UnitCode[[x]]
      if(!parkc %in% "ACAD"){ # no precip data for MCCF
      pcp_in <- get_wdat('pcpn',
                         stn = station,
                         park = parkc)
      tmax_f <- get_wdat('maxt',
                         stn = station,
                         park = parkc)
      tmin_f <- get_wdat('mint',
                         stn = station,
                         park = parkc)

    ws_ls <- list(pcp_in, tmax_f, tmin_f)
    ws_comb <- reduce(ws_ls, full_join, by = c("SiteCode", "UnitCode", "Date"))
    } else {
    tmax_f <- get_wdat('maxt',
                       stn = station,
                       park = parkc)
    tmin_f <- get_wdat('mint',
                       stn = station,
                       park = parkc)

    ws_ls <- list(tmax_f, tmin_f)
    ws_comb <- reduce(ws_ls, full_join, by = c("SiteCode", "UnitCode", "Date"))
    ws_comb$pcp_in <- NA_real_
    }
  })
  } else {
    station = parks_ws$id
    parkc = parks_ws$UnitCode
    if(any(!parkc %in% "ACAD")){ # no precip data for MCCF
      pcp_in <- get_wdat('pcpn',
                         stn = station,
                         park = parkc)
      tmax_f <- get_wdat('maxt',
                         stn = station,
                         park = parkc)
      tmin_f <- get_wdat('mint',
                         stn = station,
                         park = parkc)

      ws_ls <- list(pcp_in, tmax_f, tmin_f)
      ws_comb <- reduce(ws_ls, full_join, by = c("UnitCode", "Date"))
    } else {
      tmax_f <- get_wdat('maxt',
                         stn = station,
                         park = parkc)
      tmin_f <- get_wdat('mint',
                         stn = station,
                         park = parkc)

      ws_ls <- list(tmax_f, tmin_f)
      ws_comb <- reduce(ws_ls, full_join, by = c("UnitCode", "Date"))
      ws_comb$pcp_in <- NA_real_
    }
  }

  # ACAD McFarland WS doesn't include precip. on any site but NADP. Downloading from NADP
  # park_list <- sort(unique(sites_ws$UnitCode))

  if(park_list %in% "ACAD"){
  start_date <- paste0("01/01/", min(years))
  end_date <- paste0("12/31/", max(years))
  stationID <- "ME98"

  PrecipURL<-paste0('http://nadp2.slh.wisc.edu/siteOps/ppt/Data.aspx?id=', stationID, '&stdate=',
                    start_date,'T13:35&endate=', end_date,
                    'T13:30&plot_sel=1111110&data_sel1=H&data_sel2=110&sel2_count=2&offset_txt=-5')

  precip_tbl <- XML::readHTMLTable(PrecipURL, header=T, as.data.frame = T,
                                   stringsAsFactors = F)$GridView_data[,c('Date','Hour','Precip (in)')]
  precip_tbl$precip_in <- as.numeric(precip_tbl$`Precip (in)`)
  precip_day <- precip_tbl |> group_by(Date) |> summarize(tot_prec_in = sum(precip_in))
  precip_day$precip_mm <- precip_day$tot_prec_in * 25.4
  precip_day$UnitCode <- "ACAD"
  precip_ACAD <- precip_day[,c("UnitCode", "Date", "precip_mm")]
  }

  ws_comb2 <-
  if(park_list %in% "ACAD"){
    left_join(ws_comb, precip_ACAD, by = c("UnitCode", "Date"))
  } else {ws_comb}

  ws_comb2$ws_pcpmm <- ifelse(ws_comb2$UnitCode %in% "ACAD",
                              round(ws_comb2$precip_mm, 3), round(ws_comb2$pcpn * 25.4, 3))
  ws_comb2$ws_tmaxc <- round((ws_comb2$maxt - 32) * (5/9), 3)
  ws_comb2$ws_tminc <- round((ws_comb2$mint - 32) * (5/9), 3)
  ws_comb2$year <- substr(ws_comb2$Date, 1, 4)
  ws_comb3 <- ws_comb2 |> filter(year %in% years) |>
    select(UnitCode, Date, year, ws_tmaxc, ws_tminc, ws_pcpmm)

  ws_final <- left_join(ws_comb3, closest_WS, by = "UnitCode", relationship = "many-to-many") |>
    select(SiteCode, UnitCode, Date, year, ws_id = id,
           ws_lat, ws_long, ws_dist_km, ws_tmaxc, ws_tminc, ws_pcpmm) |>
    arrange(UnitCode, SiteCode, Date)

  ws_final$ws_dist_km <- round(ws_final$ws_dist_km, 3)

  if(export == TRUE){write.csv(ws_final,
                               paste0(filepath, "Weather_station_data_", min(years), "-", max(years), ".csv"),
                               row.names = F)}
  return(data.frame(ws_final))

}
