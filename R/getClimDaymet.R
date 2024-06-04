#' @include getSites.R
#'
#' @title getClimDaymet: Download daily Daymet gridded climate data directly from REST
#'
#' @description This function downloads daily gridded climate data directly from Daymet REST for
#' each selected NETN water monitoring site based on its lat/long coordinates, and binds each
#' site's data into a single dataframe. If downloading for all sites and multiple years, function
#' may be slow. The returned data frame includes Day length (dayl in s/day), Precipitation (prcp
#' in mm/day), Shortwave radiation (srad in W/m2), Snow water equivalent (swe in kg/m2),
#' Maximum air temperature (tmax in C), Minimum air temperature (tmin in C), and Water vapor
#' pressure (vp in Pascals). More details on metrics can be found online:
#' https://daymet.ornl.gov/overview.html > Parameters, Parameter abbreviations, Units and Descriptions.
#' Note that occasionally you're unable to connect to the server, and will receive an error message
#' when that happens.
#'
#' @importFrom dplyr left_join select
#' @importFrom purrr pmap
#' @importFrom tidyr pivot_wider
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
#' @param weather_station Logical. If TRUE, will return Daymet data for coordinates of nearest weather station to a park.
#' If FALSE (default), returns Daymet data for water monitoring sites only. This argument facilitates calculating decadal
#' averages to compare with current-year weather station data.
#'
#' @param years Vector of years to download Daymet data. Earliest available year is 1980. Latest is
#' currently 12/31/2023.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @return Data frame of Daymet daily climate data for each specified site.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # download for MORR 2023 only
#' morr <- getClimDaymet(park = "MORR", years = 2023)
#'
#' # download for ACAD lakes from 1980:2023
#' acad_lakes <- getClimDaymet(park = "ACAD", site_type = "lake", years = 1980:2023)
#'
#'}
#'
#' @export

getClimDaymet <- function(park = "all", site = "all",
                              site_type = c("all", "lake", "stream"),
                              years = c(2006:2023), active = TRUE,
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

  #--- compile data ---
  # Create list of lat/longs to generate
  data("closest_WS")

  sites1 <- force(getSites(park = park, site = site, site_type = site_type,
                           active = active)) |>
    select(site = SiteCode, lat = SiteLatitude, long = SiteLongitude)

  sites <- if(weather_station == TRUE){
    left_join(sites1, closest_WS, c("site" = "SiteCode")) |>
      select(site, lat = ws_lat, long = ws_long)
  } else {sites1}

  test <- httr::POST("https://daymet.ornl.gov/single-pixel/api/data?")$status_code
  if(test %in% c(503, 404)){stop("Unable to reach Daymet Server.")}

  # Function to download one site
  getdaym <- function(sitecode, lat, long, years){
    # set up url
    year_range <- paste0(years, collapse = ",")
    daym_server <- "https://daymet.ornl.gov/single-pixel/api/data?"
    daym_url <- paste0(daym_server,
                       "lat=", lat,
                       "&lon=", long,
                       #"&vars=tmax,tmin,dayl,prcp,srad,swe,vp",
                       "&years=", year_range)
    # Download from server
    dat <- httr::GET(daym_url)
    # Turn into dataframe
    dat2 <- httr::content(dat, as = "text", encoding = "UTF-8")
    header_start <- unlist(gregexpr("year,yday", dat2))
    tile_pos <- unlist(gregexpr("Tile: ", dat2)) + 5
    tile <- as.numeric(substr(dat2, tile_pos, tile_pos + 6)) #12116
    alt_pos <- unlist(gregexpr("Elevation: " , dat2)) + 11
    altitude <- as.numeric(gsub("[[:alpha:]]|", "", (substr(dat2, alt_pos, alt_pos + 5))))
    dat3 <- substr(dat2, header_start, nchar(dat2))
    dat4 <- read.table(textConnection(dat3), sep = ",", header = T)
    colnames(dat4) <- gsub("\\.\\.", "_", names(dat4))
    colnames(dat4) <- gsub("\\.", "", names(dat4))
    dat4$SiteLatitude = lat
    dat4$SiteLongitude = long
    dat4$SiteCode = sitecode
    dat4$dm_tile = tile
    dat4$altitude = altitude
    dat4$Date <- as.Date(dat4$yday, origin = paste0(dat4$year, "-01-01"))
    dat4 <- dat4[, c("SiteCode", "dm_tile", "SiteLatitude", "SiteLongitude", "altitude",
                     "Date", "year", "yday",
                     "dayl_s", "prcp_mmday", "srad_Wm2", "swe_kgm2", "tmax_degc",
                     "tmin_degc", "vp_Pa")]

    newnames <- c(names(dat4[, 1:8]),
                  paste0("dm_", names(dat4[, 9:ncol(dat4)])))
    colnames(dat4) <- newnames
    dat4
  }

  # Iterate to download all sites
  site_list <- list(sites$site, sites$lat, sites$long)
  yrs = years

  comb_daym <- pmap(site_list, function(sitecode, lat, long, yrs){
    getdaym(sitecode, lat, long, years)
  }) |> list_rbind()
  return(comb_daym)

}
