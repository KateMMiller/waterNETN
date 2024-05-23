#' @include getSites.R
#' @include getClimDaymet.R
#' @include getClimWStat.R
#'
#' @title sumClimMonthly: Summarize monthly climate statistics
#'
#' @importFrom dplyr filter full_join group_by left_join mutate select summarize
#'
#' @description This function calculates monthly statistics from daily Daymet and/or
#' weather station data by site. Statistics returned are monthly total precipitation
#' in mm (ppt_mm), average maximum temperature (tmax_C), average minimum temperature (tmin_C),
#' and average temperature (tmean_C). When Daymet data are requested, results also return a
#' solar radiation (srad in mjm2), potential evapotranspiration using Hargreaves (PEThar),
#' water balance (BAL), 1- month and 3-month Standardized Precipitation Evapotranspiration Index (SPEI),
#' a drought index that takes into account precipitation, temperature, evapotranspiration,
#' and solar radiation. The 3-month SPEI will be NA for the first 2 months in the year span,
#' because it's a 3-month average. Daymet vs. weather station metrics are indicated by whether
#' they start with dm_ or ws_. Note that occasionally you're unable to connect to the Daymet
#' or rcc-acis server and will receive and error message when that happens.
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
#' @param site Filter on 6-letter SiteCode (e.g., "ACABIN", "MORRSA", etc.). Easiest
#' way to pick a site. Defaults to "all".
#'
#' @param site_type Combine all site types, lakes or streams. Not needed if specifying
#' particular sites.
#' \describe{
#' \item{"all"}{Default. Includes all site types, unless site or site_name select specific site types.}
#' \item{"lake"}{Include only lakes.}
#' \item{"stream"}{Include streams only.}
#' }
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites.
#' If FALSE, returns all sites that have been monitored.
#'
#' @param years Vector of years to download weather station data for, will start with 01/01/year and end with 12/31/year.
#' Note that not all weather stations have complete a complete period of record from 2006 to current.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults. Note that only returns data for complete months.
#'
#' @param data_type Specify Daymet ("daymet"), weather station ("wstn"), or both ('all'; default). Note that Daymet
#' data aren't available immediately, whereas weather station data may be available within days of current day.
#' When Daymet data are requested, results also return 1-month and 3-month SPEI, a drought index.
#'
#' @param ... Additional arguments relevant to \code{getSites()}
#'
#' @return Data frame of monthly statistics for selected parameters.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # sum 2006 - 2023 stats for Pogue including daymet and weather station
#' pog_stats <- sumClimMonthly(site = "MABIPA", years = 2006:2023)
#'
#' # sum 2023 weather station stats for all active sites in MIMA
#' mima_stats <- sumClimMonthly(park = "MIMA", years = 2023, data_type = 'wstn')
#'
#'}
#'
#' @export

sumClimMonthly <- function(park = 'all', site = 'all', site_type = 'all',
                           active = TRUE, years = c(2006:2023), data_type = 'all',
                           months = 1:12, ...){

#--- Bug handling ---
# Check that suggested package required for this function are installed
if(!requireNamespace("SPEI", quietly = TRUE)){
   stop("Package 'SPEI' needed to calculate SPEI with Daymet data. Please install it.", call. = FALSE)
}

park <- match.arg(park, several.ok = TRUE,
                  c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                    "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
site_type <- match.arg(site_type, c("all", "lake", "stream"))
data_type <- match.arg(data_type, c("all", "daymet", "wstn"))
stopifnot(class(active) == 'logical')
stopifnot(class(years) %in% c("numeric", "integer"))
stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))

#--- compile data ---
sites <- force(getSites(park = park, site = site, site_type = site_type,
                  active = active, ...)) |>
  select(SiteCode, SiteName, UnitCode, SiteLatitude, SiteLongitude)

if(data_type %in% c("all", "daymet")){
daym <- force(getClimDaymet(park = park, site = site, site_type = site_type,
                           active = active, years = years, silent = TRUE,
                           weather_station = F, ...))

daym$srad_mjm2 <- (daym$dm_srad_Wm2 * daym$dm_dayl_s/1000000)

daym$month <- as.numeric(format(daym$Date, "%m"))
daym$mon <- format(daym$Date, "%b")
daym$year <- as.numeric(daym$year)

daym_mon <- daym |>
  mutate(dm_tmean_degc = (dm_tmax_degc + dm_tmin_degc)/2) |>
  group_by(SiteCode, year, month, mon, SiteLatitude) |>
  summarize(dm_ppt_mm = sum(dm_prcp_mmday, na.rm = T),
            dm_tmax_C = mean(dm_tmax_degc, na.rm = T),
            dm_tmin_C = mean(dm_tmin_degc, na.rm = T),
            dm_tmean_C = mean(dm_tmean_degc, na.rm = T),
            dm_srad = mean(srad_mjm2),
            .groups = 'drop')

daym_mon$dm_PEThar <- SPEI::hargreaves(Tmin = daym_mon$dm_tmin_C,
  Tmax = daym_mon$dm_tmax_C, lat = daym_mon$SiteLatitude,
  Ra = daym_mon$dm_srad, Pre = daym_mon$dm_ppt_mm, verbose = FALSE)

daym_mon$dm_BAL <- daym_mon$dm_ppt_mm - daym_mon$dm_PEThar

daym_SPEI <- data.frame(
  daym_mon,
  dm_SPEI01 = SPEI::spei(daym_mon$dm_BAL, scale = 1, verbose = F)$fitted,
  dm_SPEI03 = SPEI::spei(daym_mon$dm_BAL, scale = 3, verbose = F)$fitted)
}

if(data_type %in% c("all", "wstn")){
  wstn <- getClimWStat(park = park, site = site, site_type = site_type, years = years,
                       active = active, ...)
  wstn$Date <- as.Date(wstn$Date, format = "%Y-%m-%d")
  wstn$month <- as.numeric(format(wstn$Date, "%m"))
  wstn$mon <- format(wstn$Date, "%b")
  wstn$ws_tmean_C <- (wstn$ws_tmaxc + wstn$ws_tminc)/2

  # drop data for incomplete months
  wstn$mon_next_day <- as.numeric(format(wstn$Date + 1, "%m"))

  wstn$mon_comp <- ifelse(wstn$month == 12 & wstn$mon_next_day == 1,
                          1, wstn$mon_next_day - wstn$month)

  wstn_comp <- wstn |> filter(!is.na(ws_tmaxc)) |>
    group_by(SiteCode, year, month, mon) |>
    summarize(mon_comp = sum(mon_comp), .groups = 'drop') |> filter(mon_comp == 1)

  wstn_comp <- left_join(wstn_comp, wstn, by = c("SiteCode", "year", "month", "mon"))

  wstn_mon <- wstn_comp |> group_by(SiteCode, year, month, mon) |>
    summarize(ws_ppt_mm = sum(ws_pcpmm, na.rm = T),
              ws_tmax_C = mean(ws_tmaxc, na.rm = T),
              ws_tmin_C = mean(ws_tminc, na.rm = T),
              ws_tmean_C = mean(ws_tmean_C, na.rm = T),
              .groups = 'drop')

  wstn_mon$year <- as.numeric(wstn_mon$year)

}

clim_dat <-
  if(data_type == "all"){
    full_join(daym_SPEI, wstn_mon, by = c("SiteCode", "year", "month", "mon"))
  } else if(data_type == "daymet"){
    daym_SPEI
  } else if(data_type == "wstn"){
    wstn_mon
  }

clim_dat_final <- left_join(sites |> select(SiteCode, SiteName, UnitCode),
                            clim_dat, by = "SiteCode") |>
  filter(month %in% months)

return(data.frame(clim_dat_final))

}
