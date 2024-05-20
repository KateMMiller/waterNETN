#' @include getSites.R
#' @include getClimDaymet.R
#'
#' @title sumClimAvgs: Summarize monthly climate norms
#'
#' @importFrom dplyr mutate group_by left_join mutate select summarize
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_wider
#'
#' @description This function calculates monthly decadal and 30-year norms from
#' daily Daymet data by site. For each decade from 1980 to 2020 and for 30-year
#' spans through 2020, this function returns monthly averages for total precipitation
#' in mm (ppt_mm), average maximum temperature (tmax_C),average minimum temperature
#' (tmin_C), and average temperature (tmean_C) based on Daymet gridded climate data.
#' The 30-year norms from 1980 - 2009 and 1990 - 2019 are designated by _30yr_1980
#' for 1980-2009, and _30yr_1990 for 1990 - 2020. Averages by decade are designated
#' by _d1980 for 1980-1989, _d1990 for 1990 - 1999, etc. Note that Daymet data are
#' downloaded from an online web service and can be slow if many sites selected, and
#' occasionally the server is down that will result in an error message.
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
#' @param ... Additional arguments relevant to \code{getSites()} or \code{getClimDaymet()}
#'
#' @return Data frame of averaged monthly climate variables.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # calculate averages for the Pogue
#' pogue <- sumClimAvgs(site = "MABIPA")
#'
#' # Calculate for all active sites in MIMA
#' mima <- sumClimAvgs(park = "MIMA", start_year = 1990)
#'
#'}
#'
#' @export

sumClimAvgs <- function(park = 'all', site = 'all', site_type = 'all',
                        active = TRUE, ...){

#--- Bug handling ---
park <- match.arg(park, several.ok = TRUE,
                  c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                    "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
site_type <- match.arg(site_type, c("all", "lake", "stream"))
stopifnot(class(active) == 'logical')

sites <- force(getSites(park = park, site = site, site_type = site_type,
                  active = active, ...)) |>
  select(SiteCode, SiteName, UnitCode, SiteLatitude, SiteLongitude)

daym <- force(getClimDaymet(park = park, site = site, site_type = site_type,
                      active = active, years = 1980:2020,
                      ...))

daym$month <- as.numeric(format(daym$Date, "%m"))
daym$mon <- format(daym$Date, "%b")

daym_mon <- daym |>
  mutate(decade = paste0("d", year - year %% 10)) |>
  group_by(SiteCode, decade, year, month, mon) |>
  summarize(ppt_mm = sum(dm_prcp_mmday, na.rm = T),
            tmax_C = mean(dm_tmax_degc, na.rm = T),
            tmin_C = mean(dm_tmin_degc, na.rm = T),
            tmean_C = mean((dm_tmax_degc - dm_tmin_degc), na.rm = T),
            .groups = 'drop')
head(daym_mon)

daym_10 <- daym_mon |> filter(year %in% 1980:2019) |>
  group_by(SiteCode, month, mon, decade) |>
  summarize(ppt_mm = mean(ppt_mm),
            tmax_C = mean(tmax_C),
            tmin_C = mean(tmin_C),
            tmean_C = mean(tmean_C),
            .groups = 'drop') |>
  pivot_wider(names_from = decade, values_from = c(ppt_mm, tmax_C, tmin_C, tmean_C))

daym_80_09 <- daym_mon |> filter(year %in% 1980:2009) |>
  group_by(SiteCode, month, mon) |>
  summarize(ppt_mm_30yr_80 = mean(ppt_mm),
            tmax_C_30yr_80 = mean(tmax_C),
            tmin_C_30yr_80 = mean(tmin_C),
            tmean_C_30yr_80 = mean(tmean_C),
            .groups = 'drop')

daym_90_20 <- daym_mon |> filter(year %in% 1990:2019) |>
  group_by(SiteCode, month, mon) |>
  summarize(ppt_mm_30yr_90 = mean(ppt_mm),
            tmax_C_30yr_90 = mean(tmax_C),
            tmin_C_30yr_90 = mean(tmin_C),
            tmean_C_30yr_90 = mean(tmean_C),
            .groups = 'drop')


daym_comb <- reduce(list(daym_10, daym_80_09, daym_90_20), left_join, by = c("SiteCode", "month", "mon"))

daym_final <- left_join(sites, daym_comb, by = "SiteCode")

return(data.frame(daym_final))

}
