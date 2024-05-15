#' @include getSites.R
#' @include getClimDaymet.R
#'
#' @title sumClim30yr: Summarize 30-year monthly climate norms
#'
#' @importFrom dplyr mutate group_by left_join select summarize
#'
#' @description This function calculates monthly 30 year norms from daily Daymet data by site.
#' User can specify start of the 30-year norm, the earliest being 1980. Note that Daymet data are
#' downloaded from an online web service and can be slow if many sites selected. Normals returned
#' are 30-year monthly averages for total precipitation in mm (ppt_mm), average maximum temperature
#' (tmax_C), average minimum temperature (tmin_C), and average temperature (tmean_C). Note that
#' occasionally you're unable to connect to the server and will receive and error message when
#' that happens.
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
#' @param start_year Year to start the 30-year normal. Common choices are 1980
#' (1980 - 2009; default), or 1990 (1990 - 2019). Daymet data only go back to 1980, so
#' earlier norms are not possible.
#'
#' @param ... Additional arguments relevant to \code{getSites()} or \code{getClimDaymet()}
#'
#' @return Data frame of 30-year normals for selected parameters.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # sum 1980 - 2010 normals for the Pogue
#' pog80 <- sumClim30yr(site = "MABIPA", start_year = 1980)
#'
#' # sum 1990 - 2020 normals for all active sites in MIMA
#' mima90 <- sumClim30yr(park = "MIMA", start_year = 1990)
#'
#'}
#'
#' @export

sumClim30yr <- function(park = 'all', site = 'all', site_type = 'all',
                        active = TRUE, start_year = 1980, ...){

#--- Bug handling ---
park <- match.arg(park, several.ok = TRUE,
                  c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                    "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
park <- ifelse(park == "LNETN",
               c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
site_type <- match.arg(site_type)
stopifnot(class(active) == 'logical')
stopifnot(class(start_year) %in% c("integer", "numeric"), start_year >= 1980)

sites <- force(getSites(park = park, site = site, site_type = site_type,
                  active = active, ...)) |>
  select(SiteCode, SiteName, UnitCode, SiteLatitude, SiteLongitude)

daym <- force(getClimDaymet(park = park, site = site, site_type = site_type,
                      active = active, years = start_year:(start_year + 29),
                      ...))

daym$month <- as.numeric(format(daym$Date, "%m"))
daym$mon <- format(daym$Date, "%b")

daym_mon <- daym |> group_by(SiteCode, year, month, mon) |>
  summarize(ppt_mm = sum(dm_prcp_mmday, na.rm = T),
            tmax_C = mean(dm_tmax_degc, na.rm = T),
            tmin_C = mean(dm_tmin_degc, na.rm = T),
            tmean_C = mean((dm_tmax_degc - dm_tmin_degc), na.rm = T),
            .groups = 'drop')

daym_30 <- daym_mon |> group_by(SiteCode, month, mon) |>
  summarize(ppt_mm = mean(ppt_mm),
            tmax_C = mean(tmax_C),
            tmin_C = mean(tmin_C),
            tmean_C = mean(tmean_C),
            .groups = 'drop')

daym_30_final <- left_join(sites, daym_30, by = "SiteCode")

return(data.frame(daym_30_final))

}
