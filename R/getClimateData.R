#' @include getSites.R
#'
#' @title getClimateData: Download daily Daymet gridded climate data
#'
#' @description This function downloads daily climate data for each selected NETN water monitoring site
#' based on its lat/long coordinates, and binds each site's data into a single dataframe. Final dataframe
#' can also be written to disk (export = T). If downloading for all sites and multiple years, function
#' may be slow.
#'
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
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
#' @param years Vector of years to download Daymet data for. Earliest available year is 1980. Latest is currently 12/31/2023.
#'
#' @param export Logical. If TRUE, will export a CSV of the compiled Daymet data with a date stamp. Must supply
#' a filepath to write output to. If FALSE (Default), will only return results to R environment.
#'
#' @param filepath Quoted path to save files to. If not specified, will save to working directory.
#'
#' @param silent Logical. If TRUE (Default), won't show Daymet comments in console. If FALSE, will print Daymet comments
#' as sign of progress (and working) in console.
#'
#' @return Data frame of Daymet daily climate data for each specified site.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' #++++++ ADD EXAMPLES +++++++
#'
#'}
#'
#' @export

getClimateData <- function(park = "all", site = "all",
                           site_type = c("all", "lake", "stream"),
                           years = c(2006:2023),
                           filepath = NA, export = FALSE,
                           silent = TRUE){

  # Check that suggested package required for this function are installed
  if(!requireNamespace("daymetr", quietly = TRUE) & zip == TRUE){
    stop("Package 'daymetr' needed to download climate data. Please install it.", call. = FALSE)
  }

  stopifnot(class(silent) == 'logical')
  stopifnot(class(years) == "numeric", class >= 1980)

  #++++ ADD CHECK FOR FILEPATH IF EXPORT = T ++++
  # And check that file path exists

  # Create list of lat/longs to generate
  sites <- force(getSites(park = park, site = site, site_type = site_type)) |>
    select(site = SiteCode, latitude = SiteLatitude, longitude = SiteLongitude)

  # save to tmp folder for daymet to pull from and save indiv. site files to
  dir.create(tmp <- tempfile())
  write.csv(sites, paste0(tmp, "\\daymet_sites.csv"), row.names = F)

  cdata_long <-
  daymetr::download_daymet_batch(
    file_location = paste0(tmp, "\\daymet_sites.csv"),
                           start = min(years),
                           end = max(years),
                           path = tmp,
                           simplify = TRUE,
                           silent = silent)

  cdata_wide <- cdata_long |> pivot_wider(names_from = measurement, values_from = value) |> data.frame()
  colnames(cdata_wide) <- gsub("\\.\\.", "_", names(cdata_wide))
  colnames(cdata_wide) <- gsub("\\.", "", names(cdata_wide))

  return(cdata_wide)

}
