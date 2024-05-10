#' @include getSites.R
#'
#' @title getClimDaymet: Download daily Daymet gridded climate data
#'
#' @description This function downloads daily gridded climate data from Daymet for each selected NETN
#' water monitoring site based on its lat/long coordinates, and binds each site's data into a single
#' dataframe. Final dataframe can also be written to disk (export = T). If downloading for all sites
#' and multiple years, function may be slow. The returned data frame includes
#' Day length (dayl in s/day), Precipitation (prcp in mm/day), Shortwave radiation (srad in W/m2),
#' Snow water equivalent (swe in kg/m2), Maximum air temperature (tmax in C), Minimum air temperature (tmin in C),
#' and Water vapor pressure (vp in Pascals). More details on metrics can be found online:
#' https://daymet.ornl.gov/overview.html > Parameters, Parameter abbreviations, Units and Descriptions.
#' Note that occasionally you're unable to connect to the server, and will receive an error message
#' when that happens.
#'
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map_dfr
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
#' @param years Vector of years to download Daymet data for. Earliest available year is 1980. Latest is currently 12/31/2023.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
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
#' # download for MORR 2023 only and export to csv
#' morr <- getClimDaymet(park = "MORR", years = 2023, filepath = "C:/data", export = T)
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
                          filepath = NA, export = FALSE,
                          silent = TRUE){
  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  site_type <- match.arg(site_type)
  stopifnot(class(silent) == 'logical')
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1980)
  stopifnot(class(active) == "logical")

  # Check that suggested package required for this function are installed
  if(!requireNamespace("daymetr", quietly = TRUE)){
    stop("Package 'daymetr' needed to download Daymet data. Please install it.", call. = FALSE)
  }


  if(export == TRUE){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when export = TRUE"))
    } else if(!file.exists(filepath)){
        stop("Specified file path does not exist.")}

    if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")} # add / to end of filepath if doesn't exist
  }

  #--- compile data ---
  # Create list of lat/longs to generate
  sites <- force(getSites(park = park, site = site, site_type = site_type, active = active)) |>
    select(site = SiteCode, latitude = SiteLatitude, longitude = SiteLongitude)

  # save to tmp folder for daymet to pull from and save indiv. site files to
  dir.create(tmp <- tempfile())
  write.csv(sites, paste0(tmp, "\\daymet_sites.csv"), row.names = F)

  cdata_long <-
    tryCatch({
      daymetr::download_daymet_batch(
      file_location = paste0(tmp, "\\daymet_sites.csv"),
                             start = min(years),
                             end = max(years),
                             path = tmp,
                             simplify = TRUE,
                             silent = silent)},
      error = function(e){stop('Unable to connect to Daymet server.')}
    )

  cdata_wide <- cdata_long |> pivot_wider(names_from = measurement, values_from = value) |> data.frame()
  colnames(cdata_wide) <- gsub("\\.\\.", "_", names(cdata_wide))
  colnames(cdata_wide) <- gsub("\\.", "", names(cdata_wide))
  newnames <- c(names(cdata_wide[1:7]), paste0("dm_", names(cdata_wide[8:14])))
  colnames(cdata_wide) <- newnames

  cdata_wide$Date <- as.Date(cdata_wide$yday, origin = paste0(cdata_wide$year, "-01-01"))

  cdata_final <- cdata_wide |> select(SiteCode = site, dm_tile = tile,
                                      SiteLatitude = latitude, SiteLongitude = longitude,
                                      altitude, Date, year, yday, dm_dayl_s, dm_prcp_mmday,
                                      dm_srad_Wm2, dm_swe_kgm2, dm_tmax_degc, dm_tmin_degc, dm_vp_Pa)

  if(export == TRUE){write.csv(cdata_final,
                               paste0(filepath, "Daymet_climate_data_", min(years), "-", max(years), ".csv"),
                               row.names = F)}

  return(data.frame(cdata_final))

}
