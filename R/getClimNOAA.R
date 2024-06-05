#' @include getSites.R
#'
#' @title getClimNOAA: Download monthly gridded NOAA climate data
#'
#' @description This function downloads monthly gridded climate data directly from NOAA's National Centers
#' for Environmental Information (www.ncei.noaa.gov) for the centroid of each park in NETN. The returned
#' data frame includes total precipitation (prcp in mm/day), Maximum air temperature (tmax in C) averaged
#' over the month, minimum air temperature (tmin in C) averaged over the month, and average temperature
#' (tavg in C). Note that occasionally you're unable to connect to the server, and will receive an error message
#' when that happens.
#'
#' @importFrom dplyr full_join select
#' @importFrom purrr map2 reduce safely
#'
#' @param year 4-digit year to query. Earliest available year is 1951, and latest is 2024. Currently can only
#' handle 1 year at a time.
#'
#' @param months vector of months to query. Typically there's about a 6 week delay in monthly data availability.
#'
#' @return Data frame of Daymet daily climate data for each specified site.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' #+++++ UPDATE ++++++
#'}
#'
#' @export

getClimNOAA <- function(year = 2024, months = 5){
  #--- error handling ---
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1980)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))

  if(!requireNamespace("raster", quietly = TRUE)){
    stop("Package 'raster' needed to download NOAA gridded climate data. Please install it.", call. = FALSE)
  }

  mos <- sprintf("%02d", months)

  #--- compile data ---
  # Create list of lat/longs to generate
  data("NETN_centroids")

  getnoaa <- function(yr, mon){
    mly_url <- paste0("https://www.ncei.noaa.gov/thredds/dodsC/nclimgrid-daily/", yr,
                      "/ncdd-", yr, mon, "-grd-scaled.nc")
    tryCatch(prcp <- raster::raster(mly_url, varname = "prcp"),
              error = function(e){
                stop(paste0("Unable to download from ncei.noaa.gov for ",
                       year, " and ", mon, ".\n",
                       "The year, month combination may not be availble yet, or the server is down.", "\n",
                       "Check https://www.ncei.noaa.gov/thredds/catalog/nclimgrid-daily/catalog.html for more info.", "\n",
                       "Only files ending in 'scaled' are considered available."))})

      #prcp <- raster::raster(mly_url, varname = "prcp")
      prcp_crop <- raster::crop(prcp, NETN_bbox)

      tmax <- raster::raster(mly_url, varname = "tmax")
      tmax_crop <- raster::crop(tmax, NETN_bbox)

      tmin <- raster::raster(mly_url, varname = "tmin")
      tmin_crop <- raster::crop(tmin, NETN_bbox)

      tavg <- raster::raster(mly_url, varname = "tavg")
      tavg_crop <- raster::crop(tavg, NETN_bbox)

      netn_prcp <- cbind(netn_cent_final, prcp = raster::extract(prcp_crop, netn_cent_final[,c("long", "lat")]))
      netn_tmax <- cbind(netn_cent_final, tmax = raster::extract(tmax_crop, netn_cent_final[,c("long", "lat")]))
      netn_tmin <- cbind(netn_cent_final, tmin = raster::extract(tmin_crop, netn_cent_final[,c("long", "lat")]))
      netn_tavg <- cbind(netn_cent_final, tavg = raster::extract(tavg_crop, netn_cent_final[,c("long", "lat")]))

      clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
      netn_comb <- reduce(clim_list, full_join, by = c("UNIT_CODE", "long", "lat"),)

      colnames(netn_comb) <- c("UnitCode", "long", "lat",
                               paste0("prcp_", yr, mon),
                               paste0("tmax_", yr, mon),
                               paste0("tmin_", yr, mon),
                               paste0("tavg_", yr, mon))
    }

  netn_final <- if(length(mos) > 1){
    map(mos, function(x){possibly(getnoaa(yr = year, mon = x), otherwise = NA)}) |> list_rbind()
    } else {getnoaa(yr = year, mon = mos)}

  return(netn_final)
}
