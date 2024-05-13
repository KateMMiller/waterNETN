#' @include getSites.R
#' @include theme_WQ.R
#' @include sumClimMonthly.R
#'
#' @title plotClimTrend: Plot climate trends
#'
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @description This function produces a line or smoothed trend plot filtered on park, site, year, month, and
#' climate parameter. If multiple sites are specified, they can either be plotted on the same figure or separate
#' figures. If multiple parameters are specified, they will be plotted on separate figures.
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
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param parameter Specify the parameter(s) to plot. Acceptable values are
#' \describe{
#' \item{"all"}{Plot all climate variables}
#' \item{"dm_ppt_mm"}{Daymet monthly total precipitation in mm.}
#' \item{"dm_tmax_C"}{Daymet monthly average maximum temperature in C.}
#' \item{"dm_tmin_C"}{Daymet monthly average minimum temperature in C.}
#' \item{"dm_tmean_C"}{Daymet monthly average temperature in C.}
#' \item{"dm_PEThar"}{Daymet potential evapotranspiration using Hargreaves equation}
#' \item{"dm_BAL"}{Daymet water balance, calculated as precip - PET}
#' \item{"dm_SPEI01"}{Drought index: Standarized Precipitation Evapotranspiration Index at the 1-month level. }
#' \item{"dm_SPEI03"}{Drought index: Standarized Precipitation Evapotranspiration Index at the 3-month level. }
#' \item{"ws_ppt_mm"}{Weather station monthly total precipitation in mm.}
#' \item{"ws_tmax_C"}{Weather station monthly average maximum temperature in C.}
#' \item{"ws_tmin_C"}{Weather station monthly average minimum temperature in C.}
#' \item{"ws_tmean_C"}{Weather station monthly average temperature in C.}
#' }
#'
#' @param facet_site Logical. If TRUE, plots sites on separate facets (ie figures). If FALSE (Default),
#' plots all sites on the same figure. This is only enabled if multiple sites are chosen.
#'
#' @param layers Options are "points" and "lines". By default, both will plot.
#'
#' @param color_theme Theme to plot points and lines. Options currently are 'viridis' (Default- ranges of blue, green and yellow),
#' or from RColorBrewer: 'set1', 'dark2', or 'accent' (see https://ggplot2-book.org/scales-colour).
#'
#' @param smooth Logical. If TRUE (Default), will plot a loess smoothed line. If FALSE, will plot actual line. Only
#' plots if layers argument includes 'lines'.
#'
#' @param span Numeric. Determines how smoothed the line will be for smooth = TRUE. Default is 0.3. Higher spans (up to 1)
#' cause more smoothing (straighter lines). Smaller spans are wavier. Span can range from 0 to 1. Span of 1 is linear.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param ... Additional arguments relevant to \code{getChemistry()} or \code{getSondeInSitu()}
#'
#' @examples
#' \dontrun{
#'
#'
#' plotClimTrend()
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimTrend <- function(park = "all", site = "all",
                          site_type = c("all", "lake", "stream"),
                          years = 2006:format(Sys.Date(), "%Y"),
                          months = 1:12, active = TRUE,
                          parameter = NA, facet_site = FALSE,
                          color_theme = 'viridis', smooth = TRUE,
                          span = 0.3,
                          legend_position = 'none', ...){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)

  dm_param <- c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C",
                "dm_tmean_C", "dm_srad", "dm_PEThar",
                "dm_BAL", "dm_SPEI01", "dm_SPEI03")

  ws_param <- c( "ws_ppt_mm", "ws_tmax_C", "ws_tmin_C", "ws_tmean_C")

  parameter <- match.arg(parameter, c("all", dm_param, ws_param), several.ok = TRUE)

  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  stopifnot(class(include_censored) == "logical")
  stopifnot(class(smooth) == "logical")
  stopifnot(class(span) %in% "numeric")
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  color_theme <- match.arg(color_theme, c("viridis", "set1", "dark2", "accent"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))

  #-- Compile data for plotting --
  clim_dat <-
    if(any(parameter == "all") | (any(parameter %in% dm_param) & any(parameter %in% ws_param))){
      sumClimMonthly(park = park, site = site, site_type = site_type, active = active, years = years,
                   data_type = 'all')
    } else if(all(parameter %in% dm_param)){
      sumClimMonthly(park = park, site = site, site_type = site_type, active = active, years = years,
                     data_type = 'daymet')
    } else if(all(parameter %in% ws_param)){
      sumClimMonthly(park = park, site = site, site_type = site_type, active = active, years = years,
                     data_type = 'wstn')
    }

  param <- if(any(parameter == "all")){c(dm_param, ws_param)} else {parameter}

  clim_dat2 <- clim_dat[,c("SiteCode", "SiteName", "UnitCode", "month", "mon", "year", param)]

  clim_dat_long <- clim_dat2 |> pivot_longer({{param}}, names_to = "param", values_to = "value") |>
    arrange(SiteCode, month, param)

  clim_dat_long$date <- as.Date(paste0(clim_dat_long$year, "-", clim_dat_long$month, "-", 15), format = "%Y-%m-%d")

  #-- Set up plotting features --
  ylab <- ifelse(length(unique(clim_dat_long$param)) == 1, unique(clim_dat_long$param), "Monthly Value")

  facet_site <- ifelse(length(unique(clim_dat_long$SiteCode)) > 1, TRUE, FALSE)
  facet_param <- ifelse(length(unique(clim_dat_long$param)) > 1, TRUE, FALSE)

  pars <- c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C", "dm_tmean_C", "dm_BAL", "dm_SPEI01", "dm_SPEI03",
            "ws_ppt_mm", "ws_tmax_C", "ws_tmin_C", "ws_tmean_C")

  plabs <- c("Gridded Total Precip. (mm)", "Gridded Max. Temp. (C)", "Gridded Min. Temp. (C)",
             "Gridded Avg. Temp. (C)", "Water Balance", "SPEI - 1 month", "SPEI - 3 month",
             "WS Total Precip. (mm)", "WS Max. Temp. (C)", "WS Min. Temp. (C)", "WS Avg. Temp (C)")

  param_labels <- data.frame(
                    param = c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C", "dm_tmean_C", "dm_BAL", "dm_SPEI01", "dm_SPEI03",
                              "ws_ppt_mm", "ws_tmax_C", "ws_tmin_C", "ws_tmean_C"),
                    param_label = c("Mon. Gridded Total Precip. (mm)", "Mon. Gridded Max. Temp. (C)", "Mon. Gridded Min. Temp. (C)",
                                    "Mon. Gridded Avg. Temp. (C)", "Mon. Water Balance", "SPEI - 1 month", "SPEI - 3 month",
                                    "Mon. WS Total Precip. (mm)", "Mon. WS Max. Temp. (C)", "Mon. WS Min. Temp. (C)",
                                    "Mon. WS Avg. Temp (C)"))

  clim_dat_long <- left_join(clim_dat_long, param_labels, by = 'param')

  #-- Create plot --
  climtrendplot <-
    ggplot(clim_dat_long, aes(x = date, y = value, group = SiteName,
                     color = SiteName, fill = SiteName)) +
      # layers
      {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span) } +
      {if(smooth == FALSE & any(layers %in% "lines")) geom_line()} +
      {if(any(layers %in% "points")) geom_point(alpha = 0.6)} +
      # facets
      {if(facet_site == TRUE & facet_param == TRUE) facet_wrap(~SiteName + param_label, drop = T)} +
      {if(facet_site == TRUE & facet_param == FALSE) facet_wrap(~SiteName, drop = T)} +
      {if(facet_site == FALSE & facet_param == TRUE) facet_wrap(~param_label, drop = T)} +

      # themes
      theme_WQ() + theme(legend.position = legend_position, legend.title = element_blank()) +
      # palettes
      {if(color_theme == "viridis") scale_color_viridis_d()} +
      {if(color_theme == "set1") scale_color_brewer(palette = "Set1")} +
      {if(color_theme == "dark2") scale_color_brewer(palette = "Dark2")} +
      {if(color_theme == "accent") scale_color_brewer(palette = "Accent")} +
      {if(color_theme == "viridis") scale_fill_viridis_d()} +
      {if(color_theme == "set1") scale_fill_brewer(palette = "Set1")} +
      {if(color_theme == "dark2") scale_fill_brewer(palette = "Dark2")} +
      {if(color_theme == "accent") scale_fill_brewer(palette = "Accent")} +
      # labels
      labs(x = "Year", y = ylab)

 return(suppressWarnings(climtrendplot))
}


