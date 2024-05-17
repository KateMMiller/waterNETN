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
#' @param facet_param Logical. If TRUE (Default), plots parameters on separate facets. If FALSE, plots
#' all parameters on the same figure. Note that results will be funky if selected parameters have different units (e.g., temp and precip).
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
#' # Plot weather station precip for the Pogue in MABI for 2006:2023 and all months, without smoothing
#' plotClimTrend(site = "MABIPA", years = 2006:2023, parameter = "ws_ppt_mm", smooth = F)
#'
#' # Plot monthly weather station data for Pogue Stream in MABI from 2006:2023, with smoothed line and span 0.7.
#' plotClimTrend(site = "MABIPA", years = 2006:2023, parameter = "ws_ppt_mm", span = 0.7)
#'
#' # Plot monthly Daymet mean temperature for Pogue Stream in MABI and Kroma Kill in SARA, and  from 2006:2023, with smoothed line, span 0.7, and only sample months.
#' plotClimTrend(site = c("MABIPA", "SARASA"), years = 2006:2023, parameter = "dm_tmean_C", span = 0.7, months = 5:10)
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
                          layers = c("points", "lines"),
                          parameter = NA,
                          facet_site = FALSE, facet_param = TRUE,
                          color_theme = 'viridis', smooth = TRUE,
                          span = 0.3,
                          legend_position = 'none', ...){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)

  dm_param <- c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C",
                "dm_tmean_C", "dm_srad", "dm_PEThar",
                "dm_BAL", "dm_SPEI01", "dm_SPEI03")

  ws_param <- c( "ws_ppt_mm", "ws_tmax_C", "ws_tmin_C", "ws_tmean_C")

  parameter <- match.arg(parameter, c("all", dm_param, ws_param), several.ok = TRUE)

  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  stopifnot(class(smooth) == "logical")
  stopifnot(class(span) %in% "numeric")
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  color_theme <- match.arg(color_theme, c("viridis", "set1", "dark2", "accent"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))

  #-- Compile data for plotting --
  clim_dat <-
    if(any(parameter == "all") | (any(parameter %in% dm_param) & any(parameter %in% ws_param))){
      sumClimMonthly(park = park, site = site, site_type = site_type, active = active, years = years,
                     months = months, data_type = 'all')
    } else if(all(parameter %in% dm_param)){
      sumClimMonthly(park = park, site = site, site_type = site_type, active = active, years = years,
                     months = months, data_type = 'daymet')
    } else if(all(parameter %in% ws_param)){
      sumClimMonthly(park = park, site = site, site_type = site_type, active = active, years = years,
                     months = months, data_type = 'wstn')
    }

  if(nrow(clim_dat) == 0){stop("Specified arguments returned a data frame with 0 records.")}

  param <- if(any(parameter == "all")){c(dm_param, ws_param)} else {parameter}

  clim_dat2 <- clim_dat[,c("SiteCode", "SiteName", "UnitCode", "month", "mon", "year", param)]

  clim_dat_long <- clim_dat2 |> pivot_longer({{param}}, names_to = "param", values_to = "value") |>
    arrange(SiteCode, month, param)

  clim_dat_long$date <- as.Date(paste0(clim_dat_long$year, "-", clim_dat_long$month, "-", 15), format = "%Y-%m-%d")

  #-- Set up plotting features --
  ylab <- ifelse(length(unique(clim_dat_long$param)) == 1, unique(clim_dat_long$param), "Monthly Value")

  facetsite <- ifelse(facet_site == TRUE & length(unique(clim_dat_long$SiteCode)) > 1, TRUE, FALSE)
  facetparam <- ifelse(facet_param == TRUE & length(unique(clim_dat_long$param)) > 1, TRUE, FALSE)

  pars <- c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C", "dm_tmean_C", "dm_BAL", "dm_SPEI01", "dm_SPEI03",
            "ws_ppt_mm", "ws_tmax_C", "ws_tmin_C", "ws_tmean_C")

  plabs <- c("Gridded Total Precip. (mm)", "Gridded Max. Temp. (C)", "Gridded Min. Temp. (C)",
             "Gridded Avg. Temp. (C)", "Water Balance", "SPEI - 1 month", "SPEI - 3 month",
             "WS Total Precip. (mm)", "WS Max. Temp. (C)", "WS Min. Temp. (C)", "WS Avg. Temp (C)")

  param_labels <- data.frame(
                    param = c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C", "dm_tmean_C", "dm_BAL", "dm_SPEI01", "dm_SPEI03",
                              "ws_ppt_mm", "ws_tmax_C", "ws_tmin_C", "ws_tmean_C"),
                    param_label = c("DM Total Precip. (mm)", "DM Max. Temp. (C)", "DM Min. Temp. (C)",
                                    "DM Avg. Temp. (C)", "Water Balance", "SPEI - 1 month", "SPEI - 3 month",
                                    "WS Total Precip. (mm)", "WS Max. Temp. (C)", "WS Min. Temp. (C)",
                                    "WS Avg. Temp (C)"))

  clim_dat_long <- left_join(clim_dat_long, param_labels, by = 'param')

  ylab <- ifelse(length(parameter) > 1, "Monthly Value", param_labels$param_label[param_labels$param == parameter])

  clim_dat_long$date2 <- as.Date(clim_dat_long$date, format = c("%Y-%m-%d"))

  year_len <- length(unique(clim_dat_long$year))
  mon_len <- length(unique(clim_dat_long$month))

  break_len <- if(year_len == 1){"1 month"
  } else if(year_len  %in% c(2, 3, 4) & mon_len <= 6){"2 months"
  } else if(year_len == 2 & mon_len > 6){"4 months"
    #} else if(year_len > 4 & mon_len <= 6){"6 months"
  } else if(year_len %in% c(4, 5, 6)){"1 year"
  } else if(year_len > 6){"2 years"
  } else {"6 months"}

  date_format <- ifelse(break_len %in% c("1 year", "2 years"), "%Y", "%m/%d/%Y")
  datebreaks <- seq(min(clim_dat_long$date2), max(clim_dat_long$date2) + 30, by = break_len)

  #-- Create plot --
  climtrendplot <-
    if(facetparam == FALSE & facetsite == FALSE){
    ggplot(clim_dat_long, aes(x = date2, y = value,
            group = interaction(param_label, SiteName),
            color = interaction(param_label, SiteName),
            fill = interaction(param_label, SiteName))) +
      # layers
      {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span) } +
      {if(smooth == FALSE & any(layers %in% "lines")) geom_line()} +
      {if(any(layers %in% "points")) geom_point(alpha = 0.6)} +
      # themes
      theme_WQ() + theme(legend.position = legend_position,
                         legend.title = element_blank(),
                         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      # palettes
      {if(color_theme == "viridis") scale_color_viridis_d()} +
      {if(color_theme == "set1") scale_color_brewer(palette = "Set1")} +
      {if(color_theme == "dark2") scale_color_brewer(palette = "Dark2")} +
      {if(color_theme == "accent") scale_color_brewer(palette = "Accent")} +
      {if(color_theme == "viridis") scale_fill_viridis_d()} +
      {if(color_theme == "set1") scale_fill_brewer(palette = "Set1")} +
      {if(color_theme == "dark2") scale_fill_brewer(palette = "Dark2")} +
      {if(color_theme == "accent") scale_fill_brewer(palette = "Accent")} +
      # axis format
      scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
      # labels
      labs(x = NULL, y = ylab)
    } else if(facetparam == FALSE & facetsite == TRUE){
      ggplot(clim_dat_long, aes(x = date2, y = value,
                                group = param_label,  color = param_label,
                                fill = param_label)) +
        # layers
        {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span) } +
        {if(smooth == FALSE & any(layers %in% "lines")) geom_line()} +
        {if(any(layers %in% "points")) geom_point(alpha = 0.6)} +
        # facets
        facet_wrap(~SiteName, drop = T) +
        # themes
        theme_WQ() + theme(legend.position = legend_position,
                           legend.title = element_blank(),
                           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        # palettes
        {if(color_theme == "viridis") scale_color_viridis_d()} +
        {if(color_theme == "set1") scale_color_brewer(palette = "Set1")} +
        {if(color_theme == "dark2") scale_color_brewer(palette = "Dark2")} +
        {if(color_theme == "accent") scale_color_brewer(palette = "Accent")} +
        {if(color_theme == "viridis") scale_fill_viridis_d()} +
        {if(color_theme == "set1") scale_fill_brewer(palette = "Set1")} +
        {if(color_theme == "dark2") scale_fill_brewer(palette = "Dark2")} +
        {if(color_theme == "accent") scale_fill_brewer(palette = "Accent")} +
        # axis format
        scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
        # labels
        labs(x = NULL, y = ylab)
    } else if(facetparam == TRUE & facetsite == TRUE){
      ggplot(clim_dat_long, aes(x = date2, y = value,
                                group = SiteName,  color = SiteName,
                                fill = SiteName)) +
        # layers
        {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span) } +
        {if(smooth == FALSE & any(layers %in% "lines")) geom_line()} +
        {if(any(layers %in% "points")) geom_point(alpha = 0.6)} +
        # facets
        facet_wrap(~SiteName + param_label, drop = T) +
        # themes
        theme_WQ() + theme(legend.position = legend_position,
                           legend.title = element_blank(),
                           axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        # palettes
        {if(color_theme == "viridis") scale_color_viridis_d()} +
        {if(color_theme == "set1") scale_color_brewer(palette = "Set1")} +
        {if(color_theme == "dark2") scale_color_brewer(palette = "Dark2")} +
        {if(color_theme == "accent") scale_color_brewer(palette = "Accent")} +
        {if(color_theme == "viridis") scale_fill_viridis_d()} +
        {if(color_theme == "set1") scale_fill_brewer(palette = "Set1")} +
        {if(color_theme == "dark2") scale_fill_brewer(palette = "Dark2")} +
        {if(color_theme == "accent") scale_fill_brewer(palette = "Accent")} +
        # axis format
        scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
        # labels
        labs(x = NULL, y = ylab)
    }


 return(#suppressWarnings(
   climtrendplot)#)
}


