#' @include getSondeInSitu.R
#' @include getWaterLevel.R
#' @include theme_WQ.R
#'
#' @title plotLakeProfile: Plots smoothed trend
#'
#' @importFrom dplyr group_by left_join mutate select summarize
#' @import ggplot2
#'
#' @description This function produces a heatmap filtered on park (mostly for ACAD), site, year, month, Sonde in situ
#' parameter and either sample relative to the surface or relative to surface elevation. Can only specify one parameter
#' at a time. If multiple sites or years are selected, plots will be faceted on those factors. Keep options limited
#' for best plotting. The option to plot relative to surface elevation uses the water level data and datum elevation
#' to convert sample depth to 1-m binned elevations. This allows you to see how the water column is shifting over time.
#' If you specify a lake x year x parameter combination that doesn't exist (e.g., a year a lake isn't sampled), the
#' function will return an error message instead of an empty plot.
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
#' @param years Numeric. Years to query. Accepted values start at 2006 for depth_type = "surface". For depth_type = "elev", accepted
#' values start at 2013, as water level data prior to 2013 are not available in the data package.
#'
#' @param months Numeric. Months to query by number. This function is restricted to months 5:10.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param parameter Specify the parameter to return (can only choose 1 per function call). Current accepted values are:.
#' c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#'  "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg").
#'
#' @param depth_type Specify whether to plot sample depth relative to elevation of surface water (depth_type = "elev"; default) or
#' depth, with each sample starting at 0 regardless of level of the lake surface (depth_type = "raw").
#'
#' @param color_theme Divering color palette for plots. Options currently are 'spectral' (default), 'ryb' (red - yellow - blue),
#' 'rb' (red - blue), and 'viridis' (yellow - green - blue) (see https://ggplot2-book.org/scales-colour for more info).
#'
#' @param color_rev Reverse the order of the color pallete. For example change ryb from red - yellow - blue to blue - yellow -red.
#'
#' @param title Logical. If TRUE (default) prints site name at top of figure. If FALSE, does not print site name. Only enabled when
#' one site is selected.
#'
#' @param legend_position Specify location of legend (default is 'right'). To turn legend off, use legend_position = "none". Other
#' options are "top", "bottom", "left", "right".
#'
#' @param ... Additional arguments relevant to \code{getSondeInSitu()} or \code{getWaterLevel()}
#'
#' @examples
#' \dontrun{
#'
#'
#'
#'}
#'
#' @return Returns a panel of hydrographs during the growing season for each year
#' in the data frame.
#'
#' @export
#'
plotLakeProfile <- function(park = "all", site = "all",
                      years = 2006:format(Sys.Date(), "%Y"),
                      months = 5:10, active = TRUE,
                      parameter = NA,
                      depth_type = 'elev',
                      color_theme = "spectral", color_rev = FALSE,
                      title = TRUE,
                      legend_position = 'right', ...){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  if(any(depth_type == 'elev' & years < 2013)){
    warning("Water level data not available before 2013. Select sample_depth = 'surface' to plot earlier data")}
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(5:10))
  stopifnot(class(active) == "logical")
  stopifnot(class(color_rev) == "logical")
  stopifnot(class(title) == "logical")
  depth_type <- match.arg(depth_type, c("elev", "raw"))
  color_theme <- match.arg(color_theme, c("spectral", "ryb", "rb", "viridis"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  if(length(parameter) > 1){stop("Can only use 1 parameter at a time.")}

  #-- Compile data for plotting --
  # combine sonde and water level data and group depths by 1 m bins
  wdat <- force(getSondeInSitu(park = park, site = site, site_type = "lake",
                         years = years, months = months, parameter = parameter, sample_depth = 'all', ...)) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Depth_m, param, value)

  lev <- force(getWaterLevel(park = park, site = site, site_type = 'lake', years = years,
                       months = months, ...))

  wcomb <- left_join(wdat, lev,
                     by = c("SiteCode", "SiteName", "UnitCode", "EventDate", "year", "month", "doy"))

  wcomb$depth_1m_bin <- ifelse(wcomb$Depth_m < 1, 0, round(wcomb$Depth_m, 0))

  wcomb2 <- wcomb |> group_by(SiteCode, SiteName, EventDate, year, month, doy, WaterLevel_m,
                              depth_1m_bin, param) |>
                     summarize(value = median(value), .groups = 'drop') |>
                     mutate(sample_elev = WaterLevel_m - depth_1m_bin)

  if(nrow(wcomb2) == 0){stop("Combination of sites, years and parameters returned a data frame with no records.")}

  #-- Set up plotting features --
  # create column of abbreviated months
  wcomb2$mon <- factor(wcomb2$month, levels = unique(wcomb2$month),
                       labels = unique(month.abb[wcomb2$month]))

  param_label <- unique(ifelse(grepl("_", wcomb2$param),
                        paste0(gsub("_", " (", wcomb2$param), ")"),
                        paste0(wcomb2$param)))

  ylab <- ifelse(depth_type == "elev", "Sample Elev. (m)", "Sample Depth (m)")

  facet_site <- ifelse(length(unique(wcomb2$SiteCode)) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(unique(wcomb2$year)) > 1, TRUE, FALSE)

  color_dir <- ifelse(color_rev == FALSE, -1, 1)
  plot_title <- ifelse(length(unique(wcomb2$SiteCode)) == 1 & title == TRUE, unique(wcomb2$SiteName), NULL)

  #-- Create plot --
  profplot <-
    if(depth_type == "elev"){
     ggplot(wcomb2, aes(x = mon, y = sample_elev, color = value, fill = value)) +
      geom_tile(aes(width = 1, height = 1)) +
      theme(legend.position = legend_position, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      theme_WQ() +
      # facets if more than 1 year or site
      {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteName + year)} +
      {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~SiteName)} +
      {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year)} +
      # color palettes
      {if(color_theme == 'spectral') scale_fill_distiller(palette = "Spectral", direction = color_dir)} +
      {if(color_theme == 'spectral') scale_color_distiller(palette = "Spectral", direction = color_dir)} +
      {if(color_theme == 'ryb') scale_fill_distiller(palette = "RdYlBu", direction = color_dir)} +
      {if(color_theme == 'ryb') scale_color_distiller(palette = "RdYlBu", direction = color_dir)} +
      {if(color_theme == 'rb') scale_fill_distiller(palette = "RdBu", direction = color_dir)} +
      {if(color_theme == 'rb') scale_color_distiller(palette = "RdBu", direction = color_dir)} +
      {if(color_theme == 'viridis') scale_fill_viridis_c(direction = color_dir)} +
      {if(color_theme == 'viridis') scale_color_viridis_c(direction = color_dir)} +
      # labels
      labs(x = "month", y = ylab, color = param_label, fill = param_label, title = plot_title)

    } else if(depth_type == "raw"){
      ggplot(wcomb2, aes(x = mon, y = -depth_1m_bin, color = value, fill = value)) +
        geom_tile(aes(width = 1, height = 1)) +
        theme(legend.position = legend_position, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        theme_WQ() +
        # facets if more than 1 year or site
        {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteCode + year)} +
        {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~Site)} +
        {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year)} +
        # color palettes
        {if(color_theme == 'spectral') scale_fill_distiller(palette = "Spectral", direction = color_dir)} +
        {if(color_theme == 'spectral') scale_color_distiller(palette = "Spectral", direction = color_dir)} +
        {if(color_theme == 'ryb') scale_fill_distiller(palette = "RdYlBu", direction = color_dir)} +
        {if(color_theme == 'ryb') scale_color_distiller(palette = "RdYlBu", direction = color_dir)} +
        {if(color_theme == 'rb') scale_fill_distiller(palette = "RdBu", direction = color_dir)} +
        {if(color_theme == 'rb') scale_color_distiller(palette = "RdBu", direction = color_dir)} +
        {if(color_theme == 'viridis') scale_fill_viridis_c(direction = color_dir)} +
        {if(color_theme == 'viridis') scale_color_viridis_c(direction = color_dir)} +
        # labels
        labs(x = "month", y = ylab, color = param_label, fill = param_label, title = plot_title)
      }

 return(suppressWarnings(profplot))
}


