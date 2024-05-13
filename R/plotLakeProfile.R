#' @include getSondeInSitu.R
#' @include getWaterLevel.R
#' @include theme_WQ.R
#'
#' @title plotLakeProfile: Plots smoothed trend
#'
#' @importFrom dplyr arrange group_by left_join mutate select summarize
#' @importFrom purrr pmap_dfr possibly
#' @import ggplot2
#'
#' @description This function produces a heatmap filtered on park (mostly for ACAD), site, year, month, Sonde in situ
#' parameter and either sample relative to the surface or relative to surface elevation. The y-axis is 1m bins. If multiple
#' samples occur within a 1-m depth, the value plotted is the median. Can only specify one parameter
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
#' @param plot_title Logical. If TRUE (default) prints site name at top of figure. If FALSE, does not print site name. Only enabled when
#' one site is selected.
#'
#' @param plot_thermocline Logical. If TRUE (default) plots the depth of the thermocline, calculated by rLakeAnalyzer as the depth/elevation
#' within the water column where the temperature gradient is the steepest and indicates where the upper waters are typically not mixing with
#' deeper waters. Only plots where at least 5 depth measurements for temperature have been collected (may want to increase this threshold).
#' Note that in the rare cases that multiple sampling events occur within a month, only the first is plotted. If no thermocline is detected,
#' as defined by `rLakeAnalyzer::thermo.depth()`, no points are plotted.
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
                      plot_title = TRUE,
                      plot_thermocline = TRUE,
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
  stopifnot(class(plot_title) == "logical")
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
                     mutate(sample_elev = WaterLevel_m - depth_1m_bin) |>
                     arrange(SiteCode, month, year, doy)

  if(nrow(wcomb2) == 0){stop("Combination of sites, years and parameters returned a data frame with no records.")}

  #-- Set up plotting features --
  # create column of abbreviated months
  wcomb2$mon <- factor(wcomb2$month, levels = sort(unique(wcomb2$month)),
                       labels = unique(month.abb[wcomb2$month]))

  param_label <- unique(ifelse(grepl("_", wcomb2$param),
                        paste0(gsub("_", " (", wcomb2$param), ")"),
                        paste0(wcomb2$param)))

  ylab <- ifelse(depth_type == "elev", "Sample Elev. (m)", "Sample Depth (m)")

  facet_site <- ifelse(length(unique(wcomb2$SiteCode)) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(unique(wcomb2$year)) > 1, TRUE, FALSE)

  color_dir <- ifelse(color_rev == FALSE, -1, 1)
  ptitle <- if(length(unique(wcomb2$SiteCode)) == 1 & plot_title == TRUE){unique(wcomb2$SiteName)} else {NULL}

  #-- Calculate thermocline --
  if(plot_thermocline == TRUE){
    if(!requireNamespace("rLakeAnalyzer", quietly = TRUE) & depth_type %in% c('DSN', 'dbfile')){
      stop("Package 'rLakeAnalyzer' needed for if plot_thermocline = TRUE. Please install it.", call. = FALSE)
    }
  # thermocline calcuated for temperature only
  temp <- force(getSondeInSitu(park = park, site = site, site_type = "lake",
                               years = years, months = months, parameter = "Temp_C", sample_depth = 'all', ...)) |>
    select(SiteCode, SiteName, UnitCode, EventDate, datetime, year, month, doy, Depth_m, param, value)

  temp$depth_1m_bin <- ifelse(temp$Depth_m < 1, 0, round(temp$Depth_m, 0))

  # Check number of depths measured. If < 5 will be dropped from thermocline calculation
  num_depths <- temp |> group_by(SiteCode, year, month) |>
    summarize(num_meas = sum(!is.na(Depth_m)), .groups = 'drop') |>  # this turns into logical, where every depth with a value = 1 and is summed
    filter(num_meas >= 5)

  # Drop temp data from samples with < 5 samples with left join
  temp1 <- left_join(num_depths, temp, by = c("SiteCode", "year", "month"))

  # Check for multiple sampling events within a month, and take the first.
  num_evs <- temp |> select(SiteCode, year, month, doy) |> unique() |>
    group_by(SiteCode, year, month) |> slice(1)

  # Take only first temp sample within a month
  temp1b <- left_join(num_evs, temp1, by = c("SiteCode", "year", "month", "doy"))

  temp2 <- temp1b |>
    group_by(SiteCode, SiteName, EventDate, datetime, year, month, doy,
             depth_1m_bin, param) |>
    summarize(value = median(value), .groups = 'drop')

  site_list <- sort(unique(temp2$SiteCode))
  year_list <- sort(unique(temp2$year))
  mon_list <- sort(unique(temp2$month))

  all_params <- unique(data.frame(site = temp2$SiteCode, yr = temp2$year, mon = temp2$month))
  param_list <- list(all_params[[1]], all_params[[2]], all_params[[3]])

  # calc. thermocline on all site, year, month combinations in dataset
  tcline1 <-
    pmap_dfr(param_list, function(site, yr, mon){
      df <- temp2 |> filter(SiteCode == site) |> filter(year == yr) |> filter(month == mon)
      tc <- rLakeAnalyzer::thermo.depth(wtr = df$value, depths = df$depth_1m_bin)
      tcdf <- data.frame(SiteCode = site, year = yr, month = mon, value = tc)
      return(tcdf)
    })
  # add water level to temp data for depth_type = 'elev'
  tcomb <- left_join(temp2, lev,
                     by = c("SiteCode", "SiteName", "EventDate",
                            "year", "month", "doy"))
  # add thermocline to temp/wl data
  tcline2 <-
    if(depth_type == "elev"){
       left_join(tcomb |> select(SiteCode, SiteName, year, month, WaterLevel_m) |> unique(),
                  tcline1, by = c("SiteCode", "year", "month")) |>
          mutate(value = WaterLevel_m - value,
                 mon = factor(month, levels = unique(month),
                               labels = unique(month.abb[month])))
    } else if(depth_type == "raw"){
      left_join(tcomb |> select(SiteCode, SiteName, year, month) |> unique(),
                tcline1, by = c("SiteCode", "year", "month")) |>
        mutate(mon = factor(month, levels = unique(month),
                            labels = unique(month.abb[month])))
    }
  tcline <- tcline2[!is.na(tcline2$value),]
  }



  #-- Create plot --
  profplot <-
    if(depth_type == "elev"){
     ggplot(wcomb2 |> droplevels(), aes(x = mon, y = sample_elev)) +
      geom_tile(aes(width = 1, height = 1, color = value, fill = value), na.rm = F) +
      theme(legend.position = legend_position, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      theme_WQ() +
      # plot thermocline as point
      {if(plot_thermocline == TRUE){
        geom_point(data = tcline, aes(x = mon, y = value), color = 'black', show.legend = F)}} +
      # facets if more than 1 year or site
      {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteName + year, drop = T)} +
      {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~SiteName, drop = T)} +
      {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year, drop = T)} +
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
      labs(x = NULL, y = ylab, color = param_label, fill = param_label, title = ptitle)

    } else if(depth_type == "raw"){
      ggplot(wcomb2 |> droplevels(), aes(x = mon, y = -depth_1m_bin)) +
        geom_tile(aes(width = 1, height = 1, color = value, fill = value)) +
        theme(legend.position = legend_position, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        theme_WQ() +
        {if(plot_thermocline == TRUE){geom_point(data = tcline, aes(x = mon, y = -value), color = 'black')}} +
        # facets if more than 1 year or site
        {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteName + year, drop = T)} +
        {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~SiteName, drop = T)} +
        {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year, drop = T)} +
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
        labs(x = NULL, y = ylab, color = param_label, fill = param_label, title = ptitle)
      }

 return(#suppressWarnings(
   profplot)#)
}


