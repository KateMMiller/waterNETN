#' @include getSondeInSitu.R
#' @include getWaterLevel.R
#' @include theme_WQ.R
#'
#' @title plotLakeProfile: Plot lake profiles
#'
#' @importFrom dplyr arrange group_by lead left_join mutate select summarize
#' @importFrom purrr pmap_dfr possibly
#' @import ggplot2
#'
#' @description This function produces a heatmap filtered on park (mostly for ACAD), site, year, month, Sonde in situ
#' parameter and either sample relative to the surface or relative to surface elevation. The y-axis is 1m bins for ACAD
#' and 0.25m bins for LNETN. If multiple samples are taken within the same bin, the median value is used. The width of
#' the profile columns reflects the number of days between sample events, centered on the day sampled. Shorter intervals
#' between events result in a narrower bar. You can only specify one parameter at a time. If multiple sites or years are
#' selected, plots will be faceted on those factors. Keep options limited for best plotting. The option to plot relative
#' to surface elevation uses the water level data and datum elevation to convert sample depth to 1-m or 0.25m binned elevations.
#' This allows you to see how the water column is shifting over time. If you specify a lake x year x parameter combination
#' that doesn't exist (e.g., a year a lake isn't sampled), the function will return an error message instead of an empty
#' plot.
#'
#' @param park Combine data from all parks or one or more parks at a time. Because ACAD lakes are binned by 1m intervals,
#' and LNETN are binned by 0.25m intervals, 'all' is not an option. You either must specify "ACAD" (default), "LNETN" or a LNETN park
#' with profile data. Valid inputs:
#' \describe{
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
#' @param event_type Select the event type. Options available are below Can only choose one option.
#' \describe{
#' \item{"all"}{All possible sampling events.}
#' \item{"VS"}{Default. NETN Vital Signs monitoring events, which includes Projects named 'NETN_LS' and 'NETN+ACID'.}
#' \item{"acid"}{Acidification monitoring events in Acadia.}
#' \item{"misc"}{Miscellaneous sampling events.}
#' }
#'
#' @param years Numeric. Years to query. Accepted values start at 2006 for depth_type = "surface". For depth_type = "elev", accepted
#' values start at 2013, as water level data prior to 2013 are not available in the data package.
#'
#' @param months Numeric. Months to query by number. This function is restricted to months 5:10. X-axis may plot weird if specifying
#' anything but 5:10 for months.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param parameter Specify the parameter to return (can only choose 1 per function call). Current
#' accepted values are:
#' c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#'  "Turbidity_FNU", "ChlA_EXO_RFU", "ChlA_EXO_ugL", "BP_mmHg").
#'
#' @param depth_type Specify whether to plot sample depth relative to elevation of surface water
#' (depth_type = "elev") or depth, with each sample starting at 0 regardless of level of the lake
#' surface (depth_type = "raw"; default).
#'
#' @param palette Diverging color palette for plots. Options currently are 'viridis'
#' (yellow - green - blue), 'mako' (light blue grading to black), or any built-in continuous color palette
#' available in RColorBrewer. Run RColorBrewer::display.brewer.all() to see the diverging color
#' palettes. Common palettes include "Blues", "BuGn", "RdPu", "Spectral", "RdYlBu", "RdBu", "PiYg".
#' See https://ggplot2-book.org/scales-colour for more info.
#'
#' @param color_rev Reverse the order of the color pallete. For example change RdYlBu from red - yellow - blue
#' to blue - yellow -red.
#'
#' @param plot_title Logical. If TRUE (default) prints site name at top of figure. If FALSE,
#' does not print site name. Only enabled when one site is selected.
#'
#' @param plot_thermocline Logical. If TRUE (default) plots the depth of the thermocline, calculated by
#' rLakeAnalyzer as the depth/elevation within the water column where the temperature gradient is the steepest
#' and indicates where the upper waters are typically not mixing with deeper waters. Only plots where at least
#' 5 depth measurements for temperature have been collected (may want to increase this threshold). Note that in
#' the rare cases that multiple sampling events occur within a month, both are plotted, but will have a thinner
#' column than other samples that are more spread out. If no thermocline is detected, as defined by
#' `rLakeAnalyzer::thermo.depth()`, no points are plotted.
#'
#' @param legend_position Specify location of legend (default is 'right'). To turn legend off, use legend_position = "none". Other
#' options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param facet_scales Specify whether facet axes should be fixed (all the same; default) or "free_y", "free_x" or "free" (both).
#'
#' @param ... Additional arguments relevant to \code{getSondeInSitu()} or \code{getWaterLevel()}
#'
#' @examples
#' \dontrun{
#'
#'# Plot temperature for Upper Hadlock for years 2013 - 2023 corrected by elevation with thermocline
#' # plotted as black lines.
#' plotLakeProfile(site = "ACUHAD", parameter = "Temp_C", depth_type = 'elev',
#'                 years = 2013:2023)
#'
#' # Plot temperature for Eagle Lake for years 2006 - 2023 with raw sample depth.
#' # Note that we can go back to 2006 because we're using raw sample depth instead of elevation.
#' plotLakeProfile(site = "ACEAGL", parameter = "Temp_C", depth_type = 'raw',
#'                years = 2006:2023)
#'
#' # Same plot as above, but with no plot title or thermocline.
#' plotLakeProfile(site = "ACEAGL", parameter = "Temp_C", depth_type = 'raw',
#'                 years = 2006:2023, plot_title = FALSE, plot_thermocline = F)
#' # Plot temperature for all ACAD lakes sampled in 2023 and raw sample depth.
#' lakes23 <- c("ACBUBL", "ACEAGL", "ACECHO", "ACJORD", "ACLONG", "ACROUN",
#'              "ACSEAL", "ACUBRK", "ACUHAD", "ACWHOL")
#' plotLakeProfile(park = "ACAD", site = lakes23, parameter = "Temp_C",
#'                depth_type = 'raw', years = 2023)
#'
#' # Plot DO all ACAD lakes sampled in 2023 and raw sample depth, using reversed ryb palette.
#' Temperature thermocline is also included as black lines.
#' plotLakeProfile(park = "ACAD", site = lakes23,
#'                parameter = "DOsat_pct", depth_type = 'raw', years = 2023,
#'                palette = 'RdYlBu', color_rev = TRUE)
#'}
#'
#' @return Returns a panel of hydrographs during the growing season for each year
#' in the data frame.
#'
#' @export
#'
plotLakeProfile <- function(park = "ACAD", site = "all", event_type = "VS",
                            years = 2006:format(Sys.Date(), "%Y"),
                            months = 5:10, active = TRUE,
                            parameter = NA, depth_type = 'raw',
                            palette = "Spectral", color_rev = FALSE,
                            plot_title = TRUE, plot_thermocline = TRUE,
                            legend_position = 'right', facet_scales = 'fixed',
                            gridlines = "none", ...){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  event_type <- match.arg(event_type, c("all", "VS", "acid", "misc"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  if(any(depth_type == 'elev' & years < 2013)){
    warning("Water level data not available before 2013. Select sample_depth = 'surface' to plot earlier data")}
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(5:10))
  stopifnot(class(active) == "logical")
  stopifnot(class(color_rev) == "logical")
  stopifnot(class(plot_title) == "logical")
  depth_type <- match.arg(depth_type, c("elev", "raw"))
  facet_scales <- match.arg(facet_scales, c("fixed", "free", "free_y", "free_x"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  if(length(parameter) > 1){stop("Can only use 1 parameter at a time.")}

  if(!palette %in% "viridis"){
    if(!requireNamespace("RColorBrewer", quietly = TRUE)){
      stop("Package 'RColorBrewer' needed if palette is anything but 'viridis'. Please install it.",
           call. = FALSE)
    }}

  #-- Compile data for plotting --
  # combine sonde and water level data and group depths by 1m or 0.25m bins
  wdat <- force(getSondeInSitu(park = park, site = site, site_type = "lake", event_type = event_type,
                         years = years, months = months, parameter = parameter, sample_depth = 'all', ...)) |>
          select(SiteCode, SiteName, UnitCode, EventDate, Project, year, month, doy, SampleDepth_m, Parameter, Value)

  lev <- force(getWaterLevel(park = park, site = site, site_type = 'lake', years = years,
                       months = months, ...))

  wcomb <- left_join(wdat, lev,
                     by = c("SiteCode", "SiteName", "UnitCode", "EventDate", "year", "month", "doy"))

  # Binning by ACAD or LNETN
  wcomb$depth_bin <-
    if(any(park == "ACAD")){
      ifelse(wcomb$SampleDepth_m < 1, 0, round(wcomb$SampleDepth_m, 0))
    } else {
      ifelse(wcomb$SampleDepth_m < 0.25, 0, floor(wcomb$SampleDepth_m/0.25) * 0.25)}

  wcomb2 <- wcomb |> group_by(SiteCode, SiteName, EventDate, year, month, doy, WaterLevel_m,
                              depth_bin, Parameter) |>
                     summarize(Value = median(Value), .groups = 'drop') |>
                     mutate(sample_elev = WaterLevel_m - depth_bin) |>
                     arrange(SiteCode, month, year, doy)

  if(nrow(wcomb2) == 0){stop("Combination of sites, years and parameters returned a data frame with no records.")}

  #-- Set up plotting features --
  # create column of abbreviated months
  wcomb2$mon <- factor(format(as.Date(wcomb2$EventDate, format = c("%Y-%m-%d")), "%b"), levels = month.abb, ordered = T)
  #wcomb2$mon <- factor(format(wcomb2$EventDate, "%b"), month.abb, ordered = TRUE)
  wcomb2$mon <- wcomb2$mon[,drop = T]

  param_label <- unique(ifelse(grepl("_", wcomb2$Parameter),
                        paste0(gsub("_", " (", wcomb2$Parameter), ")"),
                        paste0(wcomb2$Parameter)))

  ylab <- ifelse(depth_type == "elev", "Sample Elev. (m)", "Sample Depth (m)")

  facet_site <- ifelse(length(unique(wcomb2$SiteCode)) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(unique(wcomb2$year)) > 1, TRUE, FALSE)

  color_dir <- ifelse(color_rev == FALSE, -1, 1)
  ptitle <- if(length(unique(wcomb2$SiteCode)) == 1 & plot_title == TRUE){unique(wcomb2$SiteName)} else {NULL}

  prof_width <- wcomb2 |> select(SiteCode, year, doy) |> unique() |> arrange(SiteCode, year, doy)
  prof_width <- prof_width |> group_by(SiteCode, year) |>
    mutate(lag_doy = lag(doy, 1),
           lead_doy = lead(doy, 1))

  # Populate NAs with 121 and 304 as first and last day of monitoring period
  prof_width$lag_doy[is.na(prof_width$lag_doy) & prof_width$doy < 200] <-
    prof_width$doy[is.na(prof_width$lag_doy) & prof_width$doy < 200]  - 28 # beginning of sample period: May 1; 121
  prof_width$lead_doy[is.na(prof_width$lead_doy) & prof_width$doy > 273] <-
    prof_width$doy[is.na(prof_width$lead_doy) & prof_width$doy > 273] + 28 # end of sample period: Oct 31.; 304

  # Calculate width of profile columns as half of distance in days between samples on left and right.
  prof_width$lag_dif <- (prof_width$doy - prof_width$lag_doy)/2
  prof_width$lead_dif <- (prof_width$lead_doy - prof_width$doy)/2

  prof_width$col_width <- prof_width$lag_dif + prof_width$lead_dif
  prof_width$doy_plot <- prof_width$doy - prof_width$lag_dif + ((prof_width$lag_dif + prof_width$lead_dif)/2)

  # join prof params with original data
  wcomb3 <- left_join(wcomb2, prof_width, by = c("SiteCode", "year", "doy"))


  #-- Calculate thermocline --
  if(plot_thermocline == TRUE){
    if(!requireNamespace("rLakeAnalyzer", quietly = TRUE)){
      stop("Package 'rLakeAnalyzer' needed if plot_thermocline = TRUE. Please install it.", call. = FALSE)
    }
  # thermocline calcuated for temperature only
  temp <- force(getSondeInSitu(park = park, site = site, site_type = "lake",
                               years = years, months = months, parameter = "Temp_C", sample_depth = 'all', ...)) |>
    select(SiteCode, SiteName, UnitCode, EventDate, datetime, year, month, doy, SampleDepth_m, Parameter, Value)

  temp$depth_bin <-
    if(any(park == "ACAD")){
      ifelse(temp$SampleDepth_m < 1, 0, round(temp$SampleDepth_m, 0))
    } else {
      ifelse(temp$SampleDepth_m < 0.25, 0, floor(temp$SampleDepth_m/0.25) * 0.25)}

  # Check number of depths measured. If < 5 will be dropped from thermocline calculation
  num_depths <- temp |> group_by(SiteCode, year, month, doy) |>
    summarize(num_meas = sum(!is.na(SampleDepth_m)), .groups = 'drop') |>  # this turns into logical, where every depth with a value = 1 and is summed
    filter(num_meas >= 5)

  # Drop temp data from samples with < 5 samples with left join
  temp1 <- left_join(num_depths, temp, by = c("SiteCode", "year", "month", "doy"))

  # Check for multiple sampling events within a month, and take the first.
  num_evs <- temp |> select(SiteCode, year, month, doy) |> unique() |>
    group_by(SiteCode, year, month, doy) |> slice(1)

  # Take only first temp if two samples on the same day
  temp1b <- left_join(num_evs, temp1, by = c("SiteCode", "year", "month", "doy"))

  temp2 <- temp1b |>
    group_by(SiteCode, SiteName, EventDate, datetime, year, month, doy,
             depth_bin, Parameter) |>
    summarize(Value = median(Value), .groups = 'drop')

  site_list <- sort(unique(temp2$SiteCode))
  year_list <- sort(unique(temp2$year))
  mon_list <- sort(unique(temp2$month))

  all_params <- unique(data.frame(site = temp2$SiteCode, yr = temp2$year, doy = temp2$doy))
  param_list <- list(all_params[[1]], all_params[[2]], all_params[[3]])

  # calc. thermocline on all site, year, month combinations in dataset
  tcline1 <-
    pmap_dfr(param_list, function(site, yr, day){
      df <- temp2 |> filter(SiteCode == site) |> filter(year == yr) |> filter(doy == day)
      dfmonth <- unique(temp2$month)
      tc <- rLakeAnalyzer::thermo.depth(wtr = df$Value, depths = df$depth_bin)
      tcdf <- data.frame(SiteCode = site, year = yr, month = dfmonth, doy = day, Value = tc)
      return(tcdf)
    })
  # add water level to temp data for depth_type = 'elev'
  tcomb <- left_join(temp2, lev,
                     by = c("SiteCode", "SiteName", "EventDate",
                            "year", "month", "doy"))
  # add thermocline to temp/wl data
  tcline2 <-
    if(depth_type == "elev"){
       left_join(tcomb |> select(SiteCode, SiteName, year, month, doy, WaterLevel_m) |> unique(),
                  tcline1, by = c("SiteCode", "year", "month", "doy")) |>
          mutate(Value = WaterLevel_m - Value,
                 mon = factor(month, levels = unique(month),
                 labels = unique(month.abb[month])))
    } else if(depth_type == "raw"){
      left_join(tcomb |> select(SiteCode, SiteName, year, month, doy) |> unique(),
                tcline1, by = c("SiteCode", "year", "month", "doy")) |>
        mutate(mon = factor(month, levels = unique(month),
                            labels = unique(month.abb[month])))
    }

  tcline <- tcline2[!is.na(tcline2$Value),]
  # add prof width data to tcline
  prof_unique <- prof_width |> select(SiteCode, year, doy, doy_plot, col_width) |> unique()

  tcline_final <- left_join(tcline, prof_unique, by = c("SiteCode", "year", "doy"))
  }

  #-- Create plot --
  profplot <-
    if(depth_type == "elev"){
     ggplot(wcomb3 |> droplevels(), aes(x = doy_plot, y = sample_elev)) +
      geom_tile(aes(width = col_width, height = 1, color = Value, fill = Value), na.rm = F) +
      theme(legend.position = legend_position, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      theme_WQ() +
      # plot thermocline as point
      {if(plot_thermocline == TRUE){
        geom_segment(data = tcline_final,
                     aes(x = doy_plot - (col_width/2), xend = doy_plot + (col_width/2),
                         y = Value, yend = Value), linewidth = 0.7) }}+
        #geom_point(data = tcline, aes(x = mon, y = value), color = 'black', show.legend = F)}} +
      # facets if more than 1 year or site
      {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteName + year, drop = T, scales = facet_scales)} +
      {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~SiteName, drop = T, scales = facet_scales)} +
      {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year, drop = T, scales = facet_scales)} +
      # color palettes
      {if(palette == 'viridis') scale_fill_viridis_c(direction = color_dir)} +
      {if(palette == 'viridis') scale_color_viridis_c(direction = color_dir)} +
      {if(!palette %in% 'viridis') scale_fill_distiller(palette = palette, direction = color_dir)} +
      {if(!palette %in% 'viridis') scale_color_distiller(palette = palette, direction = color_dir)} +
      # labels, themes
      labs(x = NULL, y = ylab, color = param_label, fill = param_label, title = ptitle) +
      {if(any(gridlines %in% c("grid_y", "both"))){
          theme(
            panel.grid.major.y = element_line(color = 'grey'),
            panel.grid.minor.y = element_line(color = 'grey'))}} +
      {if(any(gridlines %in% c("grid_x", "both"))){
          theme(
            panel.grid.major.x = element_line(color = 'grey'),
            panel.grid.minor.x = element_line(color = 'grey'))}} +
      scale_y_continuous(breaks = pretty(wcomb3$sample_elev, n = 8)) +
      scale_x_continuous(limits = c(115, 320),
                         breaks = c(121, 152, 182, 213, 244, 274, 305),
                         labels = c("May-1", "Jun-1", "Jul-1", "Aug-1", "Sep-1", "Oct-1", "Nov-1"))

    } else if(depth_type == "raw"){
      ggplot(wcomb3 |> droplevels(), aes(x = doy_plot, y = -depth_bin)) +
        geom_tile(aes(width = col_width, height = 1, color = Value, fill = Value)) +
        theme(legend.position = legend_position, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        theme_WQ() +
        {if(plot_thermocline == TRUE){
          geom_segment(data = tcline_final,
                       aes(x = doy_plot - (col_width/2), xend = doy_plot + (col_width/2),
                           y = -Value, yend = -Value), size = 0.7) }} +
        # facets if more than 1 year or site
        {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteName + year, drop = T, scales = facet_scales)} +
        {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~SiteName, drop = T, scales = facet_scales)} +
        {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year, drop = T, scales = facet_scales)} +
        # color palettes
        {if(palette == 'viridis') scale_fill_viridis_c(direction = color_dir)} +
        {if(palette == 'viridis') scale_color_viridis_c(direction = color_dir)} +
        {if(!palette %in% 'viridis') scale_fill_distiller(palette = palette, direction = color_dir)} +
        {if(!palette %in% 'viridis') scale_color_distiller(palette = palette, direction = color_dir)} +
        # labels, themes
        labs(x = NULL, y = ylab, color = param_label, fill = param_label, title = ptitle) +
        {if(any(gridlines %in% c("grid_y", "both"))){
          theme(
            panel.grid.major.y = element_line(color = 'grey'),
            panel.grid.minor.y = element_line(color = 'grey'))}} +
        {if(any(gridlines %in% c("grid_x", "both"))){
          theme(
            panel.grid.major.x = element_line(color = 'grey'),
            panel.grid.minor.x = element_line(color = 'grey'))}} +
        scale_y_continuous(breaks = pretty(-wcomb3$depth_bin, n = 8)) +
        scale_x_continuous(limits = c(115, 320),
                           breaks = c(121, 152, 182, 213, 244, 274, 305),
                           labels = c("May-1", "Jun-1", "Jul-1", "Aug-1", "Sep-1", "Oct-1", "Nov-1"))
      }

 return(#suppressWarnings(
   profplot)#)
}


