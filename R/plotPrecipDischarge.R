#' @include getSites.R
#' @include theme_WQ.R
#'
#' @title plotPrecipDischarge: Plot precipitation and discharge
#'
#' @importFrom dplyr filter select
#' @import ggplot2
#'
#' @description This function produces a plot with dual y-axes, one for precipitation an done for discharge.
#' The x-axis is date. If multiple sites and/or years are specified, they will be plotted as separate figures.
#' Note that lines aren't plotted for discharge because discharge likely varies a lot between samples.
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
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param rating Filter on measurement rating. Can choose multiple. Default is all.
#' \describe{
#' \item{"all"}{All measurements}
#' \item{"E"}{Excellent}
#' \item{"G"}{Good}
#' \item{"F"}{Fair}
#' \item{"P"}{Poor}
#' }
#'
#' @param units Specify if you want Scientific or English units. Acceptable values are "sci" (default) and "eng".
#' If "sci" precipitation units are mm; if "eng", precipitation units are in inches.
#'
#' @param palette Change colors of precipitation bar plot and discharge point and line plot. Default is c('#257EF6', 'black'), which are dark blue for precipitation,
#' and black for discharge. Y axes are color coded the same.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @examples
#' \dontrun{
#'
#' # Plot daily precipitation vs discharge for Mill Brook in MIMA for past 6 years using default colors.
#' plotPrecipDischarge(site = c("MIMASA"), years = 2019:2024)
#'
#' # Plot daily precipitation versus discharge for Aunt Betty Inlet and Kebo Stream for 2024 using different colors.
#' # Note that this can be slow because has to download precip. data from NADP.
#' # LNETN parks download from a faster web service.
#' plotPrecipDischarge(site = c("ACABIN", "ACKEBO"), years = 2024, palette = c("cornflowerblue", "orange"))
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotPrecipDischarge <- function(park = "all",
                                site = "all",
                                years = 2006:format(Sys.Date(), "%Y"),
                                #layers = c("points", "lines"),
                                months = 5:10,
                                active = TRUE,
                                rating = 'all',
                                units = 'sci',
                                palette = c("#257EF6", "black"),
                                legend_position = 'none',
                                gridlines = "none"){

  #-- Error handling --
  # check for required packages for certain arguments
  if(!requireNamespace("scales", quietly = TRUE)){
    stop("Package 'scales' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Check that suggested package required for this function are installed
  if(!requireNamespace("climateNETN", quietly = TRUE)){
    stop("Package 'climateNETN' needed to download weather station data. Please install it from github.", call. = FALSE)
  }
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  rating <- match.arg(rating, several.ok = T, c("all", "E", "G", "F", "P"))
  stopifnot(length(palette) == 2)
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  units <- match.arg(units, c("sci", "eng"))

  #-- Compile data for plotting --
  disch <- getDischarge(park = park, site = site, active = active, years = years, months = months, rating = rating) |>
    select(UnitCode, SiteCode, SiteName, EventDate, year, month, doy, Discharge_cfs)

  disch$Date2 <- as.Date(disch$EventDate, format = c("%Y-%m-%d"))

  # Use years in disch to drop years a site isn't sampled in precip download.
  sample_years <- sort(unique(disch$year))

  precip <- climateNETN::getClimWStat(park = park, years = sample_years) |> filter(month %in% months)

  precip$Date2 <- as.Date(precip$Date, format = c("%Y-%m-%d"))

  # comb <- left_join(precip, disch, by = c("UnitCode", "year", "doy", "month", "Date2")) |>
  #   filter(year %in% sample_years)

  # Control x-axis breaks
  datebreaks <- seq(min(precip$Date2), max(precip$Date2) + 30, by = "1 month")

  facet_site <- ifelse(length(unique(disch$SiteCode)) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(unique(disch$year)) > 1, TRUE, FALSE)

  precip$precip <- if(units == "sci"){precip$ws_pcpmm} else {precip$ws_pcpmm/25.4}
  ylab <- if(units == "sci"){"Daily Precip. (mm)"} else {"Daily Precip. (in)"}

  # Have to rescale so precip and discharge show up on same plot
  if(sum(disch$Discharge_cfs) > 0){
  scale = range(precip$precip, na.rm = T)[2]/range(disch$Discharge_cfs, na.rm = T)[2]
  } else if(sum(disch$Discharge_cfs) == 0){
    scale = range(precip$precip, na.rm = T)[2]/10
  }


dp_plot <-
    ggplot(precip, aes(x = Date2, y = precip)) + theme_WQ() +
      # layers y-left
      geom_bar(color = palette[1], fill = palette[1], stat = 'identity', alpha = 0.5) +
      # layers y-right
      geom_point(data = disch, aes(x = Date2, y = Discharge_cfs * scale, group = SiteName),
                 color = palette[2], shape = "+", size = 6) +
      # create 2nd axis
      scale_y_continuous(name = ylab, n.breaks = 8,
                         sec.axis = sec_axis(~./scale, name = "Discharge (cfs)")) +
      # facets
      {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteName + year, drop = T, scales = "free_x")} +
      {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~SiteName, drop = T)} +
      {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year, drop = T, scales = "free_x")} +
      # formatting
      theme(axis.text.y.left = element_text(color = palette[1]),
            axis.title.y.left = element_text(color = palette[1]),
            axis.text.y.right = element_text(color = palette[2]),
            axis.title.y.right = element_text(color = palette[2]),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      {if(any(gridlines %in% c("grid_y", "both"))){
        theme(
          panel.grid.major.y = element_line(color = 'grey'),
          panel.grid.minor.y = element_line(color = 'grey'))}}+
      {if(any(gridlines %in% c("grid_x", "both"))){
        theme(
          panel.grid.major.x = element_line(color = 'grey'),
          panel.grid.minor.x = element_line(color = 'grey'))}}+
      scale_x_date(breaks = datebreaks, labels = scales::label_date("%b")) +
      theme(legend.position = legend_position, legend.title = element_blank()) +
      labs(x = NULL)


return(#suppressWarnings(
  dp_plot)#)
}


