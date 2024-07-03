#' @include getChemistry.R
#' @include getDischarge.R
#' @include getLightPen.R
#' @include getSecchi.R
#' @include getSondeInSitu.R
#' @include getWaterLevel.R
#' @include theme_WQ.R
#'
#' @title plotTrend: Plots smoothed trend
#'
#' @importFrom dplyr mutate select
#' @import ggplot2
#'
#' @description This function produces a line or smoothed trend plot filtered on park, site, year, month, and parameter.
#' Works with both lab chemistry data and Sonde in situ data. If multiple sites are specified, they will be plotted
#' on the same figure. If multiple parameters are specified, they will be plotted on separate figures. Note that
#' if you specify a stream and parameter combination that doesn't exist (e.g., a stream site and a parameter only
#' collected in lakes), the function will return an error message instead of an empty plot.
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
#' @param parameter Specify the parameter(s) to return. Current accepted values are:.
#' chemistry: c("ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
#' "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3_mgL", "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL",
#' "TN_mgL", "TotDissN_mgL", "TotDissP_ugL", "TP_ugL")
#' sonde: c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#' "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg").
#' other: c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m").
#' Note that "all" is not an accepted value, because there are too many to plot.
#'
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the MDL/MRL/UQL values in the parameter flags. If the Flag column is not NA, that indicates
#' the value is a censored value. If FALSE (Default), only non-censored values are returned in the value column.
#'
#' @param sample_depth Filter on sample depth. If "all" (Default), returns all sample depths. If "surface",
#' only returns the median value of samples collected <= 2m from the surface. SampleDepth_m is also the median
#' sample depth of samples collected within 2m of the surface. Note that for the Penetration Ratio parameter,
#' all sample depths are plotted. Plotting all depths may return a funky plot for other parameters.
#'
#' @param layers Options are "points" and "lines". By default, both will plot.
#'
#' @param palette Theme to plot points and lines. Options currently are 'viridis' (Default- ranges of blue, green and yellow), or discrete palettes from RColorBrewer. Common options are "Set1", "Set2", "Dark2", "Accent".
#' Run RColorBrewer::display.brewer.all(type = 'qual') to see full set of options.
#'
#' @param threshold Logical. If TRUE (Default), will plot a dashed (upper) or dotted (lower) line if a water quality threshold exists for that
#' parameter and site. If FALSE, no threshold line will be plotted.
#'
#' @param smooth Logical. If TRUE (Default), will plot a loess smoothed line. If FALSE, will plot actual line. Only
#' plots if layers argument includes 'lines'.
#'
#' @param span Numeric. Determines how smoothed the line will be for smooth = TRUE. Default is 0.3. Higher spans (up to 1)
#' cause more smoothing. Span can range from 0 to 1.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when either multiple years
#' are specified or multiple parks. Default is 2.
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param ... Additional arguments relevant to \code{getChemistry()} or \code{getSondeInSitu()}
#'
#' @examples
#' \dontrun{
#'
#' # Plot smoothed surface pH for Eagle Lake for past 3 years using default span of 0.3 and by default not including the legend.
#' plotTrend(site = "ACEAGL", parameter = "pH", palette = 'Dark2', years = 2021:2023) + theme_WQ()
#'
#' # Plot smoothed surface pH for Eagle Lake for all years, removing the legend and using span of 0.75.
#' plotTrend(site = "ACEAGL", parameter = "pH", span = 0.75)
#'
#' # Plot smoothed Secchi Depth in Jordan Pond for all years, including the legend, different color palette, and using span of 0.75.
#' plotTrend(site = "ACJORD", parameter = "SDepth_m", span = 0.75, palette = 'Set1')
#'
#' # Plot smoothed surface pH for active SARA streams over all years with 0.6 span.
#' plotTrend(park = "SARA", site = c("SARASA", "SARASC", "SARASD"), site_type = "stream", parameter = "pH",
#'           legend_position = "right", span = 0.6)
#'
#' # Plot smoothed surface SO4 for all MIMA streams over all years with 0.6 span
#' plotTrend(park = "MIMA", site_type = "stream",
#'           parameter = "SO4_ueqL", legend_position = "right", span = 0.6)
#'
#' # Plot non-smoothed surface of multiple Sonde parameters for all MIMA streams over all years with 0.6 span.
#' params <- c("Temp_F", "SpCond_uScm", "DOsat_pct", "pH")
#' plotTrend(park = "MIMA", site_type = "stream",
#'           parameter = params, legend_position = "right", span = 0.6)
#'
#'}
#'
#' @return Returns a ggplot object of specified parameter trends.
#'
#' @export
#'
plotTrend <- function(park = "all", site = "all",
                      site_type = c("all", "lake", "stream"),
                      years = 2006:format(Sys.Date(), "%Y"),
                      months = 5:10, active = TRUE,
                      parameter = NA, include_censored = FALSE,
                      sample_depth = c("surface", "all"),
                      layers = c("points", "lines"),
                      palette = "viridis",
                      threshold = TRUE,
                      smooth = TRUE, numcol = 2,
                      span = 0.3, legend_position = 'none',
                      gridlines = "none", ...){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  stopifnot(class(include_censored) == "logical")
  sample_depth <- match.arg(sample_depth)
  stopifnot(class(smooth) == "logical")
  stopifnot(class(span) %in% "numeric")
  layers <- match.arg(layers, several.ok = TRUE)
  stopifnot(class(threshold) == "logical")
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))

  #-- Compile data for plotting --

  chem <- c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
            "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
            "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL", "TN_mgL",
            "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP_ugL")

  sonde <- c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
             "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg")

  other <- c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m")

  all_params <- c(chem, sonde, other)

  if(any(!parameter %in% all_params)){
    stop("At least one specified parameter is not an accepted value.")}

  par_chem <- parameter[parameter %in% chem]
  par_sonde <- parameter[parameter %in% sonde]
  par_sec <- parameter[parameter %in% "SDepth_m"]
  par_dis <- parameter[parameter %in% "Discharge_cfs"]
  par_pen <- parameter[parameter %in% "PenetrationRatio"]
  par_wl <- parameter[parameter %in% "WaterLevel_Feet"]
  par_wlm <- parameter[parameter %in% "WaterLevel_m"]

  wdat <-
    rbind(
    if(length(par_chem) > 0){
      force(getChemistry(park = park, site = site, site_type = site_type, include_censored = include_censored,
                   years = years, months = months, parameter = par_chem, ...)) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value, censored)
        } else {NULL},
    if(length(par_sonde) > 0){
      force(getSondeInSitu(park = park, site = site, site_type = site_type,
                     years = years, months = months, parameter = par_sonde, ...)) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_sec) > 0){
      force(getSecchi(park = park, site = site,
                years = years, months = months, observer_type = 'first')) |>
        #mutate(param = "SDepth_m", Value = SDepth_m) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_dis) > 0){
      force(getDischarge(park = park, site = site,
                   years = years, months = months)) |>
        mutate(Parameter = "Discharge_cfs", Value = Discharge_cfs) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_pen) > 0){
      force(getLightPen(park = park, site = site,
                  years = years, months = months)) |>
        mutate(Parameter = "PenetrationRatio", Value = PenetrationRatio) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_wl) > 0){
      force(getWaterLevel(park = park, site = site,
                    years = years, months = months)) |>
        mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
        mutate(censored = FALSE)
    } else {NULL},
    if(length(par_wlm) > 0){
      force(getWaterLevel(park = park, site = site,
                    years = years, months = months)) |>
        mutate(Parameter = "WaterLevel_m", Value = WaterLevel_m) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
        mutate(censored = FALSE)
    } else {NULL}
    )

  # Drop NAs (often from params that only have censored data and censored = F)
  wdat <- wdat[!is.na(wdat$Value),]

  wdat$param_label <- ifelse(grepl("_", wdat$Parameter),
                             paste0(gsub("_", " (", wdat$Parameter), ")"),
                             paste0(wdat$Parameter)
  )

  # join wdat with WQ thresholds, stored as a dataset in the package
  data("NETN_WQ_thresh")
  wdat2 <- tryCatch(left_join(wdat,
                     NETN_WQ_thresh[,c("SiteCode", "parameter", "UpperThreshold", "LowerThreshold")],
                     by = c("SiteCode", "Parameter" = "parameter")),
                    error = function(e){wdat})

  if(nrow(wdat2) == 0){stop("Combination of sites and parameters returned a data frame with no records.")}

  #-- Set up plotting features --
  ylabel <- ifelse(length(unique(wdat2$param_label)) == 1, unique(wdat2$param_label), "Value")
  wdat_cens <- wdat2 |> filter(censored == TRUE)

  wdat2$date2 <- as.Date(wdat2$EventDate, format = c("%Y-%m-%d"))

  year_len <- length(unique(wdat2$year))
  mon_len <- length(unique(wdat2$month))

  break_len <- if(year_len == 1){"1 month"
  } else if(year_len  %in% c(2, 3, 4) & mon_len <= 6){"2 months"
  } else if(year_len == 2 & mon_len > 6){"4 months"
    #} else if(year_len > 4 & mon_len <= 6){"6 months"
  } else if(year_len %in% c(4:19)){"1 year"
  } else if(year_len > 19){"2 years"
  } else {"6 months"}

  date_format <- ifelse(break_len %in% c("1 year", "2 years", "5 years"), "%Y",
                        ifelse(break_len %in% c("2 months", "4 months"), "%b/%Y",
                               "%b"))
  datebreaks <- seq(min(wdat2$date2, na.rm = T), max(wdat2$date2, na.rm = T) + 30, by = break_len)

  #-- Create plot --
  trendplot <-
    if(include_censored == TRUE){

    ggplot(wdat2, aes(x = date2, y = Value, group = SiteName,
                     color = SiteName, fill = SiteName, shape = censored)) +
      # layers
      {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span) } +
      {if(smooth == FALSE & any(layers %in% "lines")) geom_line()} +
      {if(any(layers %in% "points")) geom_point(aes(shape = censored, size = censored), alpha = 0.6)} +
      {if(any(layers %in% "points")) scale_shape_manual(values = c(19, 18), labels = c("Real", "Censored"))} +
      {if(any(layers %in% "points")) scale_size_manual(values = c(3,3.5), labels = c("Real", "Censored"))} +
      {if(threshold == TRUE){geom_hline(aes(yintercept = UpperThreshold, linetype = "Upper WQ Threshold"), lwd = 0.7)}} +
      {if(threshold == TRUE){geom_hline(aes(yintercept = LowerThreshold, linetype = "Lower WQ Threshold"), lwd = 0.7)}} +
      {if(threshold == TRUE){scale_linetype_manual(values = c("dotted", "dashed"))}} +
      # facets
      {if(length(unique(wdat$param_label))>1) facet_wrap(~param_label, scales = 'free_y', ncol = numcol)} +
      # themes
      theme_WQ() +
        theme(legend.position = legend_position,
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        {if(any(gridlines %in% c("grid_y", "both"))){
          theme(
            panel.grid.major.y = element_line(color = 'grey'))}} + #,
            #panel.grid.minor.y = element_line(color = 'grey'))}}+
        {if(any(gridlines %in% c("grid_x", "both"))){
          theme(
            panel.grid.major.x = element_line(color = 'grey'))}} + #,
            #panel.grid.minor.x = element_line(color = 'grey'))}}+
      # palettes
      {if(palette == "viridis") scale_color_viridis_d()} +
      {if(palette == "viridis") scale_fill_viridis_d()} +
      {if(!palette == "viridis") scale_color_brewer(palette = palette)} +
      {if(!palette == "viridis") scale_fill_brewer(palette = palette)} +
      #axis format
      scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
      scale_y_continuous(n.breaks = 8)+
      # labels
      #labs(x = "Year", y = ylab) +
      labs(x = NULL, y = ylabel) +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 1),
             shape = guide_legend(order = 1))
    } else {
      ggplot(wdat2, aes(x = date2, y = Value, group = SiteName,
                        color = SiteName, fill = SiteName)) +
        #layers
        {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span) } +
        {if(smooth == FALSE & any(layers %in% "lines")) geom_line()} +
        {if(any(layers %in% "points")) geom_point(alpha = 0.6)} +
        {if(threshold == TRUE){geom_hline(aes(yintercept = UpperThreshold, linetype = "Upper WQ Threshold"), lwd = 0.7)}} +
        {if(threshold == TRUE){geom_hline(aes(yintercept = LowerThreshold, linetype = "Lower WQ Threshold"), lwd = 0.7)}} +
        {if(threshold == TRUE){scale_linetype_manual(values = c("dashed", "solid"))}} +
        # facets
        {if(length(unique(wdat$param_label))>1) facet_wrap(~param_label, scales = 'free_y', ncol = numcol)} +
        # themes
        theme_WQ() +
        theme(legend.position = legend_position,
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        {if(any(gridlines %in% c("grid_y", "both"))){
          theme(
            panel.grid.major.y = element_line(color = 'grey'))}} + #,
        #panel.grid.minor.y = element_line(color = 'grey'))}}+
        {if(any(gridlines %in% c("grid_x", "both"))){
          theme(
            panel.grid.major.x = element_line(color = 'grey'))}} + #,
        #panel.grid.minor.x = element_line(color = 'grey'))}}+
        # color palettes
        {if(palette == "viridis") scale_color_viridis_d()} +
        {if(palette == "set1") scale_color_brewer(palette = "Set1")} +
        {if(palette == "dark2") scale_color_brewer(palette = "Dark2")} +
        {if(palette == "accent") scale_color_brewer(palette = "Accent")} +
        {if(palette == "viridis") scale_fill_viridis_d()} +
        {if(palette == "set1") scale_fill_brewer(palette = "Set1")} +
        {if(palette == "dark2") scale_fill_brewer(palette = "Dark2")} +
        {if(palette == "accent") scale_fill_brewer(palette = "Accent")} +
        #axis format
        scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
        scale_y_continuous(n.breaks = 8)+
        # labels
        #labs(x = "Year", y = ylab) +
        labs(x = NULL, y = ylabel) +
        guides(fill = guide_legend(order = 1),
               color = guide_legend(order = 1),
               shape = guide_legend(order = 1))

      }

 return(#suppressWarnings(
   trendplot)
  #)
}


