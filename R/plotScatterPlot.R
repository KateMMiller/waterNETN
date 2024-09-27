#' @include getChemistry.R
#' @include getDischarge.R
#' @include getLightPen.R
#' @include getSecchi.R
#' @include getSondeInSitu.R
#' @include getWaterLevel.R
#' @include theme_WQ.R
#'
#' @title plotScatterPlot: Plot scatterplot of 2 variables
#'
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#' @import ggplot2
#'
#' @description This function produces points or loess smoothed lines of 2 variables, filtered on park, site, year,
#' month, and 2 parameters. Works with lab chemistry, Sonde in situ, discharge, secchi depth, water level, and
#' light penetration ratio. If multiple sites are specified, they will be plotted on the same figure, unless facet_site = T.
#' Note that if you specify a site and parameter combination that doesn't exist (e.g., a stream site and a parameter
#' only collected in lakes), the function will return an error message instead of an empty plot. Censored values are
#' not permitted in this function.
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
#' @param event_type Select the event type. Options available are below. Can only choose one option.
#' \describe{
#' \item{"all"}{All possible sampling events.}
#' \item{"VS"}{Default. NETN Vital Signs monitoring events, which includes Projects named 'NETN_LS' and 'NETN+ACID'.}
#' \item{"acid"}{Acidification monitoring events in Acadia.}
#' \item{"misc"}{Miscellaneous sampling events.}
#' }
#'
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns
#' all sites that have been monitored.
#'
#' @param parameters Specify the two parameters to plot. The first parameter will be treated as the response (y).
#' The second parameter will be treated as the explanatory (x) variable. Note that censored values can not be included
#' in the scatterplot. Current accepted values are:.
#' chemistry: c("ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
#' "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3_mgL", "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL",
#' "TN_mgL", "TotDissN_mgL", "TotDissP_ugL", "TP_ugL")
#' sonde: c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#' "Turbidity_FNU", "ChlA_EXO_RFU", "ChlA_EXO_ugL", "BP_mmHg").
#' other: c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m").
#'
#' @param sample_depth Filter on sample depth. If "all" (Default), returns all sample depths. If "surface",
#' only returns the median value of samples collected <= 2m from the surface. SampleDepth_m is also the median
#' sample depth of samples collected within 2m of the surface. Note that for the Penetration Ratio parameter,
#' all sample depths are plotted. Plotting all depths may return a funky plot for other parameters.
#'
#' @param layers Options are "points" and "smooth". By default, both will plot. If "smooth" specified, will plot a loess
#' smoothed line. See span for more details. If only points specified, will return a scatterplot.
#'
#' @param palette Theme to plot points and lines. Options include 'viridis' (Default- ranges of blue,
#' green and yellow), magma (yellow, red, purple), plasma (brighter version of magma), turbo (rainbow),
#' or specify a vector of colors manually. If fewer colors than parameters are specified, they will be
#' ramped to generate enough colors.
#'
#' @param span Numeric. Determines how smoothed the line will be for smooth = TRUE. Default is 0.3. Higher spans (up to 1)
#' cause more smoothing. Span can range from 0 to 1.
#'
#' @param facet_site Logical. If TRUE (default), will facet on site if multiple sites specified. If FALSE, will plot all sites
#' on the same figure. Only enabled when multiple sites specified.
#'
#' @param facet_scales Specify whether facet axes should be fixed (all the same; default) or "free_y", "free_x" or "free" (both).
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when facet_site = T. Default is 2.
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param ... Additional arguments relevant to \code{getChemistry()} or \code{getSondeInSitu()}
#'
#' @examples
#' \dontrun{
#'
#' # Plot Temp vs DO for ROVA all years on same figure
#' plotScatterPlot(park = "ROVA", parameters = c("DO_mgL", "Temp_C"),
#'   palette = 'viridis', facet_site = F, legend_position = "bottom")
#'
#' # Plot Secchi depth vs. surface DOC in Eagle Lake, Jordon Pond, Echo Lake, and Witch Hole Pond
#' plotScatterPlot(site = c("ACEAGL", "ACJORD", "ACWHOL", "ACECHO"), parameters = c("SDepth_m", "DOC_mgL"),
#'   span = 0.9, facet_site = F, legend_position = 'bottom')
#'
#' # Plot Secchi depth vs. surface TN in Eagle Lake, Jordon Pond, and Witch Hole Pond
#' plotScatterPlot(site = c("ACEAGL", "ACJORD", "ACWHOL"), parameters = c("SDepth_m", "TN_mgL"),
#'   span = 0.9, facet_site = F, legend_position = 'bottom')
#'
#' # Plot smoothed discharge vs. specific conductance for the Pogue Brook using span of 0.9.
#' plotScatterPlot(site = "MABISA", parameters = c("SpCond_uScm", "Discharge_cfs"), span = 0.9, palette = c("forestgreen"))
#'
#' # Plot smoothed discharge vs. specific conductance for SARA streams using span of 0.9.
#' plotScatterPlot(park = "SARA", parameters = c("SpCond_uScm", "Discharge_cfs"), span = 0.9, facet_site = F,
#' legend_position = 'bottom')
#'
#' # Same as above, but faceted by site.
#' plotScatterPlot(park = "SARA", parameters = c("SpCond_uScm", "Discharge_cfs"), span = 0.9, facet_site = T)
#'
#' # Plot TN vs discharge in SARA streams
#' plotScatterPlot(park = "SARA", parameters = c("TN_mgL", "Discharge_cfs"), span = 0.9, facet_site = F)
#'
#'}
#'
#' @return Returns a ggplot scatterplot object
#'
#' @export
#'
plotScatterPlot <- function(park = "all", site = "all",
                            site_type = "all",
                            event_type = "VS",
                            years = 2006:format(Sys.Date(), "%Y"),
                            months = 5:10, active = TRUE,
                            parameters = NA,
                            sample_depth = "surface",
                            layers = c("points", "smooth"),
                            palette = "viridis",
                            facet_site = TRUE,
                            facet_scales = "free_y",
                            numcol = 2,
                            span = 0.3,
                            legend_position = 'none',
                            gridlines = "none", ...){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){
    park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  event_type <- match.arg(event_type, c("all", "VS", "acid", "misc"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  sample_depth <- match.arg(sample_depth)
  stopifnot(class(facet_site) == "logical")
  match.arg(layers, c("points", "smooth"), several.ok = TRUE)
  match.arg(facet_scales, c("fixed", "free_y", "free_x", "free"))
  stopifnot(class(span) %in% "numeric")
  layers <- match.arg(layers, several.ok = TRUE)
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  stopifnot(length(parameters) == 2)

  #-- Compile data for plotting --
  chem <- c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
            "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
            "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL", "TN_mgL",
            "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP_ugL")

  sonde <- c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
             "Turbidity_FNU", "ChlA_EXO_RFU", "ChlA_EXO_ugL", "BP_mmHg")

  other <- c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m")

  all_params <- c(chem, sonde, other)

  if(any(!parameters %in% all_params)){
    stop("At least one specified parameter is not an accepted value.")}

  # Compile data for x and y param separately, then join
  par_chem1 <- parameters[1][parameters[1] %in% chem]
  par_sonde1 <- parameters[1][parameters[1] %in% sonde]
  par_sec1 <- parameters[1][parameters[1] %in% "SDepth_m"]
  par_dis1 <- parameters[1][parameters[1] %in% "Discharge_cfs"]
  par_pen1 <- parameters[1][parameters[1] %in% "PenetrationRatio"]
  par_wl1 <- parameters[1][parameters[1] %in% "WaterLevel_Feet"]
  par_wlm1 <- parameters[1][parameters[1] %in% "WaterLevel_m"]

  par_chem2 <- parameters[2][parameters[2] %in% chem]
  par_sonde2 <- parameters[2][parameters[2] %in% sonde]
  par_sec2 <- parameters[2][parameters[2] %in% "SDepth_m"]
  par_dis2 <- parameters[2][parameters[2] %in% "Discharge_cfs"]
  par_pen2 <- parameters[2][parameters[2] %in% "PenetrationRatio"]
  par_wl2 <- parameters[2][parameters[2] %in% "WaterLevel_Feet"]
  par_wlm2 <- parameters[2][parameters[2] %in% "WaterLevel_m"]

  wdat_p1 <-
    rbind(
    if(length(par_chem1) > 0){
      force(getChemistry(park = park, site = site, site_type = site_type, include_censored = FALSE,
                   years = years, months = months, parameter = par_chem1, event_type = event_type, ...)) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
        } else {NULL},
    if(length(par_sonde1) > 0){
      force(getSondeInSitu(park = park, site = site, site_type = site_type,
                     years = years, months = months, parameter = par_sonde1, event_type = event_type, ...)) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
        } else {NULL},
    if(length(par_sec1) > 0){
      force(getSecchi(park = park, site = site, event_type = event_type,
                years = years, months = months, observer_type = 'first')) |>
        #mutate(param = "SDepth_m", Value = SDepth_m) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
        } else {NULL},
    if(length(par_dis1) > 0){
      force(getDischarge(park = park, site = site, event_type = event_type,
                   years = years, months = months)) |>
        mutate(Parameter = "Discharge_cfs", Value = Discharge_cfs) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
        } else {NULL},
    if(length(par_pen1) > 0){
      force(getLightPen(park = park, site = site, event_type = event_type,
                  years = years, months = months)) |>
        mutate(Parameter = "PenetrationRatio", Value = PenetrationRatio) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
        } else {NULL},
    if(length(par_wl1) > 0){
      force(getWaterLevel(park = park, site = site,
                    years = years, months = months)) |>
        mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
    } else {NULL},
    if(length(par_wlm1) > 0){
      force(getWaterLevel(park = park, site = site,
                    years = years, months = months)) |>
        mutate(Parameter = "WaterLevel_m", Value = WaterLevel_m) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
    } else {NULL}
    )

  wdat_p2 <-
    rbind(
      if(length(par_chem2) > 0){
        force(getChemistry(park = park, site = site, site_type = site_type, include_censored = FALSE,
                           years = years, months = months, parameter = par_chem2, event_type = event_type, ...)) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_sonde2) > 0){
        force(getSondeInSitu(park = park, site = site, site_type = site_type,
                             years = years, months = months, parameter = par_sonde2, event_type = event_type, ...)) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_sec2) > 0){
        force(getSecchi(park = park, site = site, event_type = event_type,
                        years = years, months = months, observer_type = 'first')) |>
          #mutate(param = "SDepth_m", Value = SDepth_m) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_dis2) > 0){
        force(getDischarge(park = park, site = site, event_type = event_type,
                           years = years, months = months)) |>
          mutate(Parameter = "Discharge_cfs", Value = Discharge_cfs) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_pen2) > 0){
        force(getLightPen(park = park, site = site, event_type = event_type,
                          years = years, months = months)) |>
          mutate(Parameter = "PenetrationRatio", Value = PenetrationRatio) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wl2) > 0){
        force(getWaterLevel(park = park, site = site,
                            years = years, months = months)) |>
          mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wlm2) > 0){
        force(getWaterLevel(park = park, site = site,
                            years = years, months = months)) |>
          mutate(Parameter = "WaterLevel_m", Value = WaterLevel_m) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL}
    )

  wdat <- full_join(wdat_p1, wdat_p2, by = c("SiteCode", "SiteName", "UnitCode", "EventDate", "year", "month", "doy"),
                    suffix = c("_y", "_x"))

  # Drop NAs for params not sampled every month
  wdat <- wdat[!with(wdat, is.na(Value_y) | is.na(Value_x)),]

  y_lab <- ifelse(grepl("_", parameters[1]), paste0(gsub("_", " (", parameters[1]), ")"), paste0(parameters[1]))
  x_lab <- ifelse(grepl("_", parameters[2]), paste0(gsub("_", " (", parameters[2]), ")"), paste0(parameters[2]))

  #-- Set up plotting features --
  vir_pal = ifelse(palette %in%
                     c("viridis", "magma", "plasma", "turbo", "mako", "rocket", "cividis", "inferno"),
                   "viridis", "colbrew")

  pal <-
    pal <-
    if(any(vir_pal == "colbrew")){
      colorRampPalette(palette)(length(unique(wdat$SiteCode)))
      #rep(colorRampPalette(palette)(length(unique(parameter))), times = length(parameter) * length(unique(wdat2$SiteCode)))
    }

  facetsite <- ifelse(length(unique(wdat$SiteCode)) > 1 & facet_site == TRUE, TRUE, FALSE)

  #-- Create plot --
  scatplot <-
      ggplot(wdat, aes(x = Value_x, y = Value_y, group = SiteName, color = SiteName, fill = SiteName)) +
      # layers
      {if(any(layers %in% "smooth")) geom_smooth(aes(text = paste0("Site: ", SiteName, "<br>")),
                                      method = 'loess', formula = 'y ~ x', se = F, span = span) } +
      geom_point(aes(text = paste0("Site: ", SiteName, "<br>",
                                     "X Variable: ", x_lab, "<br>",
                                     "X: ", round(Value_x, 1), "<br>",
                                     "Y Variable: ", y_lab, "<br>",
                                     "Y: ", round(Value_y, 1), "<br>")),
                 alpha = 0.4, size = 2.5) +
      # facets
      {if(facetsite == TRUE) facet_wrap(~SiteName, scales = 'free_y', ncol = numcol)} +
      # themes
      theme_WQ() +
      theme(legend.position = legend_position,
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      {if(any(gridlines %in% c("grid_y", "both"))){
          theme(panel.grid.major.y = element_line(color = 'grey'))}} + #,
                #panel.grid.minor.y = element_line(color = 'grey'))}}+
      {if(any(gridlines %in% c("grid_x", "both"))){
          theme(panel.grid.major.x = element_line(color = 'grey'))}} + #,
                #panel.grid.minor.x = element_line(color = 'grey'))}}+
      # palettes
      {if(any(vir_pal == "viridis")) scale_color_viridis_d(option = palette)} +
      {if(any(vir_pal == "viridis")) scale_fill_viridis_d(option = palette)} +
      {if(any(vir_pal == "colbrew")) scale_fill_manual(values = pal)} +
      {if(any(vir_pal == "colbrew")) scale_color_manual(values = pal)} +
      #axis format
      scale_y_continuous(n.breaks = 8) +
      # labels
      #labs(x = "Year", y = ylab) +
      labs(x = x_lab, y = y_lab) +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 1),
             shape = guide_legend(order = 1))


 #return(#suppressWarnings(
   scatplot
#   )
  #)
}


