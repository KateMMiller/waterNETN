#' @title plotTrend: Plots smoothed trend
#'
#' @importFrom dplyr mutate select
#' @import ggplot2
#'
#' @description This function produces a smoothed trend plot filtered on park, site, year, month, and parameter.
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
#' sonde: c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#' "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg").
#' other: c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevelFeet").
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
#' @param color_theme Theme to plot points and lines. Options currently are 'viridis' (Default- ranges of blue, green and yellow),
#' or from RColorBrewer: 'set1', 'dark2', or 'accent' (see https://ggplot2-book.org/scales-colour).
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
#' @param ... Additional arguments relevant to \code{getChemistry()} or \code{getSondeInSitu()}
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
plotTrend <- function(park = "all", site = "all",
                      site_type = c("all", "lake", "stream"),
                      years = 2006:format(Sys.Date(), "%Y"),
                      months = 5:10, active = TRUE,
                      parameter = NA, include_censored = FALSE,
                      sample_depth = c("surface", "all"),
                      layers = c("points", "lines"),
                      color_theme = "viridis",
                      threshold = TRUE,
                      smooth = TRUE,
                      span = 0.3, legend_position = 'none', ...){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
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
  color_theme <- match.arg(color_theme, c("viridis", "set1", "dark2", "accent"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))

  chem <- c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
            "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
            "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL", "TN_mgL",
            "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP_ugL")

  sonde <- c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
             "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg")

  other <- c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevelFeet")

  all_params <- c(chem, sonde, other)

  if(any(!parameter %in% all_params)){
    stop("At least one specified parameter is not an accepted value.")}

  par_chem <- parameter[parameter %in% chem]
  par_sonde <- parameter[parameter %in% sonde]
  par_sec <- parameter[parameter %in% "SDepth_m"]
  par_dis <- parameter[parameter %in% "Discharge_cfs"]
  par_pen <- parameter[parameter %in% "PenetrationRatio"]
  par_wl <- parameter[parameter %in% "WaterLevelFeet"]

  wdat <-
    rbind(
    if(length(par_chem) > 0){
      getChemistry(park = park, site = site, site_type = site_type, include_censored = include_censored,
                   years = years, months = months, parameter = par_chem, ...) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, param, value, censored)
        } else {NULL},
    if(length(par_sonde) > 0){
      getSondeInSitu(park = park, site = site, site_type = site_type,
                     years = years, months = months, parameter = par_sonde, ...) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, param, value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_sec) > 0){
      getSecchi(park = park, site = site,
                years = years, months = months, observer_type = 'first') |>
        mutate(param = "SDepth_m", value = SDepth_m) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, param, value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_dis) > 0){
      getDischarge(park = park, site = site,
                   years = years, months = months) |>
        mutate(param = "Discharge_cfs", value = Discharge_cfs) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, param, value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_pen) > 0){
      getLightPen(park = park, site = site,
                  years = years, months = months) |>
        mutate(param = "PenetrationRatio", value = PenetrationRatio) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, param, value) |>
        mutate(censored = FALSE)
        } else {NULL},
    if(length(par_wl) > 0){
      getWaterLevel(park = park, site = site,
                    years = years, months = months) |>
        mutate(param = "WaterLevelFeet", value = WaterLevelFeet) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, param, value) |>
        mutate(censored = FALSE)
    } else {NULL}
    )

  # Drop NAs (often from params that only have censored data and censored = F)
  wdat <- wdat[!is.na(wdat$value),]

  wdat$param_label <- ifelse(grepl("_", wdat$param),
                             paste0(gsub("_", " (", wdat$param), ")"),
                             paste0(wdat$param)
  )

  # join wdat with WQ thresholds, stored as a dataset in the package
  data("NETN_WQ_thresh")
  wdat2 <- left_join(wdat,
                     NETN_WQ_thresh[,c("SiteCode", "parameter", "UpperThreshold", "LowerThreshold")],
                     by = c("SiteCode", "param" = "parameter"))

  if(nrow(wdat2) == 0){stop("Combination of sites and parameters returned a data frame with no records.")}

  ylab <- ifelse(length(unique(wdat$param_label)) == 1, unique(wdat$param_label), "value")
  wdat_cens <- wdat2 |> filter(censored == TRUE)

  trendplot <-
    ggplot(wdat2, aes(x = EventDate, y = value, group = SiteName,
                     color = SiteName, fill = SiteName, shape = censored)) +
      {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span) } +
      {if(smooth == FALSE & any(layers %in% "lines")) geom_line()} +
      {if(any(layers %in% "points")) geom_point(aes(shape = censored, size = censored), alpha = 0.6)} +
      {if(any(layers %in% "points")) scale_shape_manual(values = c(19, 18), labels = c("Real", "Censored"))} +
      {if(any(layers %in% "points")) scale_size_manual(values = c(3,3.5), labels = c("Real", "Censored"))} +
    # {if(include_censored == TRUE & any(layers %in% "points")){
      #   geom_point(data = wdat2 |> filter(censored == TRUE),
      #              aes(x = EventDate, y = value), shape = 25, size = 2, show.legend = FALSE,
      #              alpha = 0.4)}} +
      {if(length(unique(wdat$param_label))>1) facet_wrap(~param_label, scales = 'free')} +
      {if(threshold == TRUE){geom_hline(aes(yintercept = UpperThreshold), linetype = "dashed")}} +
      {if(threshold == TRUE){geom_hline(aes(yintercept = LowerThreshold), linetype = 'dotted')}} +
      theme_WQ() + theme(legend.position = legend_position, legend.title = element_blank()) +
      {if(color_theme == "viridis") scale_color_viridis_d()} +
      {if(color_theme == "set1") scale_color_brewer(palette = "Set1")} +
      {if(color_theme == "dark2") scale_color_brewer(palette = "Dark2")} +
      {if(color_theme == "accent") scale_color_brewer(palette = "Accent")} +
      {if(color_theme == "viridis") scale_fill_viridis_d()} +
      {if(color_theme == "set1") scale_fill_brewer(palette = "Set1")} +
      {if(color_theme == "dark2") scale_fill_brewer(palette = "Dark2")} +
      {if(color_theme == "accent") scale_fill_brewer(palette = "Accent")} +
      labs(x = "Year", y = ylab)

 return(suppressWarnings(trendplot))
}

