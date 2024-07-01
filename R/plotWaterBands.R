#' @include getChemistry.R
#' @include getDischarge.R
#' @include getLightPen.R
#' @include getSecchi.R
#' @include getSondeInSitu.R
#' @include getWaterLevel.R
#' @include theme_WQ.R
#'
#' @importFrom dplyr filter group_by left_join mutate select summarize
#' @importFrom tidyr pivot_longer pivot_wider
#' @import ggplot2
#'
#' @title plotWaterBands: compare current vs. historic values
#'
#' @description This function produces a plot that summarizes the range of historic data compared with current measurements.
#' Historic measurements are displayed as min-max values ever previously recorded (outermost band), upper and lower 95%
#' distribution and middle 50% distribution (inner quartiles) of values previously recorded (inner bands). The line
#' represents the median value.
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
#' @param year_current Year that will be plotted separately. Must be numeric and 4 digits.
#' @param years_historic Years to include in historic range calculations.
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param parameter Specify the parameter to return. Can only work with 1 parameter at a time. Current accepted values are:.
#' chemistry: c("ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
#' "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3_mgL", "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL",
#' "TN_mgL", "TotDissN_mgL", "TotDissP_ugL", "TP_ugL")
#' sonde: c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
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
#' @param threshold Logical. If TRUE (Default), will plot a dashed (upper) or dotted (lower) line if a water quality threshold exists for that
#'
#' parameter and site. If FALSE, no threshold line will be plotted.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param facet_scales Specify whether facet axes should be fixed (all the same) or "free_y", "free_x" or "free" (both).
#'
#' @param ... Additional arguments relevant to \code{getChemistry()} or \code{getSondeInSitu()}
#'
#' @return Returns a ggplot object of specified current vs historic values
#'
#' @examples
#' # Plot pH in Jordan Pond for 2023
#' plotWaterBands(site = "ACJORD", year_curr = 2023, years_historic = 2006:2022,
#' parameter = "pH", legend_position = 'right')
#'
#' # Plot TN in Jordan Pond for 2023, including censored
#' plotWaterBands(site = "ACJORD", year_curr = 2023, years_historic = 2006:2022,
# 'parameter = "TN_mgL", legend_position = 'right', include_censored = T)
#'
#' # Plot TN in Kroma Kill in SARA in 2023
#' plotWaterBands(site = "SARASA", year_curr = 2023, years_historic = 2006:2022, parameter = "TN",
#' legend_position = 'right')
#'
#' @export

plotWaterBands <- function(park = "all", site = "all", site_type = "all",
                           year_current = format(Sys.Date(), "%Y"),
                           years_historic = seq(2006, as.numeric(format(Sys.Date(), "%Y")) - 1, 1),
                           months = 5:10, active = TRUE,
                           parameter = NA, include_censored = FALSE,
                           sample_depth = c("surface", "all"),
                           threshold = TRUE, legend_position = 'none',
                           gridlines = "none", facet_scales = 'fixed',
                           #plotly = FALSE,
                           ...){
  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))

    if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
    site_type <- match.arg(site_type, c("all", "lake", "stream"))
    stopifnot(class(years_historic) %in% c("numeric", "integer"), years_historic >= 2006)
    stopifnot(class(year_current) %in% c("numeric", "integer"), year_current >= 2007)
    stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
    stopifnot(class(active) == "logical")
    stopifnot(class(include_censored) == "logical")
    sample_depth <- match.arg(sample_depth)
    stopifnot(class(threshold) == "logical")
    legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
    gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
    facet_scales <- match.arg(facet_scales, c("fixed", "free", "free_y", "free_x"))
#    stopifnot(class(plotly) == "logical")

    # if(!requireNamespace("plotly", quietly = TRUE) & plotly == TRUE){
    #   stop("Package 'plotly' needed if plotly = TRUE. Please install it.", call. = FALSE)}

    #-- Compile data for plotting --
    chem <- c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
              "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
              "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL", "TN_mgL",
              "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP_ugL")

    sonde <- c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
               "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg")

    other <- c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m")

    all_params <- c(chem, sonde, other)

    parameter <- match.arg(parameter, all_params) # only allows 1 parameter

    # if(any(!parameter %in% all_params)){
    #   stop("At least one specified parameter is not an accepted value.")}

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
          force(getChemistry(park = park, site = site, site_type = site_type,
                             include_censored = include_censored,
                             years = c(years_historic, year_current), months = months,
                             parameter = par_chem, ...)) |>
            select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter,
                   Value, censored)
        } else {NULL},
        if(length(par_sonde) > 0){
          force(getSondeInSitu(park = park, site = site, site_type = site_type,
                               years = c(years_historic, year_current), months = months,
                               parameter = par_sonde, ...)) |>
            select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter,
                   Value) |>
            mutate(censored = FALSE)
        } else {NULL},
        if(length(par_sec) > 0){
          force(getSecchi(park = park, site = site,
                          years = c(years_historic, year_current), months = months,
                          observer_type = 'first')) |>
            #mutate(param = "SDepth_m", Value = SDepth_m) |>
            select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy,
                   Parameter, Value) |>
            mutate(censored = FALSE)
        } else {NULL},
        if(length(par_dis) > 0){
          force(getDischarge(park = park, site = site,
                             years = c(years_historic, year_current), months = months)) |>
            mutate(Parameter = "Discharge_cfs", Value = Discharge_cfs) |>
            select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy,
                   Parameter, Value) |>
            mutate(censored = FALSE)
        } else {NULL},
        if(length(par_pen) > 0){
          force(getLightPen(park = park, site = site,
                            years = c(years_historic, year_current), months = months)) |>
            mutate(Parameter = "PenetrationRatio", Value = PenetrationRatio) |>
            select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy,
                   Parameter, Value) |>
            mutate(censored = FALSE)
        } else {NULL},
        if(length(par_wl) > 0){
          force(getWaterLevel(park = park, site = site,
                              years = c(years_historic, year_current), months = months)) |>
            mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
            select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy,
                   Parameter, Value) |>
            mutate(censored = FALSE)
        } else {NULL},
        if(length(par_wlm) > 0){
          force(getWaterLevel(park = park, site = site,
                              years = c(years_historic, year_current), months = months)) |>
            mutate(Parameter = "WaterLevel_m", Value = WaterLevel_m) |>
            select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy,
                   Parameter, Value) |>
            mutate(censored = FALSE)
        } else {NULL}
      )

      wdat <- wdat[!is.na(wdat$Value),]

      wdat$param_label <- ifelse(grepl("_", wdat$Parameter),
                                 paste0(gsub("_", " (", wdat$Parameter), ")"),
                                 paste0(wdat$Parameter)
      )
      wdat$Date <- as.Date(wdat$EventDate, format = "%Y-%m-%d")

      # Take the text after the _ if it exists:
      wdat$unit <- ifelse(grepl("_", wdat$Parameter),
                          gsub("(.*)_([^_]+)", "\\2", wdat$Parameter),
                          NA)

      # join wdat with WQ thresholds, stored as a dataset in the package
      data("NETN_WQ_thresh")
      wdat2 <- left_join(wdat,
                         NETN_WQ_thresh[,c("SiteCode", "parameter", "UpperThreshold", "LowerThreshold")],
                         by = c("SiteCode", "Parameter" = "parameter"))
      xaxis_breaks <- month.abb[months]
      wdat2$mon <- factor(wdat2$month,
                          levels = months,
                          labels = xaxis_breaks, ordered = T)

      if(nrow(wdat2) == 0){stop("Combination of sites and parameters returned a data frame with no records.")}

      # If threshold is >10% higher than max or 10% lower than min in full dataset, don't plot
      wdat_thresh <- wdat2 |> group_by(SiteCode, SiteName, UnitCode, Parameter) |>
        summarize(max_value = max(Value, na.rm = T),
                  min_value = min(Value, na.rm = T),
                  .groups = 'drop') |> ungroup()

      wdat3 <- left_join(wdat2, wdat_thresh, by = c("SiteCode", "SiteName", "UnitCode", "Parameter")) |>
        mutate(UpperThreshold_corr =
                   ifelse(!is.na(UpperThreshold) & UpperThreshold > 1.1*max_value, NA_real_, UpperThreshold),
               LowerThreshold_corr =
                   ifelse(!is.na(LowerThreshold) & LowerThreshold < min_value - 0.1*min_value, NA_real_, LowerThreshold))

      wdat_hist <- wdat3[wdat3$year %in% years_historic, ]
      wdat_curr <- wdat3[wdat3$year == year_current, ]

      if(nrow(wdat_curr) == 0){
              stop(paste0("There are no data available to plot for year_current: ", year_current, "."))}

      if(nrow(wdat_hist) == 0){
              stop(paste0("There are no historic data available to plot for years: ",
                          years_historic[1], ":", years_historic[2], "."))}

      # Calc min/max 95% stats
      wdat_sum <- wdat_hist |>
          group_by(SiteCode, SiteName, UnitCode, month, mon,
                   Parameter, param_label, unit, UpperThreshold, LowerThreshold,
                   UpperThreshold_corr, LowerThreshold_corr) |>
          summarize(num_samps = sum(!is.na(Value)),
                    median_val = median(Value, na.rm = TRUE),
                    min_val = min(Value, na.rm = TRUE),
                    max_val = max(Value, na.rm = TRUE),
                    lower_100 = ifelse(num_samps >= 3, min(Value, na.rm = T), NA),
                    upper_100 = ifelse(num_samps >= 3, max(Value, na.rm = T), NA),
                    lower_95 = ifelse(num_samps >= 3, quantile(Value, 0.025, na.rm = T), NA),
                    upper_95 = ifelse(num_samps >= 3, quantile(Value, 0.975, na.rm = T), NA),
                    lower_50 = ifelse(num_samps >= 3, quantile(Value, 0.25, na.rm = T), NA),
                    upper_50 = ifelse(num_samps >= 3, quantile(Value, 0.75, na.rm = T), NA),
                    .groups = "drop") |>
          filter(!is.na(lower_50))

      wdat_curr <- wdat_curr |>
          mutate(metric_type = ifelse(!is.na(UpperThreshold_corr) & Value > UpperThreshold_corr, "poor value",
                            ifelse(!is.na(LowerThreshold_corr) & Value < LowerThreshold_corr, "poor value",
                              "value")))

      wdat_med <- wdat_sum |> select(SiteCode:LowerThreshold_corr, median_val) |>
          mutate(metric_type = "median")

      wdat_hist2 <- wdat_sum |>
          select(SiteCode:LowerThreshold_corr, lower_100, upper_100, lower_95, upper_95, lower_50, upper_50) |>
          pivot_longer(cols = c(lower_100, upper_100, lower_95, upper_95, lower_50, upper_50),
                       names_to = "metric", values_to = "value") |>
          mutate(metric_type = ifelse(grepl("lower", metric), "lower", "upper"),
                 distrib = paste0("d", gsub("\\D", "", metric))) |>
          select(-metric) |>
          pivot_wider(values_from = value, names_from = metric_type) |>
          mutate(metric_type = distrib)

      wdat_hist2$metric_type <- factor(wdat_hist2$metric_type, levels = c("d100", "d95", "d50"))

      # xaxis_labels <- lapply(xaxis_breaks, function(x){as.character(lubridate::month(x, label = T))})

      thresh <- ifelse((!all(is.na(wdat_curr$UpperThreshold_corr)) & threshold == TRUE) |
                         (!all(is.na(wdat_curr$LowerThreshold_corr)) & threshold == TRUE),
                       TRUE, FALSE)

      plot_values <-
        if(thresh == TRUE){
        c("d100" = "#E4F0F8", "d95" = "#B8D8ED", "d50" = "#7FB9DD", "median" = "#1378b5",
         "value" = "black", "poor value" = "orange",
         "Upper WQ Threshold" = "black", "Lower WQ Threshold" = "black")
        } else {
          c("d100" = "#E4F0F8", "d95" = "#B8D8ED", "d50" = "#7FB9DD", "median" = "#1378b5",
            "value" = "black", "poor value" = "orange")
        }

      plot_breaks <-
        if(thresh == TRUE){
         c("d100", "d95", "d50", "median" ,
           "value", "poor value",
           "Upper WQ Threshold",  "Lower WQ Threshold")
        } else {
          c("d100", "d95", "d50", "median" ,
            "value", "poor value")
        }

      plot_labels <-
        if(thresh == TRUE){
        c("d100" = "Historic range", "d95" = "Hist. 95% range",
          "d50" = "Hist. 50% range", "median" = "Hist. median",
          "value" = "Current WQ value", "poor value" = "Curr. poor WQ value",
          "Upper WQ Threshold" = "Upper WQ Threshold",
          "Lower WQ Threshold" = "Lower WQ Threshold")
        } else {c("d100" = "Historic range", "d95" = "Hist. 95% range",
                  "d50" = "Hist. 50% range", "median" = "Hist. median",
                  "value" = "Current WQ value", "poor value" = "Curr. poor WQ value")}

      line_values <-
        if(thresh == TRUE){
            c(#"median" = "solid",
              "Upper WQ Threshold" = "solid",
              "Lower WQ Threshold" = "dashed")
        } else {NULL}#c("median" = "solid")}

      line_breaks <-
        if(thresh == TRUE){c(#"median",
                             "Upper WQ Threshold", "Lower WQ Threshold")} else {NULL}#c("median")}

      line_labels <-
        if(thresh == TRUE){
          c(#"median" = "Hist. median",
           "Upper WQ Threshold" = "Upper WQ Threshold",
           "Lower WQ Threshold" = "Lower WQ Threshold")
        } else {NULL}#c("median" = "Hist. median")}

      ylab <- unique(wdat_curr$param_label)

      wdat_hist2$mon <- factor(wdat_hist2$month,
                          levels = months,
                          labels = xaxis_breaks, ordered = T)

      wdat_curr$mon <- factor(wdat_curr$month,
                              levels = months,
                              labels = xaxis_breaks, ordered = T)

      wdat_med$mon <- factor(wdat_med$month,
                         levels = months,
                         labels = xaxis_breaks, ordered = T)


      facetsite <- ifelse(length(unique(wdat_curr$SiteCode)) > 1, TRUE, FALSE)

    monthly_plot <- #suppressWarnings(
              ggplot() + theme_WQ() +
                geom_ribbon(data = wdat_hist2,
                            aes(ymin = lower, ymax = upper, x = mon,
                                fill = metric_type,
                                #color = metric_type,
                                group = metric_type))+
                # geom_line(data = wdat_hist2, aes(y = lower, color = metric_type, group = metric_type)) +
                # geom_line(data = wdat_hist2, aes(y = upper, color = metric_type, group = metric_type)) +
                geom_line(data = wdat_med,
                          aes(y = median_val, x = mon, color = metric_type, group = metric_type), lwd = 0.7) +
                geom_point(data = wdat_curr, aes(y = Value, x = mon, color = metric_type, group = metric_type)) +
                scale_color_manual(values = plot_values,
                                   breaks = plot_breaks,
                                   labels = plot_labels,
                                   name = NULL) +
                scale_fill_manual(values = plot_values,
                                  breaks = plot_breaks,
                                  labels = plot_labels,
                                  name = NULL) +
                #guides(color = "none") +
                scale_linetype_manual(values = line_values,
                                      breaks = line_breaks,
                                      labels = line_labels,
                                      name = NULL) +
                # Facets
                {if(facetsite == TRUE){facet_wrap(~SiteName, scales = facet_scales)}} +
                # Upper and lower points
                {if(thresh == TRUE){geom_hline(data = wdat_curr,
                                               aes(yintercept = UpperThreshold_corr,
                                                   group = "Upper WQ Threshold",
                                                  linetype = "Upper WQ Threshold"), lwd = 0.7)}} +
                {if(thresh == TRUE){geom_hline(data = wdat_curr,
                                               aes(yintercept = LowerThreshold_corr,
                                               group = "Lower WQ Threshold",
                                               linetype = "Lower WQ Threshold"), lwd = 0.7)}} +
                # Labels/Themes/axes
                scale_x_discrete(breaks = xaxis_breaks, drop = F, expand = c(0.04,0.04)) +
                scale_y_continuous(n.breaks = 8) +
                labs(y = ylab, x = NULL, title = NULL) +
                theme(axis.title.y = element_text(size = 10),
                      panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
                      axis.line.x = element_line(color = "#696969", linewidth = 0.4),
                      axis.line.y = element_line(color = "#696969", linewidth = 0.4),
                      axis.ticks = element_line(color = "#696969", linewidth = 0.4),
                      legend.key = element_blank(),
                      legend.spacing.y = unit(-0.2, "cm"),
                      legend.position = legend_position) +
              {if(any(gridlines %in% c("grid_y", "both"))){
                theme(
                      panel.grid.major.y = element_line(color = 'grey'),
                      panel.grid.minor.y = element_line(color = 'grey'))}} +
              {if(any(gridlines %in% c("grid_x", "both"))){
                theme(
                      panel.grid.major.x = element_line(color = 'grey'),
                      panel.grid.minor.x = element_line(color = 'grey'))}} +
                guides(color = guide_legend(order = 2),
                       fill = guide_legend(order = 1), linetype = guide_legend(order = 3))
            #)

        return(monthly_plot)
        }
