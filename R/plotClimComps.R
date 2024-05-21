#' @include getSites.R
#' @include theme_WQ.R
#' @include sumClimMonthly.R
#' @include sumClimAvgs.R
#'
#' @title plotClimComps: Plot climate comparisons
#'
#' @importFrom dplyr arrange filter left_join
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @description This function compares decadal (1980 - 1989, ..., 2010 - 2019) average
#' monthly climate variables with user-specified years to provide an idea of how extreme
#' or normal a given month in a year is. This function only works with Daymet data,
#' so is not immediately available for the current date.
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
#' @param site Filter on 6-letter SiteCode (e.g., "ACABIN", "MORRSA", etc.). Easiest way to pick a site.
#' Defaults to "all". If multiple sites sites selected, will facet on site.
#'
#' @param site_type Combine all site types, lakes or streams. Not needed if specifying particular sites.
#' \describe{
#' \item{"all"}{Default. Includes all site types, unless site or site_name select specific site types.}
#' \item{"lake"}{Include only lakes.}
#' \item{"stream"}{Include streams only.}
#' }
#'
#' @param years Numeric. Years to plot separately. Accepted values start at 2006.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites.
#' If FALSE, returns all sites that have been monitored.
#'
#' @param parameter Specify the monthly averaged parameter to plot. Acceptable values are
#' \describe{
#' \item{"temp_mean"}{Plot mean temperature comparisons (in C).}
#' \item{"temp_max"}{Plot max temperature comparisons (in C).}
#' \item{"temp_min"}{Plot min temperature comparisons (in C).}
#' \item{"precip"}{Plot precipitation comparisons (in mm).}
#' }
#'
#' @param layers Options are "points", "lines", or both for annual averages. By default, only lines will plot.
#'
#' @param color_theme Diverging color palette for plots. Options currently are 'spectral' (default), 'ryb' (red - yellow - blue),
#' 'rb' (red - blue), 'pr'  (purple - red), and 'viridis' (yellow - green - blue). (see https://ggplot2-book.org/scales-colour
#' for more info).
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param ... Additional arguments relevant to \code{sumClimAvgs()} or \code{sumClimMonthly()}
#'
#' @examples
#' \dontrun{
#'
#' # Plot mean monthly temp for the Pogue in MABI for 2019:2023 and all months with red-blue color palette
#' plotClimComps(site = "MABIPA", years = 2019:2023, parameter = "temp_mean", color_theme = "rb")
#'
#' # Same as above, but with points too
#' plotClimComps(site = "MABIPA", years = 2019:2023, parameter = "temp_mean", color_theme = "rb", layers = c('points', 'lines'))
#'
#' # Plot max monthly temp for Eagle Lake and Jordan Pond for 2019:2023 and all months
#' plotClimComps(site = c("ACEAGL", "ACJORD"), years = 2019:2023, parameter = "temp_max", color_theme = "rb", layers = 'lines')
#'
#' # Plot total monthly precip for Jordan Pond for past 10 years
#' plotClimComps(site = "ACJORD", years = 2013:2023, parameter = 'precip')
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimComps <- function(park = "all", site = "all",
                          site_type = c("all", "lake", "stream"),
                          years = 2006:format(Sys.Date(), "%Y"),
                          months = 1:12, active = TRUE,
                          layers = "lines",
                          parameter = NA,
                          color_theme = "rb",
                          legend_position = 'right', ...){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  parameter <- match.arg(parameter, c("temp_mean", "temp_max", "temp_min", "precip"))
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  color_theme <- match.arg(color_theme, c("spectral", "ryb", "rb", 'pr', "viridis"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))

  #++++++ CHANGE PALETTE SO MORE FLEXIBLE CONDITIONAL ON VIRIDIS OR RCOLORBREWER. ++++++++++

  #-- Compile data for plotting --
  # Clim data as annual monthly averages
  clim_dat <- sumClimMonthly(park = park, site = site, site_type = site_type,
                             active = active, years = years,
                             months = months, data_type = 'daymet')

  if(nrow(clim_dat) == 0){stop("Specified arguments returned a data frame with 0 records.")}

  clim_dat_long <- clim_dat |>
    pivot_longer(dm_ppt_mm:ncol(clim_dat), names_to = "param", values_to = "value") |>
    arrange(SiteCode, month, param)

  clim_dat_long$date <- as.Date(paste0(clim_dat_long$year, "-", clim_dat_long$month, "-", 15), format = "%Y-%m-%d")

  # Clim data in decadal and 30-year norms
  avg_dat <- sumClimAvgs(park = park, site = site, site_type = site_type, active = active, ...)
  avg_dat_long <- avg_dat |> pivot_longer(ppt_mm_d1980:ncol(avg_dat), names_to = "param", values_to = "value") |>
    arrange(SiteCode, param, month)

  #-- Set up plotting features --
  # annual params
  param_labels_annual <-
    data.frame(param = c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C", "dm_tmean_C", "dm_BAL", "dm_SPEI01", "dm_SPEI03"),
               param_label = c("Total Precip. (mm)", "Max. Temp. (C)", "Min. Temp. (C)",
                               "Avg. Temp. (C)", "Water Balance", "SPEI - 1 month", "SPEI - 3 month"))

  clim_dat_long2 <- left_join(clim_dat_long, param_labels_annual, by = 'param')
  clim_dat_long2$mon <- factor(clim_dat_long2$month, levels = unique(clim_dat_long2$month),
                               labels = unique(month.abb[clim_dat_long2$month]), ordered = T)
  clim_dat_long2$param_label2 <- paste0(clim_dat_long2$param_label, " (", clim_dat_long2$year, ")")

  # decadal params
  norms <- data.frame(param = c("ppt_mm_d1980", "ppt_mm_d1990", "ppt_mm_d2000", "ppt_mm_d2010",
                                "tmax_C_d1980", "tmax_C_d1990", "tmax_C_d2000", "tmax_C_d2010",
                                "tmin_C_d1980", "tmin_C_d1990", "tmin_C_d2000", "tmin_C_d2010",
                                "tmean_C_d1980", "tmean_C_d1990", "tmean_C_d2000", "tmean_C_d2010",
                                "ppt_mm_30yr_80", "tmax_C_30yr_80", "tmin_C_30yr_80", "tmean_C_30yr_80",
                                "ppt_mm_30yr_90",  "tmax_C_30yr_90",  "tmin_C_30yr_90", "tmean_C_30yr_90"),
                      # param_label = c("Total Precip. 1980-1989", "Total Precip. 1990-1999",
                      #                 "Total Precip. 2000-2009", "Total Precip. 2010-2019",
                      #                 "Max. Temp. 1980-1989", "Max. Temp. 1990-1999",
                      #                 "Max. Temp. 2000-2009", "Max. Temp. 2010-2019",
                      #                 "Min. Temp. 1980-1989", "Min. Temp. 1990-1999",
                      #                 "Min. Temp. 2000-2009", "Min. Temp. 2010-2019",
                      #                 "Mean Temp. 1980-1989", "Mean Temp. 1990-1999",
                      #                 "Mean Temp. 2000-2009", "Mean Temp. 2010-2019",
                      #                 "Total Precip. 1980-2009", "Max. Temp. 1980-2009",
                      #                 "Min. Temp. 1980-2009", "Mean Temp. 1980-2009",
                      #                 "Total Precip. 1990-2019",  "Max. Temp. 1990-2019",
                      #                 "Min. Temp. 1990-2019", "Mean Temp. 1990-2019"),
                      param_label2 = c(rep(c("decade 1980-1989", "decade 1990-1999",
                                             "decade 2000-2009", "decade 2010-2019"), 4),
                                      "Total Precip. 1980-2009", "Max. Temp. 1980-2009",
                                      "Min. Temp. 1980-2009", "Mean Temp. 1980-2009",
                                      "Total Precip. 1990-2019",  "Max. Temp. 1990-2019",
                                      "Min. Temp. 1990-2019", "Mean Temp. 1990-2019"))

  avg_dat_long2 <- left_join(avg_dat_long, norms, by = "param")

  avg_dat_long2$mon <- factor(avg_dat_long2$month,
                              levels = unique(avg_dat_long2$month),
                              labels = unique(month.abb[avg_dat_long2$month]), ordered = T)

  # set up filter and labelling on parameter
  if(parameter == "temp_mean"){
    ann_filt = "dm_tmean_C"
    dec_filt = c("tmean_C_d1980", "tmean_C_d1990", "tmean_C_d2000", "tmean_C_d2010")
    y_label = "Avg. Monthly Temp. (C)"
  } else if(parameter == "temp_min"){
    ann_filt = "dm_tmin_C"
    dec_filt = c("tmin_C_d1980", "tmin_C_d1990", "tmin_C_d2000", "tmin_C_d2010")
    y_label = "Avg. Minimum Monthly Temp. (C)"
  } else if(parameter == "temp_max"){
    ann_filt = "dm_tmax_C"
    dec_filt = c("tmax_C_d1980", "tmax_C_d1990", "tmax_C_d2000", "tmax_C_d2010")
    y_label = "Avg. Maximum Monthly Temp. (C)"
  } else if(parameter == "precip"){
    ann_filt = "dm_ppt_mm"
    dec_filt = c("ppt_mm_d1980", "ppt_mm_d1990", "ppt_mm_d2000", "ppt_mm_d2010")
    y_label = "Total Monthly Precip. (mm)"
  }

clim_plot <-
  ggplot() + theme_WQ() +
  # layers for annual data
  {if(any(layers %in% "lines"))
      geom_line(data = clim_dat_long |> filter(param %in% ann_filt),
                aes(x = mon, y = value, group = as.integer(year), color = as.integer(year)))} +
  {if(any(layers %in% "points"))
      geom_point(data = clim_dat_long |> filter(param %in% ann_filt),
                 aes(x = mon, y = value, group = as.integer(year), color = as.integer(year), fill = as.integer(year)))} +
  # color palettes for annual data
  {if(color_theme == 'spectral') scale_fill_distiller(palette = "Spectral", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'spectral') scale_color_distiller(palette = "Spectral", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'ryb') scale_fill_distiller(palette = "RdYlBu", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'ryb') scale_color_distiller(palette = "RdYlBu", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'rb') scale_fill_distiller(palette = "RdBu", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'rb') scale_color_distiller(palette = "RdBu", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'pr') scale_fill_distiller(palette = "PuRd", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'pr') scale_color_distiller(palette = "PuRd", direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'viridis') scale_fill_viridis_c(direction = color_dir, name = 'Annual Averages')} +
  {if(color_theme == 'viridis') scale_color_viridis_c(direction = color_dir, name = 'Annual Averages')} +
  # facets for multiple years
  {if(length(unique(clim_dat_long$SiteCode)) > 1) facet_wrap(~SiteName)} +
  # layers for decadal data
  geom_line(data = avg_dat_long2 |> filter(param %in% dec_filt),
            aes(x = mon, y = value, group = param_label2, linetype = param_label2), linewidth = 0.8) +
  # line type for decadal data
  scale_linetype_manual(values = c("dotted", "dashed", "longdash", "solid"), name = "Decadal Averages") +
  # labels/themes
  labs(x = NULL, y = y_label,
       color = "Annual Averages", linetype = "Decadal Averages", linewidth = "Decadal Averages") +
  theme(legend.position = legend_position)

 return(#suppressWarnings(
   clim_plot)
 #)
}


