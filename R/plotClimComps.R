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
#' \item{"tmean"}{Plot mean temperature comparisons (in C).}
#' \item{"tmax"}{Plot max temperature comparisons (in C).}
#' \item{"tmin"}{Plot min temperature comparisons (in C).}
#' \item{"ppt"}{Plot precipitation comparisons (in mm).}
#' }
#'
#' @param data_type Specify Daymet ("daymet"; default), or weather station ("wstn"). Note that Daymet
#' data aren't available immediately, whereas weather station data may be available within days of current day.
#' When 'wstn' is selected, the averages are based on Daymet data at the coordinates of the weather station,
#' because the closest weather stations to most parks do not have a period or record prior to 2000.
#'
#' @param averages Specify averages to plot. By default, the 30-year normal from 1980-2009 plots. Options include
#' one of the following:
#' \describe{
#' \item{"norm80"}{Plots the 30-year norm from 1980 - 2009}
#' \item{"norm90"}{Plots the 30-year norm from 1990 - 2019}
#' \item{"decades"}{Plots 1980 - 1989, 1990 - 1999, 2000 - 2009, and 2010 - 2019 averages}
#' }
#'
#' @param layers Options are "points", "lines", or both for annual averages. By default, only lines will plot.
#'
#' @param palette Color palette for plots. Options currently are 'viridis' (yellow - green - blue),
#' or create your own by specifying 2 or more colors in quotes, like palette = c("red", "yellow", "blue"),
#' or palette = c("yellow", "blue"). Hexcodes work too. If only 1 year is specified, only need to
#' specify 1 color
#'
#' @param color_rev Reverse the order of the color pallet. Only enabled for palette = 'viridis'.
#'
#' @param plot_title Logical. If TRUE (default) prints site name at top of figure. If FALSE,
#' does not print site name. Only enabled when one site is selected.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position =
#' "none" (Default). Other options are "top", "bottom", "left", "right".
#'
#' @param ... Additional arguments relevant to \code{sumClimAvgs()} or \code{sumClimMonthly()}
#'
#' @examples
#' \dontrun{
#'
#' # Plot mean monthly temp for the Pogue in MABI for 2019:2023 and all months with red-blue color palette
#' plotClimComps(site = "MABIPA", years = 2019:2023, parameter = "temp_mean", palette = c('red', 'blue'))
#'
#' # Same as above, but with points too
#' plotClimComps(site = "MABIPA", years = 2019:2023, parameter = "temp_mean", palette = c('red', 'blue'),
#' layers = c('points', 'lines'))
#'
#' # Plot max monthly temp for Eagle Lake and Jordan Pond for 2019:2023 and all months
#' plotClimComps(site = c("ACEAGL", "ACJORD"), years = 2019:2023, parameter = "temp_max",
#' palette = c('red', 'blue'), layers = 'lines')
#'
#' # Plot total monthly precip for Jordan Pond for past 10 years using a blue color scheme
#' plotClimComps(site = "ACJORD", years = 2013:2023, parameter = 'precip',
#' palette = c("#75C5FF", "#3563DD", "#323969"))
#'
#' # Plot total monthly precip for latest year using weather station data
#' plotClimComps(site = "ACJORD", years = 2024, parameter = "precip", data_type = 'wstn')
#'
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
                          layers = "lines", data_type = 'daymet',
                          averages = "norm80",
                          parameter = 'tmean', plot_title = TRUE,
                          palette = "viridis", color_rev = FALSE,
                          legend_position = 'right', ...){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  parameter <- match.arg(parameter, c("tmean", "tmax", "tmin", "ppt"))
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  data_type <- match.arg(data_type, c("daymet", "wstn"))
  if(all(!palette %in% c("viridis")) & length(years) > 1){stopifnot(length(palette) > 1)}
  stopifnot(class(plot_title) == "logical")
  averages <- match.arg(averages, c("norm80", "norm90", "decades"))

  #-- Compile data for plotting --
  # Clim data as annual monthly averages
  clim_dat1 <- sumClimMonthly(park = park, site = site, site_type = site_type,
                             active = active, years = years,
                             months = months, data_type = data_type, ...)

  clim_dat <- if(data_type == "daymet"){clim_dat1[,c("SiteCode", "SiteName", "UnitCode", "year", "month",
                                                     "mon", "dm_ppt_mm", "dm_tmax_C", "dm_tmin_C", "dm_tmean_C")]
  } else {clim_dat1[,c("SiteCode", "SiteName", "UnitCode", "year", "month", "mon", "ws_ppt_mm",
                       "ws_tmax_C", "ws_tmin_C", "ws_tmean_C")]}

  if(nrow(clim_dat) == 0){stop("Specified arguments returned a data frame with 0 records.")}

  clim_dat_long <-
      clim_dat |> pivot_longer(cols = -c(SiteCode, SiteName, UnitCode, year, month, mon),
                               names_to = "param", values_to = "value") |>
                  arrange(SiteCode, month, param)

  clim_dat_long$date <- as.Date(paste0(
    clim_dat_long$year, "-", clim_dat_long$month, "-", 15), format = "%Y-%m-%d")

  # Clim data in decadal and 30-year norms
  wstndat <- if(data_type == 'wstn') {TRUE} else {FALSE}

  avg_dat <- sumClimAvgs(park = park, site = site, site_type = site_type,
                         active = active, months = months, data_type = data_type, ...)

  avg_dat_long <- avg_dat |> pivot_longer(ppt_mm_d1980:ncol(avg_dat), names_to = "param", values_to = "value") |>
    arrange(SiteCode, param, month)

  #-- Set up plotting features --
  color_dir <- ifelse(color_rev == FALSE, -1, 1)

  # annual params
  param_labels_annual <-
    data.frame(param = c("dm_ppt_mm", "dm_tmax_C", "dm_tmin_C", "dm_tmean_C", "dm_BAL",
                         "dm_SPEI01", "dm_SPEI03",
                         "ws_ppt_mm", "ws_tmax_C", "ws_tmin_C", "ws_tmean_C"),
               param_label = c("Total Precip. (mm)", "Max. Temp. (C)", "Min. Temp. (C)",
                               "Avg. Temp. (C)", "Water Balance", "SPEI - 1 month", "SPEI - 3 month",
                               "Total Precip. (mm)", "Max. Temp. (C)", "Min. Temp. (C)",
                               "Avg. Temp. (C)"))
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
                      param_label = c(rep(c("years 1980-1989", "years 1990-1999",
                                            "years 2000-2009", "years 2010-2019"), 4),
                                            "years 1980-2009", "years 1980-2009",
                                            "years 1980-2009", "years 1980-2009",
                                            "years 1990-2019", "years 1990-2019",
                                            "years 1990-2019", "years 1990-2019"))

  avg_dat_long2 <- left_join(avg_dat_long, norms, by = "param")

  avg_dat_long2$mon <- factor(avg_dat_long2$month,
                              levels = unique(avg_dat_long2$month),
                              labels = unique(month.abb[avg_dat_long2$month]), ordered = T)

  # set up filter and labelling on parameter
  if(parameter == "tmean"){
    ann_filt = c("dm_tmean_C", "ws_tmean_C")
    dec_filt = c("tmean_C_d1980", "tmean_C_d1990", "tmean_C_d2000", "tmean_C_d2010")
    norm_filt <- if(!averages %in% 'decades'){if(averages == "norm80"){"tmean_C_30yr_80"} else {"tmean_C_30yr_90"}}
    y_label = "Avg. Monthly Temp. (C)"
  } else if(parameter == "tmin"){
    ann_filt = c("dm_tmin_C", "ws_tmin_C")
    dec_filt = c("tmin_C_d1980", "tmin_C_d1990", "tmin_C_d2000", "tmin_C_d2010")
    norm_filt <- if(!averages %in% 'decades'){if(averages == "norm80"){"tmin_C_30yr_80"} else {"tmin_C_30yr_90"}}
    y_label = "Avg. Minimum Monthly Temp. (C)"
  } else if(parameter == "tmax"){
    ann_filt = c("dm_tmax_C", "ws_tmax_C")
    dec_filt = c("tmax_C_d1980", "tmax_C_d1990", "tmax_C_d2000", "tmax_C_d2010")
    norm_filt <- if(!averages %in% 'decades'){if(averages == "norm80"){"tmax_C_30yr_80"} else {"tmax_C_30yr_90"}}
    y_label = "Avg. Maximum Monthly Temp. (C)"
  } else if(parameter == "ppt"){
    ann_filt = c("dm_ppt_mm", "ws_ppt_mm")
    dec_filt = c("ppt_mm_d1980", "ppt_mm_d1990", "ppt_mm_d2000", "ppt_mm_d2010")
    norm_filt <- if(!averages %in% 'decades'){if(averages == "norm80"){"ppt_mm_30yr_80"} else {"ppt_mm_30yr_90"}}
    y_label = "Total Monthly Precip. (mm)"
  }

  year_breaks <-
    if(length(years) <= 5){years
    } else {c(quantile(clim_dat_long2$year, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE))}

  pal <-
    if(!any(palette %in% "viridis")){
    if(length(palette) > 1){
    colorRampPalette(palette)(length(unique(clim_dat_long2$year)))
  } else { # hack to allow gradient to work with 1 color
    colorRampPalette(c(palette, palette))(length(unique(clim_dat_long2$year)))
  }
  }

  leg_guide <- if(length(years) > 5){"colourbar"} else{"legend"}

  ptitle <- if(length(unique(clim_dat_long2$SiteCode)) == 1 & plot_title == TRUE){
    unique(clim_dat_long2$SiteName)} else {NULL}

  avg_name <- ifelse(averages == "decadal", "Decadal Averages", "30 year Normals")

  clim_plot <-
  ggplot() + theme_WQ() +
  # layers for decadal data
  {if(averages == "decades"){
  geom_line(data = avg_dat_long2 |> filter(param %in% dec_filt),
            aes(x = mon, y = value, group = param_label, linetype = param_label), linewidth = 0.8)}} +
  # line type for decadal data
  {if(averages == "decades"){
  scale_linetype_manual(values = c("dotted", "dashed", "longdash", "solid"), name = "Decadal Averages")}} +
  # layers for 30-year normals
  {if(averages %in% c("norm80", "norm90")){
  geom_line(data = avg_dat_long2 |> filter(param %in% norm_filt),
            aes(x = mon, y = value, group = param_label, linetype = param_label),
            linewidth = 0.8)}} +
  # line type for 30-year normals
  {if(averages %in% c("norm80", "norm90")){
  scale_linetype_manual(values = c("longdash"), name = "30 year Normals")}} +
  # layers for annual data
  {if(any(layers %in% "lines"))
      geom_line(data = clim_dat_long2 |> filter(param %in% ann_filt),
                aes(x = mon, y = value, group = as.integer(year), color = as.integer(year)))} +
  {if(any(layers %in% "points"))
      geom_point(data = clim_dat_long2 |> filter(param %in% ann_filt),
                 aes(x = mon, y = value, group = as.integer(year),
                     color = as.integer(year), fill = as.integer(year)))} +
  # color palettes for annual data
  {if(any(palette == 'viridis')) scale_fill_viridis_c(direction = color_dir, guide = leg_guide,
                                                     name = 'Annual Averages', breaks = year_breaks)} +
  {if(any(palette == 'viridis')) scale_color_viridis_c(direction = color_dir, guide = leg_guide,
                                                      name = 'Annual Averages', breaks = year_breaks)} +
  {if(!any(palette %in% 'viridis')) scale_fill_gradientn(colors = pal, guide = leg_guide,
                                                    name = 'Annual Averages',
                                                    breaks = year_breaks)} +
  {if(!any(palette %in% 'viridis')) scale_color_gradientn(colors = pal, guide = leg_guide,
                                                     name = 'Annual Averages',
                                                     breaks = year_breaks)} +
  # facets for multiple years
  {if(length(unique(clim_dat_long$SiteCode)) > 1) facet_wrap(~SiteName)} +

  # labels/themes
  labs(x = NULL, y = y_label, title = ptitle,
       color = "Annual Averages", linetype = avg_name, linewidth = avg_name) +
  theme(legend.position = legend_position)

 return(#suppressWarnings(
   clim_plot
   )
 #)
}


