#' @include getSites.R
#' @include theme_WQ.R
#' @include sumClimMonthly.R
#' @include sumClimAvgs.R
#'
#' @title plotClimRel: Plot climate data relative to average value
#'
#' @importFrom dplyr arrange filter left_join
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @description This function plots a given year compared against either the 1980 - 2009
#' normal or the 1990 - 2019 normal. The 30-year normals are derived from Daymet data. The
#' annual comparisons are derived from the nearest weather station to each park.
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
#' @param years Numeric. Years to plot separately. Accepted values start at 2006.If multiple years
#' specified, will facet results on year. Note that selecting many years and/or many sites
#' may be slow to return results.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites.
#' If FALSE, returns all sites that have been monitored.
#'
#' @param parameter Specify the monthly averaged parameter to plot. Acceptable values are
#' \describe{
#'\item{"temp"}{Plot all temperature comparisons (in C).}
#' \item{"tmean"}{Plot mean temperature comparisons (in C).}
#' \item{"tmax"}{Plot max temperature comparisons (in C).}
#' \item{"tmin"}{Plot min temperature comparisons (in C).}
#' \item{"ppt"}{Plot precipitation comparisons (in mm).}
#' }
#'
#' @param averages Specify averages to plot. By default, the 30-year normal from 1980-2009 plots. Options include
#' one of the following:
#' \describe{
#' \item{"norm80"}{Plots the 30-year norm from 1980 - 2009}
#' \item{"norm90"}{Plots the 30-year norm from 1990 - 2019}
#' }
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
#' #++++ ADD EXAMPLES ++++
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimRel <- function(park = "all", site = "all",
                        site_type = c("all", "lake", "stream"),
                        years = 2006:format(Sys.Date(), "%Y"),
                        months = 1:12, active = TRUE,
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
  parameter <- match.arg(parameter, c("temp", "tmean", "tmax", "tmin", "ppt"))
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  data_type <- match.arg(data_type, c("daymet", "wstn"))
  if(all(!palette %in% c("viridis")) & length(years) > 1){stopifnot(length(palette) > 1)}
  stopifnot(class(plot_title) == "logical")
  averages <- match.arg(averages, c("norm80", "norm90"))

  #-- Compile data for plotting --
  # Clim data as annual monthly averages
  clim_dat1 <- sumClimMonthly(park = park, site = site, site_type = site_type,
                             active = active, years = years,
                             months = months, data_type = 'wstn', ...)

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
                         active = active, months = months, data_type = "wstn", ...)

  # filter on parameter and averages
  cols1 <- switch(parameter,
                 temp = c("tmax_C_30yr_80", "tmax_C_30yr_90", "tmean_C_30yr_80", "tmean_C_30yr_90",
                          "tmin_C_30yr_80", "tmean_C_30yr_90"),
                 tmax = c("tmax_C_30yr_80", "tmax_C_30yr_90"),
                 tmean = c("tmean_C_30yr_80", "tmean_C_30yr_90"),
                 tmin = c("tmax_C_30yr_80", "tmax_C_30yr_90"),
                 ppt = c("ppt_mm_30yr_80", "ppt_mm_30yr_90"))

  # select the 80 or 90 norm specified in averages
  cols <- cols1[grepl(as.numeric(substr(averages, 5, 6)), cols1)]

  avg_dat2 <- avg_dat[,c("SiteCode", "SiteName", "UnitCode", "month", cols)]
  # avg_dat2$mon <- factor(avg_dat2$month,
  #                        levels = unique(avg_dat2$month),
  #                        labels = unique(month.abb[avg_dat2$month]), ordered = T)
  avg_dat_long <- avg_dat2 |>
    pivot_longer(cols = -c(SiteCode, SiteName, UnitCode, month),
                 names_to = "param", values_to = "value")

  # Combine the annual and norm data so can calculate difference from normal using
  # a generic parameter label
  clim_dat_long$param_gen <- gsub("ws_|_mm|dm_|_C", "", clim_dat_long$param)

  avg_dat_long$param_gen <-
    gsub("_mm|_C|_30yr_80|_30yr_90|_d1980|_d1990|_d2000|_d2010|", "",
         avg_dat_long$param)

  clim_comb <- left_join(clim_dat_long, avg_dat_long,
                         by = c("SiteCode", "SiteName", "UnitCode", "month", "param_gen"),
                         suffix = c("_ws", "_norm")) |>
    filter(!is.na(value_norm)) |>
    mutate(rel_dif = value_ws - value_norm)

  clim_comb$param_gen <- factor(clim_comb$param_gen, levels = c("tmax", "tmean", "tmin", "ppt"), ordered = T)

  # set up plotting features
  ptitle <- if(length(unique(clim_dat_long2$SiteCode)) == 1 & plot_title == TRUE){
    unique(clim_dat_long2$SiteName)} else {NULL}

  color_dir <- ifelse(color_rev == FALSE, -1, 1)

  ylabel = if(parameter %in% "ppt"){"+/- mm of Precipitation"} else {"+/- Degrees C"}

  ylimits <- c(-max(abs(range(clim_comb$rel_dif))), max(abs(range(clim_comb$rel_dif))))

  pal <-
    if(!any(palette %in% "viridis")){
      if(length(palette) > 1){
        colorRampPalette(palette)(length(unique(clim_comb$param_ws)))
      } else { # hack to allow gradient to work with 1 color
        colorRampPalette(c(palette, palette))(length(unique(clim_comb$param_ws)))
      }
    }

  barplot <-
  ggplot(clim_comb,
         aes(x = mon, y = rel_dif, group = param_gen,
             color = param_gen, fill = param_gen)) +
    theme_WQ() +
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_hline(aes(yintercept = 0), linetype = "dashed", color = 'black', linewidth = 0.8) +
    # formatting
    labs(x = NULL, y = ylabel, group = NULL, color = NULL, fill = NULL) +
    ylim(ylimits) +
    # palettes
    {if(any(palette == 'viridis')) scale_fill_viridis_d(direction = color_dir, name = 'Parameter')} +
    {if(any(palette == 'viridis')) scale_color_viridis_d(direction = color_dir, name = 'Parameter')} +
    {if(!any(palette %in% 'viridis')) scale_fill_manual(values = pal, name = 'Parameter')} +
    {if(!any(palette %in% 'viridis')) scale_color_manual(values = pal, name = 'Parameter')} +
    # facets for multiple years
    {if(length(unique(clim_comb$year)) > 1) facet_wrap(~year)}


 return(#suppressWarnings(
   barplot
   )
 #)
}


