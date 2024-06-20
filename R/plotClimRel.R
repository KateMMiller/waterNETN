#' @include getSites.R
#' @include theme_WQ.R
#' @include getClimNOAA.R
#'
#' @title plotClimRel: Plot climate data relative to average value
#'
#' @importFrom dplyr arrange filter left_join
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @description This function plots a given year compared against either the 19th century
#' normal (1901 - 2000) or the latest 30-year normal (1991 - 2020) using gridded NOAA NClim data
#' for both the normals and the latest data. Works only at the park level. If a year x month
#' combination is specified that doesn't occur yet in NETN_clim_annual dataset, it will be
#' downloaded if available. New months are typically available within a few weeks of the month end.
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
#' @param years Numeric. Years to plot separately. Accepted values start at 2006.If multiple years
#' specified, will facet results on year.
#'
#' @param parameter Specify the monthly averaged parameter to plot. Acceptable values are
#' \describe{
#'\item{"temp"}{Plot all temperature comparisons (in C).}
#'\item{"tminmax}{Plot min and max temperature comparisons (in C).}
#' \item{"tmean"}{Plot mean temperature comparisons (in C).}
#' \item{"tmax"}{Plot max temperature comparisons (in C).}
#' \item{"tmin"}{Plot min temperature comparisons (in C).}
#' \item{"ppt"}{Plot precipitation comparisons (in mm).}
#' }
#'
#' @param averages Specify averages to plot. By default, the 20th century normal (1901-2000) plots.
#' Other options include:
#' \describe{
#' \item{"norm20cent"}{Plots the 20th century normal (1901 - 2000)}
#' \item{"norm1990"}{Plots the 30-year norm from 1991 - 2020}
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
#' @param title_type Specify whether to label facets with 4-letter UnitCode (default) or full UnitName.
#' Options are c("UnitCode", "UnitName").
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position =
#' "none" (Default). Other options are "top", "bottom", "left", "right".
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when either multiple years
#' are specified or multiple parks. Default is 3.
#'
#' @examples
#' \dontrun{
#'
#' # Plot all temperature variables on 1 graph for MABI in 2024.
#' plotClimRel(park = "MABI", years = 2024, parameter = "temp", palette = c("#EEE55A", "#D56062", "#067BC2"))
#'
#' # Plot precip for SARA 2023 compared to 1990 - 2019 normals.
#' plotClimRel(park = "SARA", years = 2023, parameter = "ppt", palette = "grey", averages = "norm1990")
#'
#' Plot precip for Kroma Kill in SARA 2023 and 2024 compared to 1990 - 2019 normals.
#' plotClimRel(park = "SARA", years = 2023:2024, parameter = "ppt",
#'             palette = "grey", averages = "norm1990")
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimRel <- function(park = "all",
                        years = 2006:format(Sys.Date(), "%Y"),
                        months = 1:12,
                        averages = "norm20cent",
                        parameter = 'tmean', plot_title = TRUE,
                        title_type = "UnitCode",
                        palette = "viridis", color_rev = FALSE,
                        legend_position = 'right', numcol = 3){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "all")){park = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  parameter <- match.arg(parameter, c("temp", "tminmax", "tmean", "tmax", "tmin", "ppt"))
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  #if(all(!palette %in% c("viridis")) & length(years) > 1){stopifnot(length(palette) > 1)}
  stopifnot(class(plot_title) == "logical")
  averages <- match.arg(averages, c("norm20cent", "norm1990"))
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  title_type <- match.arg(title_type, c("UnitCode", "UnitName"))

  #-- Compile data for plotting --
  # Clim data as annual monthly averages
  data("NETN_clim_annual")
  data("NETN_clim_norms")

  clim_dat <- NETN_clim_annual |> filter(UnitCode %in% park)
  clim_dat2 <- clim_dat |> filter(year %in% years) |> filter(month %in% months)
  clim_dat2$date <- as.Date(paste0(clim_dat2$year, "-", clim_dat2$month, "-", 15), format = "%Y-%m-%d")

  clim_dat_long <-
    clim_dat2 |> pivot_longer(cols = -c(UnitCode, UnitName, year, month, date, lat, long),
                              names_to = "param", values_to = "value") |>
    arrange(UnitCode, month, param)

  # Update clim data if requesting a year x month combination that is not currently in
  # the saved NETN_clim_annual.rda but only for complete months
  date_range_data <- sort(unique(clim_dat_long$date))
  date_range_fxn <- paste0(rep(years, length(months)),"-", rep(sprintf("%02d", months), length(years)), "-", 15)
  new_dates1 <- date_range_fxn[!date_range_fxn %in% date_range_data]

  # latest date of complete month
  mon_curr <- as.numeric(format(Sys.Date(), "%m"))
  mon_next_day <- as.numeric(format(Sys.Date() + 1, "%m"))
  mon_comp <- ifelse(mon_next_day > mon_curr, sprintf("%02d", mon_curr), sprintf("%02d", mon_curr - 1))
  latest_date_comp <- as.Date(paste0(format(Sys.Date(), "%Y"), "-", mon_comp, "-", 15))
  latest_date_data <- as.Date(max(date_range_data), format = "%Y-%m-%d")

  new_dates <- as.Date(new_dates1[new_dates1 <= latest_date_comp], format = "%Y-%m-%d")
  #new_dates <- as.Date(c("2024-05-15", "2024-04-15"), format = "%Y-%m-%d")

  clim_dat_final1 <-
    if(length(new_dates) == 0){clim_dat_long
    } else {
      new_months <- as.numeric(format(new_dates, "%m"))
      new_years <- as.numeric(format(new_dates, "%Y"))
      new_clim <- purrr::map(new_years, function(y){
        getClimNOAA(park = park, year = y, months = new_months)}
      ) |> list_rbind()
      new_clim_long <- new_clim |> pivot_longer(cols = -c(UnitCode, UnitName, long, lat, year, month),
                                                names_to = "param", values_to = "value")
      new_clim_long$date <- as.Date(paste0(
        new_clim_long$year, "-", new_clim_long$month, "-", 15), format = "%Y-%m-%d")
      comb_clim <- rbind(clim_dat_long, new_clim_long)
    }

  park_names <- unique(getSites(park = park)[,c("UnitCode", "UnitName")])
  clim_dat_final2 <- left_join(clim_dat_final1, park_names, by = "UnitCode")

  # Clim data in decadal and 30-year norms
  avg_dat <- NETN_clim_norms |> filter(UnitCode %in% park) #|> filter(month %in% months)

  avg_dat_long <- avg_dat |> pivot_longer(cols = -c(UnitCode, UnitName, long, lat, month),
                                          names_to = "param_full", values_to = "value") |>
    mutate(param = sub("_.*", "", param_full),
           stat = ifelse(grepl("norm", param_full), "avg", "std"),
           norm = ifelse(grepl(1901, param_full), "norm20cent", "norm1990")) |>
    arrange(UnitCode, param, month)

  # filter on parameter and averages
  cols1 <- switch(parameter,
                  temp = c("tmax_norm_1901_2000", "tmax_norm_1991_2020",
                           "tavg_norm_1901_2000", "tavg_norm_1991_2020",
                           "tmin_norm_1901_2000", "tmin_norm_1991_2020"),
                  tminmax = c("tmax_norm_1901_2000", "tmax_norm_1991_2020",
                              "tmin_norm_1901_2000", "tmin_norm_1991_2020"),
                  tmax = c("tmax_norm_1901_2000", "tmax_norm_1991_2020"),
                  tmean = c("tavg_norm_1901_2000", "tavg_norm_1991_2020"),
                  tmin = c("tmin_norm_1901_2000", "tmin_norm_1991_2020"),
                  ppt = c("precip_norm_1901_2000", "precip_norm_1991_2020"))

  # select the 80 or 90 norm specified in averages
  cols <- cols1[grepl(as.numeric(substr(averages, 5, 6)), cols1)]

  avg_dat_long2 <- avg_dat_long |> filter(param_full %in% cols) |>
    filter(stat == "avg") |> filter(norm == averages)

  # Combine the annual and norm data so can calculate difference from normal using
  # a generic parameter label
  clim_dat_long$param <- gsub("prcp", "ppt", clim_dat_long$param)
  clim_dat_long$param <- gsub("tavg", "tmean", clim_dat_long$param)

  avg_dat_long2$param <- gsub("precip", "ppt", avg_dat_long2$param)
  avg_dat_long2$param <- gsub("tavg", "tmean", avg_dat_long2$param)

  clim_comb <- left_join(clim_dat_long, avg_dat_long2,
                         by = c("UnitCode", "UnitName", "month", "param"),
                         suffix = c("_curr", "_norm")) |>
    filter(!is.na(value_norm)) |>
    mutate(rel_dif = value_curr - value_norm,
           pct_dif = ((value_curr - value_norm)/value_norm)*100)

  clim_comb$param <- factor(clim_comb$param, levels = c("tmax", "tmean", "tmin", "ppt"), ordered = T)

  # set up plotting features
  ptitle <- if(length(unique(clim_comb$UnitCode)) == 1 & plot_title == TRUE){
    unique(clim_comb$UnitName)} else {NULL}

  color_dir <- ifelse(color_rev == FALSE, -1, 1)

  ylabel = if(parameter %in% "ppt"){"+/- % of Precipitation"} else {"+/- Degrees C"}

  ylimits <- c(-max(abs(range(clim_comb$rel_dif))), max(abs(range(clim_comb$rel_dif))))

  pal <-
    if(!any(palette %in% "viridis")){
      if(length(palette) > 1){
        colorRampPalette(palette)(length(unique(clim_comb$param)))
      } else { # hack to allow gradient to work with 1 color
        colorRampPalette(c(palette, palette))(length(unique(clim_comb$param)))
      }
    }

  # set up legend labels
  param_labels <-
    data.frame(param = c("tmax", "tmean", "tmin", "ppt"),
               param_label = c("Max. Temp. (C)", "Avg. Temp. (C)",
                               "Min. Temp. (C)", "Total Precip. (mm)"))

  clim_comb2 <- left_join(clim_comb, param_labels, by = "param") |> droplevels()

  clim_comb2$mon <- factor(clim_comb2$month,
                          levels = unique(clim_comb2$month),
                          labels = unique(month.abb[clim_comb2$month]), ordered = T)

  clim_comb2$park_facet <- if(title_type == "UnitCode"){clim_comb2$UnitCode} else {clim_comb2$UnitName}
  facet_park <- ifelse(length(park) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(years) > 1, TRUE, FALSE)

  barplot <-
    if(parameter %in% "ppt"){
      ggplot(clim_comb2,
             aes(x = mon, y = pct_dif,
                 group = param_label,
                 color = param_label, fill = param_label)) +
        theme_WQ() +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_hline(aes(yintercept = 0), linetype = "dashed", color = 'black', linewidth = 0.8) +
        # formatting
        labs(x = NULL, y = ylabel, group = NULL, color = NULL, fill = NULL) +
        theme(legend.position = legend_position,
              axis.text.x = element_text(angle = 90)) +
        # facetting
        {if(facet_park == TRUE & facet_year == FALSE){facet_wrap(~park_facet, ncol = numcol)}} +
        {if(facet_park == FALSE & facet_year == TRUE){facet_wrap(~year, ncol = numcol)}} +
        {if(facet_park == TRUE & facet_year == TRUE){facet_wrap(~park_facet + year, ncol = numcol)}} +
        # palettes
        {if(any(palette == 'viridis')) scale_fill_viridis_d(direction = color_dir, name = 'Parameter')} +
        {if(any(palette == 'viridis')) scale_color_viridis_d(direction = color_dir, name = 'Parameter')} +
        {if(!any(palette %in% 'viridis')) scale_fill_manual(values = pal, name = 'Parameter')} +
        {if(!any(palette %in% 'viridis')) scale_color_manual(values = pal, name = 'Parameter')}

    } else {
      ggplot(clim_comb2,
        aes(x = mon, y = rel_dif,
             group = param_label,
             color = param_label, fill = param_label)) +
        theme_WQ() +
        geom_bar(stat = 'identity', position = 'dodge') +
        geom_hline(aes(yintercept = 0), linetype = "dashed", color = 'black', linewidth = 0.8) +
        # formatting
        labs(x = NULL, y = ylabel, group = NULL, color = NULL, fill = NULL) +
        theme(legend.position = legend_position,
              axis.text.x = element_text(angle = 90)) +
        ylim(ylimits) +
        # facetting
        {if(facet_park == TRUE & facet_year == FALSE){facet_wrap(~park_facet, ncol = numcol)}} +
        {if(facet_park == FALSE & facet_year == TRUE){facet_wrap(~year, ncol = numcol)}} +
        {if(facet_park == TRUE & facet_year == TRUE){facet_wrap(~park_facet + year, ncol = numcol)}} +
        # palettes
        {if(any(palette == 'viridis')) scale_fill_viridis_d(direction = color_dir, name = 'Parameter')} +
        {if(any(palette == 'viridis')) scale_color_viridis_d(direction = color_dir, name = 'Parameter')} +
        {if(!any(palette %in% 'viridis')) scale_fill_manual(values = pal, name = 'Parameter')} +
        {if(!any(palette %in% 'viridis')) scale_color_manual(values = pal, name = 'Parameter')}
    }

 return(#suppressWarnings(
   barplot
   )
 #)
}


