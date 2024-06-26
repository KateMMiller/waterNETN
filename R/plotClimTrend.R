#' @include getSites.R
#' @include theme_WQ.R
#' @include getClimNOAA.R
#'
#' @title plotClimTrend: Plot climate trends
#'
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map list_rbind
#' @import ggplot2
#'
#' @description This function produces a line or smoothed trend plot filtered on park, year, month, and
#' climate parameter. If multiple parks are specified, they can either be plotted on the same figure or separate
#' figures. If multiple parameters are specified, they will be plotted on separate figures.
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
#' @param years Numeric. Years to query. Accepted values start at 1895.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12.
#' If specifying new months not yet included in NETN_clim_annual dataset, will
#' download months that are available from NOAA.
#'
#' @param parameter Specify the parameter(s) to plot. Acceptable values are
#' \describe{
#' \item{"all"}{Plot all climate variables}
#' \item{"ppt"}{Monthly total precipitation in mm.}
#' \item{"tmax"}{Monthly average maximum temperature in C.}
#' \item{"tmin}{Monthly average minimum temperature in C.}
#' \item{"tmean}{Monthly average temperature in C.}
#' }
#'
#' @param facet_park Logical. If TRUE, plots sites on separate facets (ie figures). If FALSE (Default),
#' plots all sites on the same figure. This is only enabled if multiple sites are chosen.
#'
#' @param facet_param Logical. If TRUE (Default), plots parameters on separate facets. If FALSE, plots
#' all parameters on the same figure. Note that results will be funky if selected parameters have different units (e.g., temp and precip).
#'
#' @param layers Options are "points", "lines", "smooth", and "bar". By default, both points and lines will plot. The lines option connects
#' each monthly value with a linear line. If you include lines and smooth, the loess-smoothed line will also plot.
#' The bar argument plots a bar chart.
#'
#' @param palette Theme to plot points and lines. Options currently are 'viridis' (Default- ranges of blue, green and yellow), or discrete palettes from RColorBrewer. Common options are "Set1", "Set2", "Dark2", "Accent".
#' Run RColorBrewer::display.brewer.all(type = 'qual') to see full set of options.
#'
#' @param span Numeric. Determines how smoothed the line will be for layers = 'smooth'. Default is 0.3. Higher spans (up to 1)
#' cause more smoothing (straighter lines). Smaller spans are wavier. Span can range from 0 to 1. Span of 1 is linear.
#'
#' @param plot_se Logical. If TRUE, will plot a standard error ribbon. If FALSE (Default), will not plot a ribbon.
#' Only enabled if layers = "smooth" is specified.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @examples
#' \dontrun{
#'
#' # Plot total monthly precip for 2006:2023 and all months, without smoothing
#' plotClimTrend(park = "MABI", years = 2006:2023, parameter = "ppt", smooth = F)
#'
#' # Plot avg temp in MABI from 1895:2024, without points and with smoothed line and span 0.1.
#' plotClimTrend(park = "MABI", years = 1895:2024, layers = 'lines', parameter = "tmean", span = 0.1)
#'
#' # Plot monthly mean temperature for MABI and SARA from 2006:2024, with smoothed line, span 0.7,
#' and only sample months using the Dark2 color palette.
#' plotClimTrend(park = c("MABI", "SARA"), years = 2006:2024,
#'               parameter = "tmean", span = 0.7, months = 5:10, palette = "Dark2")
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimTrend <- function(park = "all",
                          years = 2006:format(Sys.Date(), "%Y"),
                          months = 1:12,
                          layers = c("points", "lines"),
                          parameter = NA,
                          facet_park = FALSE,
                          facet_param = TRUE,
                          palette = "viridis",
                          span = 0.3, plot_se = FALSE,
                          legend_position = 'none', gridlines = 'none'){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "all")){park = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")
    } else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1895)

  parameter <- match.arg(parameter, c("all", "tmean", "tmin", "tmax", "ppt"), several.ok = TRUE)
  facet_y <- if(any(parameter == "all")){"free_y"} else {"fixed"}

  if(any(parameter == "all")){parameter = c("tmean", "tmin", "tmax", "ppt")}
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(span) %in% "numeric")
  stopifnot(class(plot_se) %in% "logical")
  layers <- match.arg(layers, c("points", "lines", "smooth", "bar"), several.ok = TRUE)
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))

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
  # the saved NETN_clim_2006_2024.rda but only for complete months
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

  clim_dat <-
    if(length(new_dates) == 0){clim_dat_long
    } else {
      new_months <- as.numeric(format(new_dates, "%m"))
      new_years <- as.numeric(format(new_dates, "%Y"))
      new_clim <- purrr::map(new_years, function(y){
        getClimNOAA(park = park, year = y, months = new_months)}
      ) |> list_rbind()
      if(nrow(new_clim) > 0){
      new_clim_long <- new_clim |> pivot_longer(cols = -c(UnitCode, UnitName, long, lat, year, month),
                                                names_to = "param", values_to = "value")
      new_clim_long$date <- as.Date(paste0(
        new_clim_long$year, "-", new_clim_long$month, "-", 15), format = "%Y-%m-%d")
      comb_clim <- rbind(clim_dat_long, new_clim_long)
      } else {clim_dat_long}
    }

  clim_dat$param[clim_dat$param == "prcp"] <- "ppt"
  clim_dat$param[clim_dat$param == "tavg"] <- "tmean"

  if(nrow(clim_dat) == 0){stop("Specified arguments returned a data frame with 0 records.")}

  param <- if(any(parameter == "all")){c("ppt", "tmean", "tmax", "tmin")} else {parameter}

  #-- Set up plotting features --
  ylab <- ifelse(length(unique(clim_dat$param)) == 1, unique(clim_dat$param), "Monthly Value")

  facetpark <- ifelse(facet_park == TRUE & length(unique(clim_dat$UnitCode)) > 1, TRUE, FALSE)
  facetparam <- ifelse(facet_param == TRUE & length(unique(clim_dat$param)) > 1, TRUE, FALSE)

  pars <- c("ppt", "tmax", "tmin", "tmean")

  plabs <- c("Total Precip. (mm)", "Avg. Max. Temp. (C)", "Avg. Min. Temp. (C)",
             "Average Temp. (C)")

  param_labels <- data.frame(param = pars, param_label = plabs)

  clim_dat1 <- left_join(clim_dat, param_labels, by = 'param')

  ylab <- ifelse(length(parameter) > 1, "Monthly Value",
                 param_labels$param_label[param_labels$param == parameter])

  clim_dat1$date2 <- as.Date(clim_dat1$date, format = c("%Y-%m-%d"))

  clim_dat_final <- clim_dat1 |> filter(param %in% parameter)

  year_len <- length(unique(clim_dat_final$year))
  mon_len <- length(unique(clim_dat_final$month))

  break_len <- if(year_len == 1){"1 month"
  } else if(year_len  %in% c(2, 3, 4) & mon_len <= 6){"2 months"
  } else if(year_len == 2 & mon_len > 6){"4 months"
    #} else if(year_len > 4 & mon_len <= 6){"6 months"
  } else if(year_len %in% c(4:19)){"1 year"
  } else if(year_len %in% c(20:40)){"2 years"
  } else if(year_len > 40){"5 years"
  } else {"6 months"}

  date_format <- ifelse(break_len %in% c("1 year", "2 years", "5 years"), "%Y",
                        ifelse(break_len %in% c("2 months", "4 months"), "%b/%Y",
                               "%b"))
  datebreaks <- seq(min(clim_dat_final$date2, na.rm = T), max(clim_dat_final$date2, na.rm = T) + 30, by = break_len)

  seq_int <- if(any(parameter == "ppt")){20} else {2}

  #ybreaks <- seq(floor(min(clim_dat_final$value)), ceiling(max(clim_dat_final$value)), seq_int)

  #summary(clim_dat_final$value)
  #-- Create plot --
  climtrendplot <-
    ggplot(clim_dat_final, aes(x = date2, y = value,
                               group = if(facetpark == TRUE & facetparam == FALSE){param_label
                               } else if(facetpark == FALSE & facetparam == TRUE){UnitCode
                               } else if(facetpark == TRUE & facetparam == TRUE){interaction(param_label, UnitCode)
                               } else {param_label},
                               color = if(facetpark == TRUE & facetparam == FALSE){param_label
                               } else if(facetpark == FALSE & facetparam == TRUE){UnitCode
                               } else if(facetpark == TRUE & facetparam == TRUE){interaction(param_label, UnitCode)
                               } else {param_label},
                               fill = if(facetpark == TRUE & facetparam == FALSE){param_label
                               } else if(facetpark == FALSE & facetparam == TRUE){UnitCode
                               } else if(facetpark == TRUE & facetparam == TRUE){interaction(param_label, UnitCode)
                               } else {param_label})) +
      # layers
      {if(any(layers %in% "smooth"))
        geom_smooth(method = 'loess', formula = 'y ~ x', se = plot_se, span = span, alpha = 0.2) } +
      {if(any(layers %in% "lines")) geom_line()} +
      {if(any(layers %in% "points")) geom_point(alpha = 0.6)} +
      {if(any(layers %in% "bar")) geom_bar(stat = 'identity')} +
      # themes
      theme_WQ() + theme(legend.position = legend_position,
                         legend.title = element_blank(),
                         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    {if(any(gridlines %in% c("grid_y", "both"))){
      theme(
        panel.grid.major.y = element_line(color = 'grey'))}} + #,
    #panel.grid.minor.y = element_line(color = 'grey'))}} +
    {if(any(gridlines %in% c("grid_x", "both"))){
      theme(
        panel.grid.major.x = element_line(color = 'grey'))}} +#,
    #panel.grid.minor.x = element_line(color = 'grey'))}} +
      # facets
      {if(facetparam == FALSE & facetpark == TRUE){facet_wrap(~UnitName)}}+
      {if(facetparam == TRUE & facetpark == FALSE){facet_wrap(~param_label, scales = facet_y)}}+
      {if(facetparam == TRUE & facetpark == TRUE){facet_wrap(~UnitName + param_label)}}+
      # palettes
      {if(palette == "viridis") scale_color_viridis_d()} +
      {if(palette == "viridis") scale_fill_viridis_d()} +
      {if(!palette == "viridis") scale_color_brewer(palette = palette)} +
      {if(!palette == "viridis") scale_fill_brewer(palette = palette)} +
      # axis format
      scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
      scale_y_continuous(n.breaks = 8) +
      # labels/themes
      labs(x = NULL, y = ylab)

 return(#suppressWarnings(
   climtrendplot)#)
}


