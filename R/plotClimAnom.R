#' @include theme_WQ.R
#' @include getClimNOAA.R
#'
#' @title plotClimAnom: Plot climate anomalies from baseline
#'
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map list_rbind
#' @import ggplot2
#'
#' @description This function produces a trend plot filtered on park, year, month, and
#' climate parameter and shows anomalies from chosen baseline (1901 - 2000 or 1991 - 2020)
#' as red for above and blue for below average using gridded NOAA NClim data. If a year x month
#' combination is specified that doesn't occur yet in NETN_clim_annual dataset, it will be
#' downloaded if available. New months are typically available within a few weeks of the month end.
#' If multiple parks are specified, they can either be plotted on the same figure or separate
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
#' @param parameter Specify the monthly averaged parameter to plot. Acceptable values are
#' \describe{
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
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default).
#' Other options are "top", "bottom", "left", "right".
#'
#' @param title_type Specify whether to label facets with 4-letter UnitCode (default) or full UnitName.
#' Options are c("UnitCode", "UnitName").
#'
#' @examples
#' \dontrun{
#'
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotClimAnom <- function(park = "all",
                          years = 1895:format(Sys.Date(), "%Y"),
                          months = 1:12,
                          parameter = 'tmean',
                          averages = "norm20cent",
                          legend_position = 'none',
                          title_type = "UnitCode"){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "all")){park = c("ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")
  } else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1895)

  parameter <- match.arg(parameter, c("tmean", "tmax", "tmin", "ppt"), several.ok = TRUE)
  averages <- match.arg(averages, c("norm20cent", "norm1990"))
  if(any(parameter == "all")){parameter = c("tmean", "tmin", "tmax", "ppt")}
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))

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

  avg_dat <- NETN_clim_norms |> filter(UnitCode %in% park) #|> filter(month %in% months)
  avg_dat_long <- avg_dat |> pivot_longer(cols = -c(UnitCode, UnitName, long, lat, month),
                                          names_to = "param_full", values_to = "value") |>
    mutate(param = sub("_.*", "", param_full),
           stat = ifelse(grepl("norm", param_full), "avg", "std"),
           norm = ifelse(grepl(1901, param_full), "norm20cent", "norm1990")) |>
    arrange(UnitCode, param, month)

  avg_dat_long2 <- avg_dat_long |> filter(norm == averages)

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
      new_clim_long <- new_clim |> pivot_longer(cols = -c(UnitCode, UnitName, long, lat, year, month),
                                                names_to = "param", values_to = "value")
      new_clim_long$date <- as.Date(paste0(
        new_clim_long$year, "-", new_clim_long$month, "-", 15), format = "%Y-%m-%d")
      comb_clim <- rbind(clim_dat_long, new_clim_long)
    }

  if(nrow(clim_dat) == 0){stop("Specified arguments returned a data frame with 0 records.")}

  clim_dat$param[clim_dat$param == "prcp"] <- "ppt"
  clim_dat$param[clim_dat$param == "tavg"] <- "tmean"
  avg_dat_long2$param[avg_dat_long2$param == "precip"] <- "ppt"
  avg_dat_long2$param[avg_dat_long2$param == "tavg"] <- "tmean"

  avg_dat3 <- avg_dat_long2 |> dplyr::select(-param_full) |> pivot_wider(names_from = stat, values_from = value)

  clim_comb <- left_join(clim_dat, avg_dat3 |> dplyr::select(-lat, -long),
                         by = c("UnitCode", "UnitName", "month", "param"))

  clim_comb$anom <- clim_comb$value - clim_comb$avg
  clim_comb$anom_type <- ifelse(clim_comb$anom >0, "above", "below")

  param <- if(any(parameter == "all")){c("ppt", "tmean", "tmax", "tmin")} else {parameter}

  #-- Set up plotting features --
  ylab <- ifelse(length(unique(clim_comb$param)) == 1, unique(clim_comb$param), "Monthly Value")

  facetpark <- ifelse(length(unique(clim_comb$UnitCode)) > 1, TRUE, FALSE)
  facetparam <- ifelse(length(unique(clim_comb$param)) > 1, TRUE, FALSE)
  facet_y <- if(any(parameter == "all") | length(parameter) > 1 & any(parameter %in% "ppt" )){"free_y"} else {"fixed"}

  clim_comb$park_facet <- if(title_type == "UnitCode"){clim_comb$UnitCode} else {clim_comb$UnitName}

  pars <- c("ppt", "tmax", "tmin", "tmean")

  plabs <- c("Total Precip. (mm)", "Avg. Max. Temp. (C)", "Avg. Min. Temp. (C)",
             "Average Temp. (C)")

  param_labels <- data.frame(param = pars, param_label = plabs)

  clim_comb1 <- left_join(clim_comb, param_labels, by = 'param')

  ylab <- ifelse(length(parameter) > 1, "Monthly Value", param_labels$param_label[param_labels$param == parameter])

  clim_comb1$date2 <- as.Date(clim_comb1$date, format = c("%Y-%m-%d"))

  clim_comb2 <- clim_comb1 |> filter(param %in% parameter) |> filter(year %in% years)

  year_len <- length(unique(clim_comb2$year))
  mon_len <- length(unique(clim_comb2$month))

  break_len <- if(year_len == 1){"1 month"
  } else if(year_len  %in% c(2, 3, 4) & mon_len <= 6){"2 months"
  } else if(year_len == 2 & mon_len > 6){"4 months"
    #} else if(year_len > 4 & mon_len <= 6){"6 months"
  } else if(year_len %in% c(4, 5, 6)){"1 year"
  } else if(year_len > 6 & year_len < 20){"2 years"
  } else if(year_len >= 20){"5 years"
  } else {"6 months"}

  date_format <- ifelse(break_len %in% c("1 year", "2 years", "5 years"), "%Y",
                        ifelse(break_len %in% c("2 months", "4 months"), "%b/%Y",
                                                "%b"))
  datebreaks <- seq(min(clim_comb2$date2), max(clim_comb2$date2) + 30, by = break_len)

  ylabel <- ifelse(parameter == "ppt", "Precipitation Anomaly (mm)", "Temperature Anomaly (C)")
  xlabel <- if(year_len == 1){paste0("Year: ", years)} else {NULL}
  avglabel <- ifelse(averages == "norm19cent", "Baseline: 1901 - 2000", "Baseline: 1991 - 2020")

anomplot <-
  ggplot(clim_comb2, aes(x = date2, y = anom,
                         fill = factor(anom_type),
                         color = factor(anom_type),
                         group = factor(anom_type))) + theme_WQ() +
    # geom_area(stat = 'identity', alpha = 0.5) +
    geom_bar(stat = 'identity', alpha = 0.4, lwd = 0.05) +
    scale_color_manual(values = c("above" = "red", "below" =  "blue"),
                      labels = c("above" = "Above baseline",
                                 "below" = "Below baseline"),
                      aesthetics = c("color", "fill"),
                      name = NULL) +
    geom_hline(aes(yintercept = 0, linetype = "Baseline")) +
    scale_linetype_manual(values = c("Baseline" = "solid"),
                          labels = c("Baseline" = avglabel),
                          name = NULL) +
    # facets
    {if(facetparam == FALSE & facetpark == TRUE){facet_wrap(~park_facet)}} +
    {if(facetparam == TRUE & facetpark == FALSE){facet_wrap(~param_label, scales = facet_y)}} +
    {if(facetparam == TRUE & facetpark == TRUE){facet_wrap(~park_facet + param_label)}} +
    # labels/themes
    labs(x = xlabel, y = ylabel) +
    scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    guides(linetype = guide_legend(order = 2),
           fill = guide_legend(order = 1),
           color = guide_legend(order = 1))

   return(#suppressWarnings(
    anomplot)#)
}


