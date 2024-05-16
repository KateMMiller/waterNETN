#' @include getSites.R
#' @include theme_WQ.R
#' @include getClimWStat.R
#'
#' @title plotPrecipDischarge: Plot precipitation and discharge
#'
#' @importFrom dplyr filter select
#' @import ggplot2
#'
#' @description This function produces a plot with dual y-axes, one for precipitation an done for discharge.
#' The x-axis is date. If multiple sites and/or years are specified, they will be plotted as separate figures.
#' Note that lines aren't plotted for discharge because discharge likely varies a lot between samples.
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
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param colors Change colors of precipitation bar plot and discharge point and line plot. Default is c('#257EF6', 'black'), which are dark blue for precipitation,
#' and black for discharge. Y axes are color coded the same.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param ... Additional arguments relevant to \code{getChemistry()} or \code{getSondeInSitu()}
#'
#' @examples
#' \dontrun{
#'
#' # Plot Discharge for Mill Brook in MIMA for past 5 years using default colors.
#' plotPrecipDischarge(site = c("MIMASA"), years = 2019:2023)
#'
#' # Plot Discharge for Aunt Betty Inlet and Kebo Stream for 2022 using different colors.
#' # Note that this can be slow because has to download precip. data from NADP.
#' # LNETN parks download from a faster web service.
#' plotPrecipDischarge(site = c("ACABIN", "ACKEBO"), years = 2022, colors = c("cornflowerblue", "orange"))
#'
#'}
#'
#' @return Returns a ggplot object of specified climate trends
#'
#' @export
#'
plotPrecipDischarge <- function(park = "all", site = "all",
                                years = 2006:format(Sys.Date(), "%Y"),
                                #layers = c("points", "lines"),
                                months = 5:10, active = TRUE,
                                colors = c("#257EF6", "black"),
                                legend_position = 'none', ...){

  #-- Error handling --
  # check for required packages for certain arguments
  if(!requireNamespace("scales", quietly = TRUE)){
    stop("Package 'scales' needed for this function to work. Please install it.", call. = FALSE)
  }
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  stopifnot(length(colors) == 2)
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))

  #-- Compile data for plotting --
  disch <- getDischarge(park = park, site = site, active = active, years = years, months = months) |>
    select(SiteCode, SiteName, EventDate, year, month, doy, Discharge_cfs)

  disch$Date2 <- as.Date(disch$EventDate, format = c("%Y-%m-%d"))

  # Use years in disch to drop years a site isn't sampled in precip download.
  sample_years <- sort(unique(disch$year))

  precip <- getClimWStat(park = park, site = site, site_type = 'stream', active = active,
                         years = sample_years) |> filter(month %in% months)

  precip$Date2 <- as.Date(precip$Date, format = c("%Y-%m-%d"))

  comb <- left_join(precip, disch, by = c("SiteCode", "SiteName", "year", "doy", "month", "Date2")) |>
    filter(year %in% sample_years)

  # Have to rescale so precip and discharge show up on same plot
  scale = range(precip$ws_pcpmm, na.rm = T)[2]/range(disch$Discharge_cfs, na.rm = T)[2]

  # Control x-axis breaks
  datebreaks <- seq(min(comb$Date2), max(comb$Date2) + 30, by = "1 month")

  facet_site <- ifelse(length(unique(comb$SiteCode)) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(unique(comb$year)) > 1, TRUE, FALSE)


dp_plot <-
    ggplot(comb, aes(x = Date2, y = ws_pcpmm, group = SiteName)) + theme_WQ() +
      # layers y-left
      geom_bar(color = colors[1], fill = colors[1], stat = 'identity', alpha = 0.5) +
      # layers y-right
      # {if(any(layers %in% "points"))
        geom_point(data = disch, aes(x = Date2, y = Discharge_cfs * scale),
                   color = colors[2], shape = "+", size = 6) +  #}+
      # {if(any(layers %in% "lines"))
      #   geom_line(data = disch, aes(x = Date2, y = Discharge_cfs * scale),
      #             color = colors[2], linewidth = 0.7)} +
      # create 2nd axis
      scale_y_continuous(name = "Daily Precip. (mm)",
                         sec.axis = sec_axis(~./scale, name = "Discharge (cfs)")) +
      # facets
      {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~SiteName + year, drop = T, scales = "free_x")} +
      {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~SiteName, drop = T)} +
      {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year, drop = T, scales = "free_x")} +
      # formatting
      theme(axis.text.y.left = element_text(color = colors[1]),
            axis.title.y.left = element_text(color = colors[1]),
            axis.text.y.right = element_text(color = colors[2]),
            axis.title.y.right = element_text(color = colors[2]),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      scale_x_date(breaks = datebreaks, labels = scales::label_date("%m/%d/%y")) +
      theme(legend.position = legend_position, legend.title = element_blank()) +
      labs(x = NULL)

  return(suppressWarnings(dp_plot))
}


