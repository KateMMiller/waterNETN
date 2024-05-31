#' @include getSites.R
#' @include getClimDrought.R
#'
#' @title plotClimDrought: Plots weekly drought based on county-level drought index
#'
#' @description This function plots weekly drought index at the county level for each specified park or
#' weather station nearest to a specified park. Resulting plot shows the percent of county area in 5 levels
#' of drought, with D0 = Abnormally Dry, D1 = Moderate Drought, D2 = Severe Drought, D3 = Extreme Drought,
#' and D4 = Exceptional Drought. Drought designations come from the U.S. Drought Monitor. If multiple parks are
#' specified, results will be faceted with a separate plot for each park. Where multiple counties occur in
#' a park, data can be faceted by county if dom_county = FALSE. To only plot predominant county, specify
#' dom_county = TRUE (default).
#'
#' @importFrom dplyr case_when filter left_join
#' @importFrom tidyr pivot_longer
#' @import ggplot2
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
#' @param years Vector of years to download drought index for, will start with the first Tuesday of the year
#' and end with the last Tuesday of the year specified. Defaults to current year.
#'
#' @param months Vector of months to download drought index for. Default = 1:12.
#'
#' @param weather_station Logical. If TRUE, will return county-level data for coordinates of nearest weather station
#' to a park. If FALSE (default), returns county-level drought data for water monitoring sites. In most cases, the
#' results are the same.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites
#' that have been monitored.
#'
#' @param dom_county Logical. If TRUE (Default) only plots predominant county if park covers multiple counties.
#' If FALSE, facets on county.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none". Other
#' options are "top", "bottom", "left", "right" (default).
#'
#' @param plot_title Logical. If TRUE (default) prints site name at top of figure. If FALSE,
#' does not print site name. Only enabled when one site is selected.
#'
#' @return Data frame of weather station daily climate data for each specified site.
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # Plot drought info for MABI and SAGA for 2020 to 2024
#' plotClimDrought(park = c("MABI", "SAGA"), years = 2020:2024)
#'
#' # Plot drought info for both MORR counties for 2023
#' plotClimDrought(park = "MORR", years = 2023, dom_county = FALSE)
#'
#' # Plot drought info for ACAD counties in 2024
#' plotClimDrought(park = "ACAD", years = 2024, dom_county = FALSE)
#'}
#'
#' @export

plotClimDrought <- function(park = "all",
                            years = format(Sys.Date(), format = "%Y"),
                            months = 1:12, dom_county = TRUE,
                            legend_position = 'right', plot_title = TRUE,
                            weather_station = FALSE){

  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1980)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  stopifnot(class(plot_title) == "logical")
  stopifnot(class(dom_county) == "logical")
  stopifnot(class(weather_station) == "logical")
  if(!is.na(week_start)){
    date_check <- as.Date(week_start, format = "%m/%d/%Y")
    if(is.na(date_check)){stop("Wrong date format specified. Must be formatted as 'mm/dd/yyyy'.")}
  }

  # Need to include sites to get fips code
  sites <- force(unique(getSites(park = park))[, c("UnitCode", "UnitName")]) |> unique()

  ddata <- getClimDrought(park = park, years = years,
                          active = active, weather_station = weather_station) |>
    mutate(dom_county = case_when(UnitCode == "ACAD" & County == "Knox County" ~ FALSE,
                                  UnitCode == "MORR" & County == "Somerset County" ~ FALSE,
                                  TRUE ~ TRUE)) |> unique()

  # Take only dominant county
  ddata2 <- if(dom_county == TRUE){filter(ddata, dom_county == TRUE)} else {ddata}

  ddata_long <- ddata2 |> pivot_longer(cols = c(D0, D1, D2, D3, D4),
                                       names_to = "Drought_Level", values_to = "Pct_Area")

  ddata_long$Date <- as.Date(ddata_long$ValidStart, format = "%Y-%m-%d")
  ddata_long$year <- format(as.Date(ddata_long$Date, format = "%Y-%m-%d"), "%Y")
  ddata_long$month <- as.numeric(format(as.Date(ddata_long$Date, format = "%Y-%m-%d"), "%m"))

  ddata3 <- left_join(ddata_long, sites, by = "UnitCode") |>
    filter(month %in% months) |>
    mutate(drought_legend = case_when(Drought_Level == "D0" ~ "Abnormally Dry",
                                      Drought_Level == "D1" ~ "Moderate Drought",
                                      Drought_Level == "D2" ~ "Severe Drought",
                                      Drought_Level == "D3" ~ "Extreme Drought",
                                      Drought_Level == "D4" ~ "Exceptional Drought"))


  year_len <- length(unique(ddata3$year))
  mon_len <- length(unique(ddata3$month))

  break_len <- if(year_len == 1){"1 month"
  } else if(year_len  %in% c(2, 3, 4) & mon_len <= 6){"2 months"
  } else if(year_len == 2 & mon_len > 6){"4 months"
    #} else if(year_len > 4 & mon_len <= 6){"6 months"
  } else if(year_len %in% c(4, 5, 6)){"1 year"
  } else if(year_len > 6){"2 years"
  } else {"6 months"}

  date_format <- ifelse(break_len %in% c("1 year", "2 years"), "%Y", "%m/%d/%y")
  datebreaks <- seq(min(ddata3$Date), max(ddata3$Date) + 30, by = break_len)

  dplot <-
    ggplot(ddata3, aes(x = Date, y = Pct_Area, fill = drought_legend)) +
    # layers
    geom_area() +
    geom_line(aes(y = DSCI, linetype = "Cumulative Index"), linewidth = 0.5) +
    # axis format
    scale_x_date(breaks = datebreaks, labels = scales::label_date(date_format)) +
    # layer formattting
    scale_linetype_manual(values = "dashed", name = "Drought Severity") +
    scale_fill_manual(values = c("#FFF000", "#FCD37F", "#FFAA00", "#E60000", "#730000"), name = "Drought Level") +
    # theme and labels
    theme_WQ() +
    theme(legend.position = legend_position,
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(y = "County Area in Drought", x = NULL) +
    {if(length(sites$UnitCode) > 1 & length(unique(ddata3$County)) == 1){facet_wrap(~UnitCode)}} + #change to county
    {if(length(sites$UnitCode) == 1 & length(unique(ddata3$County)) > 1){facet_wrap(~County)}} +
    {if(length(sites$UnitCode) > 1 & length(unique(ddata3$County)) > 1){facet_wrap(~UnitCode + County)}} +
    guides(fill = guide_legend(order = 1))

  return(dplot)

}
