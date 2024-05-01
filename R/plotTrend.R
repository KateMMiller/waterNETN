#' @title plotTrend: Plots smoothed trend
#'
#' @importFrom dplyr select
#' @import ggplot2
#'
#' @description This function produces a smoothed trend plot filtered on park, site, year, month, and parameter.
#' Works with both lab chemistry data and Sonde in situ data. If multiple sites are specified, they will be plotted
#' on the same figure. If multiple parameters are specified, they will be plotted on separate figures.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
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
#' @param parameter Specify the parameter(s) to return. Current accepted values are:.
#' chemistry: c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA", "ChlA_ugL", "Cl", "Cl_ueqL",
#' "DOC", "DOC_mgL", "NH3", "NH3_mgL", "NO2", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
#' "NO3", "NO3_ueqL", "pH_Lab", "PO4", "PO4_ugL", "SO4", "SO4_ueqL", "TN", "TN_mgL",
#' "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP", "TP_ugL")
#' sonde: c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#' "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg")
#'
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the MDL/MRL/UQL values in the parameter flags. If the Flag column is not NA, that indicates
#' the value is a censored value. If FALSE (Default), only non-censored values are returned in the value column.
#'
#' @param sample_depth Filter on sample depth. If "all" (Default), returns all sample depths. If "surface",
#' only returns the median value of samples collected <= 2m from the surface. SampleDepth_m is also the median
#' sample depth of samples collected within 2m of the surface.
#'
#' @param smooth Logical. If TRUE (Default), will plot a loess smoothed line. If FALSE, will plot actual line.
#'
#' @param ... Additional arguments relevant to \code{getChemistry()} or \code{getSondeInSitu()}
#'
#' @examples
#' \dontrun{
#'
#' ++++ ADD EXAMPLES +++++
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
                      months = 5:10,
                      parameter = NA, include_censored = FALSE,
                      sample_depth = c("all", "surface"),
                      smooth = c(TRUE, FALSE), ...){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(include_censored) == "logical")
  sample_depth <- match.arg(sample_depth)
  stopifnot(class(smooth) == "logical")

  chem <- c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA", "ChlA_ugL", "Cl", "Cl_ueqL",
            "DOC", "DOC_mgL", "NH3", "NH3_mgL", "NO2", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
            "NO3", "NO3_ueqL", "pH_Lab", "PO4", "PO4_ugL", "SO4", "SO4_ueqL", "TN", "TN_mgL",
            "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP", "TP_ugL")

  sonde <- c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
             "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg")

  data_type <- if(any(parameter %in% chem) & any(parameter %in% sonde)){"both"
  } else if(all(parameter %in% chem)){"chem"
  } else if(all(parameter %in% sonde)){"sonde"
        } else {stop("At least one specified parameter is not an accepted value.")}

  par_chem <- parameter[parameter %in% chem]
  par_sonde <- parameter[parameter %in% sonde]

  wdat <-
  if(data_type == 'both'){
    rbind(getChemistry(park = park, site = site, site_type = site_type,
                       years = years, parameter = par_chem, ...) |>
            select(SiteCode, UnitCode, EventDate, year, month, doy, param, value),
          getSondeInSitu(park = park, site = site, site_type = site_type,
                         years = years, parameter = par_sonde, ...) |>
            select(SiteCode, UnitCode, EventDate, year, month, doy, param, value))
  }

  wdat$param_label <- ifelse(grepl("_", wdat$param),
                             paste0(gsub("_", " (", wdat$param), ")"),
                             paste0(wdat$param)
  )

  smplot <-
    ggplot(wdat, aes(x = EventDate, y = value, group = SiteCode,
                     color = SiteCode, fill = SiteCode)) +
      {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F) } +
      {if(smooth == FALSE) geom_line()} +
      {if(length(unique(wdat$param_label))>1) facet_wrap(~param_label, scales = 'free')} +
      theme_WQ() +
      scale_color_viridis_d() +
      labs(x = "Year", y = "value")

 return(smplot)
}


