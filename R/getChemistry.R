#' @include getSites.R
#' @include getEvents.R
#'
#' @title getChemistry: query NETN water chemistry data
#'
#' @description Queries NETN water chemistry data by site, event, and parameter.
#'
#' @importFrom dplyr all_of filter full_join group_by inner_join left_join mutate select summarize
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_longer pivot_wider
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
#' @param event_type Select the event type. Options available are below Can only choose one option.
#' \describe{
#' \item{"all"}{All possible sampling events.}
#' \item{"VS"}{Default. NETN Vital Signs monitoring events, which includes Projects named 'NETN_LS' and 'NETN+ACID'.}
#' \item{"acid"}{Acidification monitoring events in Acadia.}
#' \item{"misc"}{Miscellaneous sampling events.}
#' }
#'
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @param parameter Specify the chemical parameter(s) to return. Note if additional parameters are added to the Chemistry view, there will be additional
#' to the views, they will be added as accepted values in this function. Current accepted values are:.
#' c("ANC_ueqL", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL", "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3_mgL",
#' ""NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL", "TN_mgL", "TotDissN_mgL", "TotDissP_ugL", "TP_ugL")
#'
#' @param sample_type Filter on sample type.
#' \describe{
#' \item{"all"}{Include all sample types}
#' \item{"C"}{Include Core samples only}
#' \item{"G"}{Include Grab samples only}
#' }
#'
#' @param QC_type Specify QC type to return.
#' \describe{
#' \item{"all"}{Include all QC types.}
#' \item{"ENV"}{Environmental. Default. Indicates a real non-QC sample.}
#' \item{"BLANK"}{Field blank}
#' \item{"DUP"}{Duplicate}
#' \item{"LABREP"}{Lab replicate}
#' \item{"REP"}{Field replicate}
#' }
#'
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the MDL/MRL/UQL values in the parameter flags. If the Flag column is not NA, that indicates
#' the value is a censored value. If FALSE (Default), only non-censored values are returned in the value column.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @return Data frame of chemistry data in long form.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # get chemistry data for all sites and all parameters in MABI from 2021-2023
#' mabi <- getChemistry(park = "MABI", years = 2021:2023)
#'
#' # get all N params in MIMA from 2006-2023
#' n_params <- c("NH3_mgL", "NO2_mgL", "NO2+NO3_mgL", "NO3_ueqL", "TN_mgL")
#' period <- 2006:2023
#' mima_n <- getChemistry(park = "MIMA", years = period, parameter = n_params)
#'
#' # get lab pH for all sites in MIMA and SAIR
#' ma_parks <- getChemistry(park = c("SAIR", "MIMA"), param = "pH_Lab")
#'
#' # get chemistry for all ACAD lakes sampled in April
#' ACAD_lake4<- getChemistry(park = 'ACAD', site_type = 'lake', months = 4)
#'
#' # get ANC for lower NETN parks from May to Oct
#' lnetn <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")
#' anc <- getChemistry(park = lnetn, param = "ANC", months = 5:10)
#' }
#' @export

getChemistry <- function(park = "all", site = "all",
                         site_type = c("all", "lake", "stream"),
                         event_type = "VS",
                         years = 2006:format(Sys.Date(), "%Y"),
                         months = 5:10, active = TRUE,
                         QC_type = "ENV",
                         sample_type = c("all", "G", "C"),
                         parameter = "all", include_censored = FALSE,
                         output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  event_type <- match.arg(event_type, c("all", "VS", "acid", "misc"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  output <- match.arg(output)
  stopifnot(class(include_censored) == "logical")
  QC_type <- match.arg(QC_type, several.ok = TRUE,
                       c("ENV", "all", "BLANK", "LABREP", "REP", "DUP"))
  sample_type <- match.arg(sample_type, several.ok = TRUE)

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({chem <- get("Chemistry_Data_Long", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  options(scipen = 10) # prevent scientific notation

  # Fix data types (will wait on the params until long)
  # character fixes
  chr_cols <- c("SubUnitCode", "SubUnitName", "SampleType", "LabCode", "QCtype", "ValueFlag",
                "ValueUnit", "ValueDetectionCondition")
  chem[,chr_cols][chem[,chr_cols] == "NA"] <- NA_character_

  # numeric fixes
  chem$SampleDepth_m <-  as.numeric(gsub("NA", NA_real_, chem$SampleDepth_m))
  chem$Value <- gsub(",", "", chem$Value)
  chem$Value <- as.numeric(gsub("NA", NA_real_, chem$Value))

  # logic fixes
  chem$IsEventCUI <- as.logical(chem$IsEventCUI)

  # Add year, month and day of year column to dataset
  chem$year <- as.numeric(substr(chem$EventDate, 1, 4))
  chem$month <- as.numeric(substr(chem$EventDate, 6, 7))
  chem$doy <- as.numeric(strftime(chem$EventDate, format = "%j"))

  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = site_type, active = active))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = site_type,
                         event_type = event_type, active = active,
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode, Project)

  chem2 <- chem |> filter(SiteCode %in% sites)
  chem3 <- inner_join(evs, chem2, by = c("SiteCode", "EventDate", "EventCode"))

  chem3$censored <- ifelse(is.na(chem3$ValueDetectionCondition) | chem3$ValueDetectionCondition == "Not Detected",
                           TRUE, FALSE)

  if(include_censored == TRUE){
    chem3$Value <- suppressWarnings(
      ifelse(chem3$censored == TRUE,
             as.numeric(gsub("<MRL |<MDL |, color interference|>UQL |E, <MRL |E, >UQL |ND, Cl interference|IS",
                             "", chem3$ValueFlag)),
             chem3$Value))
  }

  # check that parameter matches param_list
  param_list <- sort(unique(chem3$Parameter))
  parameter <- match.arg(parameter, several.ok = TRUE, c("all", param_list))

  # filters for params, sampletype, qctype
  chem4 <-
  if(any(parameter == "all")){chem3
  } else {filter(chem3, Parameter %in% parameter)}

  chem5 <-
    if(any(sample_type == "all")){chem4
    } else {filter(chem4, SampleType %in% sample_type)}

  chem6 <-
    if(any(QC_type == "all")){chem5
    } else {filter(chem5, QCtype %in% QC_type)}

  # add date/time stamp for rLakeAnalyzer and LakeMonitoR
  chem6$datetime <- as.POSIXct(paste(chem6$EventDate, chem6$SampleTime),
                                    format = "%Y-%m-%d %H:%M:%S")

  chem7 <-
  if(output == "short"){chem6[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode", "EventDate","EventCode", "Project",
                                 "year", "month", "doy", "datetime", "QCtype", "SampleType", "SampleTime",
                                 "SampleDepth_m", "Parameter", "Value", "ValueFlag", "ValueUnit",
                                 "ValueDetectionCondition", "censored", "LabCode")]
  } else {chem6}

  if(nrow(chem7) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(chem7))

  }



