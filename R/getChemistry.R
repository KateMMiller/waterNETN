#' @title getChemistry: query NETN water chemistry data
#'
#' @description Queries NETN water chemistry data by site, event, and parameter.
#'
#' @importFrom dplyr filter full_join left_join mutate select
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_longer pivot_wider
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
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @param parameter Specify the chemical parameter(s) to return. Note if additional parameters are added to the Chemistry view, there will be additional
#' to the views, they will be added as accepted values in this function. Current accepted values are:.
#' c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA", "ChlA_ugL", "Cl", "Cl_ueqL",
#' "DOC", "DOC_mgL", "NH3", "NH3_mgL", "NO2", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
#' "NO3", "NO3_ueqL", "pH_Lab", "PO4", "PO4_ugL", "SO4", "SO4_ueqL", "TN", "TN_mgL",
#' "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP", "TP_ugL")
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
#' @param sample_depth Filter on sample depth. If "all" (Default), returns all sample depths. If "surface",
#' only returns the record with the smallest sample depth (i.e., the surface). NOT ENABLED YET.
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
#' # get events for all sites in MABI from 2021-2023
#' mabi <- getChemistry(park = "MABI", years = 2021:2023)
#'
#' # get events for SARA sites sampled in 2019 and 2023
#' sara <- getChemistry(park = "SARA", years = c(2019, 2023))
#'
#' # get events for MIMA and SAIR
#' ma_parks <- getChemistry(park = c("SAIR", "MIMA"))
#'
#' # get info for all ACAD lakes sampled in April
#' ACAD_lake4<- getChemistry(park = 'ACAD', site_type = 'lake', months = 4)
#'
#' # get site info for 2 streams in MORR with full output
#' morr_sites <- getChemistry(site = c("MORRSA", "MORRSB"), output = 'verbose')
#' }
#' @export

getChemistry <- function(park = "all", site = "all",
                     site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"),
                     months = 5:10,
                     QC_type = c("ENV", "all", "BLANK", "LABREP", "REP", "DUP"),
                     sample_type = c("all", "G", "C"),
                     sample_depth = c("all", "surface"),
                     parameter = "all", include_censored = FALSE,
                     output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  output <- match.arg(output)
  stopifnot(class(include_censored) == "logical")
  QC_type <- match.arg(QC_type, several.ok = TRUE)
  sample_type <- match.arg(sample_type, several.ok = TRUE)

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({chem <- get("Chemistry_Data", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  options(scipen = 10)
  # Add year, month and day of year column to dataset
  chem$year <- as.numeric(substr(chem$EventDate, 1, 4))
  chem$month <- as.numeric(substr(chem$EventDate, 6, 7))
  chem$doy <- as.numeric(strftime(chem$EventDate, format = "%j"))
  chem$IsEventCUI <- as.logical(chem$IsEventCUI)

  # All the columns with NA as blanks are reading in as chr instead of numeric with NA_real_
  # Will fix parameters after reshaping to long.
  chem$SampleDepth_m <- suppressWarnings(as.numeric(chem$SampleDepth_m))

  # Make parameters long, so more efficient and easier to filter.
  # Need to end up with a column for each: parameter, value, flag, Method,
  # I don't want to hard code the pivot, in case a new parameter is ever added, so
  # selecting the columns based on what I don't want to include in the pivot.
  keep_cols <- c("GroupCode", "GroupName", "UnitCode", "UnitName", "SubUnitCode",
                 "SubUnitName", "SiteCode", "SiteName", "EventDate",
                 "year", "month", "doy", "EventCode", "QCtype", "LabCode",
                 "SampleTime", "SampleDepth_m", "SampleType", "Comments", "IsEventCUI")
sort(unique(getSites()$SiteCode))
  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = site_type))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = site_type,
                         years = years, months = months, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode)

  chem2 <- chem |> filter(SiteCode %in% sites)
  chem3 <- left_join(evs, chem2, by = c("SiteCode", "EventDate", "EventCode"))

  chem_long <- chem3 |> pivot_longer(cols = !all_of(keep_cols),
                                    names_to = 'param', values_to = 'value') |>
    filter(value != "NA") |>
    mutate(Flag = ifelse(grepl("Flag", param), "Flag", NA_character_),
           Lab = ifelse(grepl("pH_Lab_Method", param), "pH_Lab_Method", NA_character_))

  chem_long_param <- chem_long |>
    filter(!Flag %in% "Flag") |> filter(!Lab %in% "pH_Lab_Method") |>
    select(-Flag, -Lab)

  chem_long_flag <- chem_long |> filter(Flag == "Flag") |>
    mutate(param = gsub("Flag", "", param)) |>
    select(-Lab, -Flag, flag = value)

  chem_long_lab <- chem_long |> filter(Lab == "pH_Lab_Method") |>
    mutate(param = gsub("_Method", "", param)) |>
    select(-Lab, -Flag, lab_method = value)

  cols <- intersect(names(chem_long_param), names(chem_long_flag))
  cols2 <- intersect(cols, names(chem_long_lab))

  chem_list <- list(chem_long_param, chem_long_flag, chem_long_lab)
  chem_comb <- purrr::reduce(chem_list, full_join, by = cols2)
  chem_comb$value <- suppressWarnings(as.numeric(chem_comb$value))

  if(include_censored == TRUE){
  chem_comb$value <- suppressWarnings(
    ifelse(is.na(chem_comb$value),
           as.numeric(gsub("<MRL |<MDL |, color interference|>UQL |E, <MRL |E, >UQL |ND, Cl interference|IS",
                           "", chem_comb$flag)),
           chem_comb$value))
  }

  # check that parameter matches param_list
  param_list <- sort(unique(chem_comb$param))
  parameter <- match.arg(parameter, several.ok = TRUE, c("all", param_list))

  # filters for params, sampletype, qctype
  chem_comb2 <-
  if(any(parameter == "all")){chem_comb
  } else {filter(chem_comb, param %in% parameter)}

  chem_comb3 <-
    if(any(sample_type == "all")){chem_comb2
    } else {filter(chem_comb2, SampleType %in% sample_type)}

  chem_comb4 <-
    if(any(QC_type == "all")){chem_comb3
    } else {filter(chem_comb3, QCtype %in% QC_type)}

  chem_comb5 <-
  if(output == "short"){chem_comb4[,c("SiteCode", "UnitCode", "SubUnitCode", "EventDate",
                                     "year", "month", "doy", "QCtype", "SampleType",
                                     "SampleDepth_m", "param", "value", "flag",
                                     "lab_method", "Comments")]
    } else {chem_comb4}

  if(nrow(chem_comb5) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(chem_comb5))

  }



