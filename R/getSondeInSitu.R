#' @include getSites.R
#' @include getEvents.R
#'
#' @title getSondeInSitu: query NETN water chemistry Sonde data
#'
#' @description Queries NETN water chemistry data with the Sonde in the field by site, event, and parameter.
#' If pulling all sites, parameters, years, etc., function may take a second or two to run.
#'
#' @importFrom dplyr filter first full_join group_by left_join mutate select summarize ungroup
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
#' @param years Numeric. Years to query. Accepted values start at 2006.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 5 and 10, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param parameter Specify the chemical parameter(s) to return. Note if additional parameters are added to the Chemistry view, there will be additional
#' to the views, they will be added as accepted values in this function. Current accepted values are:.
#' c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#' "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg")
#'
#' @param QC_type Specify QC type to return. Must be quoted.
#' \describe{
#' \item{"all"}{Include all QC types.}
#' \item{"0"}{Environmental. Default. Indicates a real non-QC sample.}
#' \item{"900"}{QC Replicate}
#' \item{"899"}{Pre-deployment 100 percent sat check}
#' \item{"999"}{Post-deployment 100 percent sat check}
#' }
#'
#' @param sample_depth Filter on sample depth. If "all", returns all sample depths. If "surface" (Default),
#' only returns the median value of samples collected <= 2m from the surface. SampleDepth_m is also the median
#' sample depth of samples collected within 2m of the surface.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @return Data frame of Sonde chemistry data in long form.
#'
#' @examples
#' \dontrun{
#' importData()
#'
#' # get Sonde data for all sites and parameters in MABI from 2021-2023 for non-QAQC samples
#' mabi <- getSondeInSitu(park = "MABI", years = 2021:2023)
#'
#' # get data for all DO parameters in MIMA from 2006-2023 for non-QAQC samples
#' params <- c("DOsat_pct", "DOsatLoc_pct", "DO_mgL")
#' period <- 2006:2023
#' mima_n <- getSondeInSitu(park = "MIMA", years = period, parameter = params)
#'
#' # get Temp data for surface measurements only in Jordan Pond in ACAD for non-QAQC samples
#' ACAD_lake <- getSondeInSitu(site = 'ACJORD', parameter = "Temp_C", sample_depth = "surface")
#'
#' # get pH for lower NETN parks from May to Oct for QAQC and non-QAQC samples
#' lnetn <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")
#' lnetn_ph <- getSondeInSitu(park = lnetn, param = "pH", months = 5:10, QC_type = 'all')
#' }
#' @export

getSondeInSitu <- function(park = "all", site = "all",
                     site_type = c("all", "lake", "stream"),
                     years = 2006:format(Sys.Date(), "%Y"),
                     months = 5:10, active = TRUE,
                     parameter = "all",
                     QC_type = "0",
                     sample_depth = "surface",
                     output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  site_type <- match.arg(site_type)
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2006)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  output <- match.arg(output)
  QC_type <- match.arg(QC_type, several.ok = TRUE,
                       c("0", "all", "900", "899", "999"))
  sample_depth <- match.arg(sample_depth, c("surface", "all"))

  parameter <- match.arg(parameter,
                         c("all", "Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct",
                           "DO_mgL", "pH", "pHmV", "Turbidity_FNU", "ChlA_RFU",
                           "ChlA_ugL", "BP_mmHg"), several.ok = TRUE)

  qccode <- ifelse(unique(QC_type) == "all", c("0", "all", "900", "899", "999"),
                   unique(QC_type))

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({sonde <- get("Sonde_InSitu_Data", envir = env)},
            error = function(e){stop("Water views not found. Please import data.")}
  )

  options(scipen = 10) # prevent scientific notation
  # Add year, month and day of year column to dataset and fix data types
  # will fix the parameter types later after they're pivoted long

  # char fixes
  chr_cols <- c("SubUnitCode", "SubUnitName", "XYAccuracy", "QCType_Code", "QCType_Value",
                "SondeType", "WQInSitu_Flag", "WQFlag_Comments")
  sonde[,chr_cols][sonde[,chr_cols] == "NA"] <- NA_character_

  # numeric fixes
  num_cols <- c("SondeLatitude", "SondeLongitude", "Rep", "Depth_m")
  sonde[,num_cols][sonde[,num_cols] == "NA"] <- NA_real_
  sonde[,num_cols] <- apply(sonde[,num_cols], 2, function(x) as.numeric(x))

  # logic fixes
  sonde$IsEventCUI <- as.logical(sonde$IsEventCUI)

  # Add year, month and day of year column to dataset
  sonde$year <- as.numeric(substr(sonde$EventDate, 1, 4))
  sonde$month <- as.numeric(substr(sonde$EventDate, 6, 7))
  sonde$doy <- as.numeric(strftime(sonde$EventDate, format = "%j"))

  # Make parameters long, so more efficient and easier to filter.
  # Need to end up with a column for each: parameter, value, flag, Method,
  # I don't want to hard code the pivot, in case a new parameter is ever added, so
  # selecting the columns based on what I don't want to include in the pivot.
  keep_cols <- c("GroupCode", "GroupName", "UnitCode", "UnitName", "SubUnitCode",
                 "SubUnitName", "SiteCode", "SiteName", "SiteType", "EventDate", "EventCode",
                 "year", "month", "doy", "MeasurementTime", "SondeLatitude", "SondeLongitude",
                 "Datum", "XYAccuracy", "QCType_Code", "QCType_Value", "Rep",
                 "SondeType", "WQInSitu_Flag", "WQFlag_Comments", "IsEventCUI", "Depth_m")

  param_cols <- setdiff(names(sonde), keep_cols)
  # Pivoting numeric and character fields in one step, so converting all non-keep to character.
  sonde[,param_cols] <- apply(sonde[,param_cols], 2, as.character)

  #sort(unique(getSites()$SiteCode))
  # Filter by site, years, and months to make data set small
  sites <- force(getSites(park = park, site = site, site_type = site_type, active = active))$SiteCode
  evs <- force(getEvents(park = park, site = site, site_type = site_type,
                         years = years, months = months, active = active, output = 'verbose')) |>
    select(SiteCode, SiteType, EventDate, EventCode)

  sonde2 <- sonde |> filter(SiteCode %in% sites)
  sonde3 <- left_join(evs, sonde2, by = c("SiteCode", "EventDate", "EventCode"))
  #head(sonde3)

  sonde_long <- sonde3 |> pivot_longer(cols = !all_of(keep_cols),
                                    names_to = 'param', values_to = 'value') |>
    filter(value != "NA")

  # find special characters forcing chr instead of num
  # values = sort(unique(sonde_long$value))
  # values[which(!grepl('^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$',values))]

  sonde_long$value <- gsub(",", "", sonde_long$value)
  sonde_long$value <- as.numeric(gsub("NA", NA_real_, sonde_long$value))

  # filters for params, sampletype, qctype
  sonde4 <-
  if(any(unique(parameter) == "all")){sonde_long
  } else {filter(sonde_long, param %in% parameter)}

  sonde5 <- filter(sonde4, QCType_Code %in% qccode)

  # Filter on surface vs. epilimnion. For sample_depth = surface, take median for all depths <=2m.
  # For depth = "all", include all measurements without aggregating.
  sonde6 <- if(sample_depth == 'surface'){
    sonde5 |> filter(Depth_m <= 2) |>
      group_by( SiteCode, SiteType, EventDate, EventCode, GroupCode, GroupName,
                UnitCode, UnitName, SubUnitCode, SubUnitName, SiteName,
                Datum, XYAccuracy, QCType_Code,
                QCType_Value, Rep,SondeType, WQInSitu_Flag, WQFlag_Comments, IsEventCUI,
                year, month, doy, param) |>
      summarize(Depth_m = median(Depth_m, na.rm = T),
                value = median(value, na.rm = T),
                SondeLatitude = first(SondeLatitude),
                SondeLongitude = first(SondeLongitude),
                MeasurementTime= first(MeasurementTime),
                .groups = 'drop') |> ungroup()
  } else {sonde5}

  # add date/time stamp for rLakeAnalyzer and LakeMonitoR
  # sonde6$datetime <- as.POSIXct(paste(sonde6$EventDate, sonde6$MeasurementTime),
  #                      format = "%Y-%m-%d %H:%M:%S")

  sonde6$datetime <- as.POSIXct(paste(sonde6$EventDate, "12:00:00"),
                                format = "%Y-%m-%d %H:%M:%S")
  sonde7 <-
  if(output == "short"){sonde6[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode", "EventDate",
                                  "year", "month", "doy", "datetime", "QCType_Code", "QCType_Value",
                                  "Rep", "SondeType", "Depth_m", "param", "value",
                                  "WQInSitu_Flag", "WQFlag_Comments")]
  } else {sonde6}

  if(nrow(sonde7) == 0){
    stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(sonde7))

  }



