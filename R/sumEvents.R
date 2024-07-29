#' @include getEvents.R
#' @include getChemistry.R
#' @include getDischarge.R
#' @include getLightPen.R
#' @include getSecchi.R
#' @include getSondeInSitu.R
#' @include getWaterLevel.R
#'
#' @title sumEvents: summarize sample events
#'
#' @description Summarize the number of samples collected at each site by year, month, parameter, and
#' whether measurment was real or censored.
#'
#' @importFrom dplyr arrange filter first group_by last mutate select summarize
#' @importFrom tidyr pivot_wider
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
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites
#' that have been monitored.
#'
#' @param parameter Specify the parameter(s) to return. Current accepted values are:.
#' chemistry: c("ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
#' "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3_mgL", "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL",
#' "TN_mgL", "TotDissN_mgL", "TotDissP_ugL", "TP_ugL")
#' sonde: c("Temp_C", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#' "Turbidity_FNU", "ChlA_EXO_RFU", "ChlA_EXO_ugL", "BP_mmHg").
#' other: c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m").
#' Note that "all" is the default value.
#'
#' @return Data frame of summarize event info
#'
#'@examples
#' \dontrun{
#' importData()
#'
#' # get all events for ACAD for all years and active sites
#' acad_ev <- sumEvents(park = "ACAD")
#'
#' # get only lake events for ACAD for all years
#' acad_lk <- sumEvents(park = "ACAD", site_type = "lake")
#'
#' # get LNETN events only
#' lnetn <- sumEvents(park = "LNETN")
#'
#' }
#' @export

sumEvents <- function(park = "all", site = "all",
                      site_type = c("all", "lake", "stream"),
                      event_type = "VS",
                      years = 2006:format(Sys.Date(), "%Y"), active = TRUE,
                      months = 5:10){

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

  evs <- force(getEvents(park = park, site = site, site_type = site_type, years = years, event_type = event_type,
                         active = active, months = months)) |>
    select(UnitCode, SiteCode, SiteName, SiteType, year) |> unique() |>
    group_by(UnitCode, SiteCode, SiteName, SiteType) |>
    summarize(year_start = first(year),
              year_latest = last(year),
              num_years = sum(!is.na(year)),
              .groups = 'drop')

  wdat <-
    rbind(
        force(getChemistry(park = park, site = site, site_type = site_type, event_type = event_type,
                           include_censored = TRUE, years = years, months = months, parameter = "all")) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value, censored) |>
          mutate(param_type = "Lab chemistry"),
        force(getSondeInSitu(park = park, site = site, site_type = site_type, event_type = event_type,
                             years = years, months = months, parameter = "all")) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
          mutate(censored = FALSE,
                 param_type = "Sonde field meas."),
        tryCatch(
        force(getSecchi(park = park, site = site, event_type = event_type,
                        years = years, months = months, observer_type = 'first')) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
          mutate(censored = FALSE,
                 param_type = "Light penetration"),
        error = function(e){NULL}),
        tryCatch(
        force(getDischarge(park = park, site = site, event_type = event_type,
                           years = years, months = months)) |>
          mutate(Parameter = "Discharge_cfs", Value = Discharge_cfs) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
          mutate(censored = FALSE,
                 param_type = "Water quantity"),
        error = function(e){NULL}),
        tryCatch(
        force(getLightPen(park = park, site = site, event_type = event_type,
                          years = years, months = months)) |>
          mutate(Parameter = "PenetrationRatio", Value = PenetrationRatio) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
          mutate(censored = FALSE,
                 param_type = "Light penetration"),
        error = function(e){NULL}),
        tryCatch(
          force(getWaterLevel(park = park, site = site,
                            years = years, months = months)) |>
          mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value) |>
          mutate(censored = FALSE,
                 param_type = "Water quantity"),
          error = function(e){NULL})
    )

wdat$value_type <- ifelse(wdat$censored == TRUE, 'cens', "real")

wdat2 <- left_join(unique(evs[,c("UnitCode", "SiteCode", "SiteName", "SiteType",
                                 "year_start", "year_latest", "num_years")]),
                   wdat, by = c("UnitCode", "SiteCode", "SiteName"))
wdat2$mon <- factor(format(as.Date(wdat2$EventDate, format = c("%Y-%m-%d")), "%b"), levels = month.abb, ordered = T)
wdat2$mon <- wdat2$mon[,drop = T]

samp_tab <- wdat2 |> group_by(UnitCode, SiteCode, SiteName, SiteType, mon,
                              param_type, value_type, Parameter,
                              year_start, year_latest, num_years) |>
  summarize(num_samples = sum(!is.na(year)),
            .groups = "drop") |>
  pivot_wider(names_from = c(mon, value_type), values_from = num_samples, values_fill = 0)

active_ACAD <- c("ANC_ueqL", "ChlA_ugL", "ChlA_EXO_ugL", "Discharge_cfs", "DO_mgL", "DOC_mgL",
                 "PenetrationRatio", "pH", "SecchiDepth_m", "SpCond_uScm", "Temp_C",
                 "TN_mgL", "TP_ugL", "Turbidity_FNU", "WaterLevel_Feet")

active_LNETN <- c("ANC_ueqL", "Discharge_cfs", "DO_mgL", "PenetrationRatio",
                  "pH", "SpCond_uScm", "Temp_C", "TN_mgL", "TP_ugL",
                  "Turbidity_FNU", "WaterLevel_Feet")

samp_tab2 <- samp_tab |> filter(UnitCode == "ACAD" & Parameter %in% c(active_ACAD) |
                                  UnitCode != "ACAD" & Parameter %in% c(active_LNETN))

# Add month columns that could be missing (mostly cens)
all_cols <- c("UnitCode", "SiteCode", "SiteName", "SiteType", "param_type", "Parameter",
              "year_start", "year_latest", "num_years",
              "May_real", "May_cens", "Jun_real", "Jun_cens", "Jul_real", "Jul_cens",
              "Aug_real", "Aug_cens", "Sep_real", "Sep_cens", "Oct_real", "Oct_cens")

missing <- setdiff(all_cols, names(samp_tab2))

samp_tab2[missing] <- 0

samp_tab_final <- samp_tab2 |>
  mutate(year_range = paste0(year_start, " \U2013 ", year_latest)) |>
  select(UnitCode, SiteType, SiteName,  param_type, Parameter, year_range,
         num_years,
         May = May_real, Jun = Jun_real, Jul = Jul_real,
         Aug = Aug_real, Sep = Sep_real, Oct = Oct_real,
         May_cens, Jun_cens, Jul_cens, Aug_cens, Sep_cens,
         Oct_cens) |>
  arrange(UnitCode, SiteType, SiteName, param_type, Parameter)

return(samp_tab_final)
}
