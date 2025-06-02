# Functions used in QC_NETN_report_Compile or QC_NETN_report.RMD.

# Summarize results of QC check
QC_check <- function(df, meas_type, tab, check, chk_type){
  result <- data.frame("Type" = meas_type, "Data" = tab,
                       "Description" = check, "Num_Records" = nrow(df), "check_type" = chk_type)
}

# function to make tables via kable
make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    if(nrow(df) > 1){
      kable(df, format = 'html', align = 'c', caption = cap)  |>
        kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                      full_width = TRUE, position = 'left', font_size = 12) |>
        row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        collapse_rows(1, valign = 'top') |>
        row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
    } else if(nrow(df) == 1){
      kable(df, format = 'html', align = 'c', caption = cap)  |>
        kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                      full_width = TRUE, position = 'left', font_size = 12) |>
        row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
    }
  } else NULL
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level), collapse = ""), " ", tab_title, " {.tabset} ", "\n\n")}
  check_null(table)
}

# Functionalize the percent checks, so easier to create many checks.
pct_check <- function(param = NA, meas_type = "Water Quality", tab = "Sonde Measurements",
                      chk_type = "check"){

  # determines which function to use to get data
  chem <- c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
            "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
            "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL", "TN_mgL",
            "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP_ugL")

  sonde <- c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
             "Turbidity_FNU", "ChlA_EXO_RFU", "ChlA_EXO_ugL", "BP_mmHg")

  other <- c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m")

  par_chem <- param[param %in% chem]
  par_sonde <- param[param %in% sonde]
  par_sec <- param[param %in% "SDepth_m"]
  par_dis <- param[param %in% "Discharge_cfs"]
  par_pen <- param[param %in% "PenetrationRatio"]
  par_wl <- param[param %in% "WaterLevel_Feet"]
  par_wlm <- param[param %in% "WaterLevel_m"]

  wdat <-
    rbind(
      if(length(par_chem) > 0){getChemistry(years = year_range, parameter = par_chem, output = "verbose") |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_sonde) > 0){getSondeInSitu(years = year_range, parameter = par_sonde, sample_depth = 'all') |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_sec) > 0){getSecchi(park = park, years = year_range, parameter = par_sec) |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_dis) > 0){getDischarge(park = park, years = year_range, parameter = par_dis) |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_pen) > 0){getLightPen(park = park, years = year_range, parameter = par_pen) |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wl) > 0){getWaterLevel(park = park, years = year_range, parameter = par_wl) |>
          mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wlm) > 0){getWaterLevel(park = park, years = year_range, parameter = par_wlm) |>
        mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
        select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)} else {NULL}
      )

dat_pct <- wdat |> group_by(UnitCode) |>
  mutate(pct99 = quantile(Value, probs = 0.99, na.rm = T),
         pct01 = quantile(Value, probs = 0.01, na.rm = T),
         pct90 = quantile(Value, probs = 0.90, na.rm = T),
         pct10 = quantile(Value, probs = 0.10, na.rm = T),
         check = case_when(Value > pct99 ~ "pct99",
                           Value > pct90 ~ "pct90",
                           Value < pct01 ~ "pct01",
                           Value < pct10 ~ "pct10",
                           TRUE ~ NA_character_)) |>
  filter(!is.na(check)) |>
  arrange(check, UnitCode, SiteCode)

check99 <- dat_pct |> filter(check %in% "pct99") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, pct99)

check90 <- dat_pct |> filter(check %in% "pct90") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, pct90)

check10 <- dat_pct |> filter(check %in% "pct10") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, pct10)

check01 <- dat_pct |> filter(check %in% "pct01") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, pct01)

if(exists("QC_table")){
  QC_table <-
    rbind(QC_table,
        QC_check(df = check99, tab = tab, meas_type = meas_type,
                 check = paste0(param, " values that are above 99% of all historic values within a park."),
                 chk_type = chk_type))
  } else {
  QC_table <-
    QC_check(df = check99, tab = tab, meas_type = meas_type,
           check = paste0(param, " values that are above 99% of all historic values within a park."),
           chk_type = chk_type)}

QC_table <- rbind(QC_table,
                  QC_check(df = check90, tab = tab, meas_type = meas_type,
                           check = paste0(param, " values that are 90 - 99% above all historic values within a park."),
                           chk_type = chk_type),
                  QC_check(df = check10, tab = tab, meas_type = meas_type,
                           check = paste0(param, " values that are 1 - 10% below all historic values within a park."),
                           chk_type = chk_type),
                  QC_check(df = check01, tab = tab, meas_type = meas_type,
                           check = paste0(param, " values that are below 1% of all historic values within a park."),
                           chk_type = chk_type))

kab99 <- make_kable(check99, paste0(param, " values that are above 99% of all historic values within a park."))
kab01<- make_kable(check01, paste0(param, " values that are below 1% of all historic values within a park."))
kab90 <- make_kable(check90, paste0(param, " values that are 90 - 99% above all historic values within a park."))
kab10 <- make_kable(check10, paste0(param, " values that are 1 - 10% below all historic values within a park."))

assign(paste0("tbl_", param, "_99"), kab99, envir = .GlobalEnv)
assign(paste0("tbl_", param, "_01"), kab01, envir = .GlobalEnv)
assign(paste0("tbl_", param, "_90"), kab90, envir = .GlobalEnv)
assign(paste0("tbl_", param, "_10"), kab10, envir = .GlobalEnv)

rm(kab99, kab01, kab90, kab10)

return(QC_table)

}
