# Functions used in QC_NETN_report_Compile or QC_NETN_report.RMD.

# Summarize results of QC check
QC_check <- function(df, meas_type, tab, check, chk_type){
  result <- data.frame("Data" = tab, "Type" = meas_type,
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

# Funcionalize the percent checks, so easier to create many checks.
pct_check <- function(param = "DO_mgL", meas_type = "Water Quality", tab = "Sonde Measurements",
                      check = NA_character_, chk_type = "check" ){

  # determines which function to use to get data
  chem <- c("ANC", "ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
            "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3", "NO2+NO3_mgL",
            "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL", "TN_mgL",
            "TotDissN", "TotDissN_mgL", "TotDissP", "TotDissP_ugL", "TP_ugL")

  sonde <- c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
             "Turbidity_FNU", "ChlA_EXO_RFU", "ChlA_EXO_ugL", "BP_mmHg")

  other <- c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m")

  par_chem <- parameter[parameter %in% chem]
  par_sonde <- parameter[parameter %in% sonde]
  par_sec <- parameter[parameter %in% "SDepth_m"]
  par_dis <- parameter[parameter %in% "Discharge_cfs"]
  par_pen <- parameter[parameter %in% "PenetrationRatio"]
  par_wl <- parameter[parameter %in% "WaterLevel_Feet"]
  par_wlm <- parameter[parameter %in% "WaterLevel_m"]

  wdat <-
    rbind(
      if(length(par_chem) > 0){getChemistry(years = year_range, parameter = par_chem) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_sonde) > 0){getSondeInSitu(years = year_range, parameter = par_sonde) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_sec) > 0){getSecchi(park = park, years = year_range, parameter = par_sec) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_dis) > 0){getDischarge(park = park, years = year_range, parameter = par_dis) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_pen) > 0){getLightPen(park = park, years = year_range, parameter = par_pen) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wl) > 0){getWaterLevel(park = park, years = year_range, parameter = par_wl) |>
          mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
          select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wlm) > 0){getWaterLevel(park = park, years = year_range, parameter = par_wlm) |>
        mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |>
        select(SiteCode, SiteName, UnitCode, EventDate, year, month, doy, Parameter, Value)} else {NULL}
      )

do_pct <- do |> group_by(UnitCode) |>
  mutate(do_99pct = quantile(Value, probs = 0.99, na.rm = T),
         do_1pct = quantile(Value, probs = 0.01, na.rm = T),
         do_75pct = quantile(Value, probs = 0.75, na.rm = T),
         do_25pct = quantile(Value, probs = 0.25, na.rm = T),
         do_check = case_when(Value > do_99pct ~ "99",
                              Value > do_75pct ~ "75",
                              Value < do_1pct ~ "1",
                              Value < do_25pct ~ "25",
                              TRUE ~ NA_character_)) |>
  filter(!is.na(do_check)) |>
  arrange(do_check, UnitCode, SiteCode)

do_99 <- do_pct |> filter(do_check %in% "99") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, do_99pct)

do_75 <- do_pct |> filter(do_check %in% "75") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, do_99pct)

do_25 <- do_pct |> filter(do_check %in% "25") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, do_99pct)

do_01 <- do_pct |> filter(do_check %in% "1") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, do_99pct)

if(exists("QC_table")){rbind(QC_table,
                             QC_check(do_99,
                                      "Water Quality",
                                      "Sonde Measurements",
                                      "Dissolved Oxygen values that are above 99% of all values that have been recorded within a park.",
                                      "check"))
  } else {QC_check(do_99,
                  "Water Quality",
                  "Sonde Measurements",
                  "Dissolved Oxygen values that are above 99% of all values that have been recorded within a park.",
                  "check")}


QC_table <- QC_check(do_99,
                     "Water Quality",
                     "Sonde Measurements",
                     "Dissolved Oxygen values that are above 99% of all values that have been recorded within a park.",
                     "check")

tbl_do_99 <- make_kable(do_99,
                        "Dissolved Oxygen values that are above 99% of all values that have been recorded within a park.")

QC_table <- rbind(QC_table,
                  QC_check(do_75,
                           "Water Quality",
                           "Sonde Measurements",
                           "Dissolved Oxygen values that are 75 - 99% of all values that have been recorded within a park.",
                           "check"))

tbl_do_75 <- make_kable(do_75,
                        "Dissolved Oxygen values that are  75 - 99% of all values that have been recorded within a park.")



QC_table <- rbind(QC_table,
                  QC_check(do_25,
                           "Water Quality",
                           "Sonde Measurements",
                           "Dissolved Oxygen values that are 1 - 25% of all values that have been recorded within a park.",
                           "check"))

tbl_do_25 <- make_kable(do_25,
                        "Dissolved Oxygen values that are 1 - 25% of all values that have been recorded within a park.")

QC_table <- rbind(QC_table,
                  QC_check(do_01,
                           "Water Quality",
                           "Sonde Measurements",
                           "Dissolved Oxygen values that are below 1% of all values that have been recorded within a park.",
                           "check"))

tbl_do_01 <- make_kable(do_01,
                        "Dissolved Oxygen values that are below 1% of all values that have been recorded within a park.")

}
