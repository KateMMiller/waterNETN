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
      if(length(par_sec) > 0){getSecchi(years = year_range, output = 'verbose') |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_dis) > 0){getDischarge(years = year_range) |>
          mutate(SiteType = "stream", Parameter = "Discharge_cfs") |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value = Discharge_cfs)
      } else {NULL},
      if(length(par_pen) > 0){getLightPen(years = year_range, parameter = par_pen) |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wl) > 0){getWaterLevel(years = year_range, output = 'verbose') |>
          mutate(Parameter = "WaterLevel_Feet", Value = WaterLevel_Feet) |> mutate(SiteType = "lake") |>
          select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)
      } else {NULL},
      if(length(par_wlm) > 0){getWaterLevel(years = year_range, output = 'verbose') |>
        mutate(Parameter = "WaterLevel_m", Value = WaterLevel_m) |> mutate(SiteType = "lake") |>
        select(SiteCode, SiteName, SiteType, UnitCode, EventDate, year, month, doy, Parameter, Value)} else {NULL}
      )

dat_pct <- wdat |> group_by(UnitCode) |>
  mutate(pct99 = quantile(Value, probs = 0.99, na.rm = T),
         pct01 = quantile(Value, probs = 0.01, na.rm = T),
         check = case_when(Value > pct99 ~ "pct99",
                           Value < pct01 ~ "pct01",
                           TRUE ~ NA_character_)) |>
  filter(!is.na(check)) |>
  arrange(check, UnitCode, SiteCode)

check99 <- dat_pct |> filter(check %in% "pct99") |> arrange(UnitCode, SiteType, SiteCode) |>
  select(Park = UnitCode, SiteCode, SiteName, SiteType, EventDate, Parameter, Value, pct99)

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
                  QC_check(df = check01, tab = tab, meas_type = meas_type,
                           check = paste0(param, " values that are below 1% of all historic values within a park."),
                           chk_type = chk_type))

kab99 <- make_kable(check99, paste0(param, " values that are above 99% of all historic values within a park."))
kab01<- make_kable(check01, paste0(param, " values that are below 1% of all historic values within a park."))

assign(paste0("tbl_", param, "_99"), kab99, envir = .GlobalEnv)
assign(paste0("tbl_", param, "_01"), kab01, envir = .GlobalEnv)

rm(kab99, kab01)

return(QC_table)

}

svl_pct_check <- function(param_sonde = NA, param_lab = NA){

  lab <- getChemistry(parameter = param_lab, include_censored = TRUE) |>
    select(UnitCode, SiteCode, SiteName, EventDate, year, month, Parameter, Value, censored)
  sonde <- getSondeInSitu(parameter = param_sonde, sample_depth = "surface") |>
    mutate(censored = FALSE) |>
    select(UnitCode, SiteCode, SiteName, EventDate, year, month, Parameter, Value, censored)

  df_join <- full_join(sonde, lab,
                       by = c("UnitCode", "SiteCode", "SiteName", "EventDate", "year", "month"),
                       suffix = c("_sonde", "_lab"),
                       relationship = 'many-to-many') |>
    filter(!is.na(Value_sonde) & !is.na(Value_lab))

  df_join$diff <- df_join$Value_sonde - df_join$Value_lab
  df_join$pct_diff <- round(((df_join$Value_lab - df_join$Value_sonde)/df_join$Value_sonde)*100, 2)

  diff_10 <- df_join |> filter(abs(pct_diff) > 10) |>
    select(UnitCode, SiteCode, EventDate, year, month, parameter = Parameter_sonde, Value_sonde, Value_lab, pct_diff)
  head(diff_10)

  pct_diff_kbl <-
    make_kable(diff_10, cap =
                 paste0("Measurements that are more than 10% different between Sonde and lab.",
                        "Negative values indicate the Sonde value was greater than the lab. ",
                        "Positive values indicate the Sonde value was lower than the lab."))

  max_diff = max(df_join$Value_sonde, df_join$Value_lab, na.rm = T)
  max_pctdif = max(df_join$pct_diff, na.rm = T)



  svl_diff <-
    ggplot(data = df_join, aes(x = pct_diff)) +
    geom_density(alpha = 0.5, fill = "#95a1b9", color = "#747e91") +
    geom_vline(xintercept = 0, col = "#717171", linewidth = 0.75) +
    labs(y = "Density", x = paste0(param_sonde, " % Difference")) +
    theme(legend.position = 'none', panel.border = element_blank(), panel.background = element_blank()) +
    annotate(geom = "label", x = -max_pctdif, y = Inf, label = "Sonde > Lab",
             color = 'black', size = 4, hjust = 0, vjust = 1) +
    annotate(geom = "label", x = max_pctdif, y = Inf, label = "Sonde < Lab",
             color = 'black', size = 4, hjust = 1, vjust = 1) +
    geom_vline(xintercept = 10, linetype = 'dashed', col = 'red', linewidth = 0.75) +
    geom_vline(xintercept = -10, linetype = 'dashed', col = 'red', linewidth = 0.75) +
    annotate(geom = 'label', x = 0, y = Inf, label = "Within 10%", fill = "white",
             size = 4, hjust = 0.5, vjust = 1, alpha = 0.8)

  assign(paste0("tbl_", param_sonde, "_10pct"), pct_diff_kbl, envir = .GlobalEnv)
  assign(paste0("pctdiff_", param_sonde), svl_diff, envir = .GlobalEnv)

  QC_table <- rbind(QC_table,
                    QC_check(df = diff_10, meas_type = "Water Quality", tab = "Lab vs Sonde",
                    check = paste0(param_sonde, " lab values that are greater than 10% different than Sonde values."),
                    chk_type = "check"))
  return(QC_table)
}
### JESS ADDITION ####
svl_pct_check_env_rep <- function(param_env = NA, param_rep = NA){

  env <- getChemistry(parameter = param_env, include_censored = TRUE, QC_type = "ENV") |>
    select(UnitCode, SiteCode, SiteName, EventDate, year, month, Parameter, Value, censored)
  rep <- getChemistry(parameter = param_rep, QC_type = "REP") |>
    mutate(censored = FALSE) |>
    select(UnitCode, SiteCode, SiteName, EventDate, year, month, Parameter, Value, censored)

  df_join <- full_join(env, rep,
                       by = c("UnitCode", "SiteCode", "SiteName", "EventDate", "year", "month"),
                       suffix = c("_env", "_rep"),
                       relationship = 'many-to-many') |>
    filter(!is.na(Value_env) & !is.na(Value_rep))

  df_join$diff <- df_join$Value_env - df_join$Value_rep
  df_join$pct_diff <- round(((df_join$Value_rep - df_join$Value_env)/df_join$Value_env)*100, 2)

  diff_10 <- df_join |> filter(abs(pct_diff) > 10) |>
    select(UnitCode, SiteCode, EventDate, year, month, parameter = Parameter_env, Value_env, Value_rep, pct_diff)
  head(diff_10)

  pct_diff_kbl_rep <-
    make_kable(diff_10, cap =
                 paste0("Measurements that are more than 10% different between ENV and REP.",
                        "Negative values indicate the ENV value was greater than the REP. ",
                        "Positive values indicate the ENV value was lower than the REP."))

  max_diff = max(df_join$Value_env, df_join$Value_rep, na.rm = T)
  max_pctdif = max(df_join$pct_diff, na.rm = T)

  svl_diff_rep <-
    ggplot(data = df_join, aes(x = pct_diff)) +
    geom_density(alpha = 0.5, fill = "#95a1b9", color = "#747e91") +
    geom_vline(xintercept = 0, col = "#717171", linewidth = 0.75) +
    labs(y = "Density", x = paste0(param_env, " % Difference")) +
    theme(legend.position = 'none', panel.border = element_blank(), panel.background = element_blank()) +
    annotate(geom = "label", x = -max_pctdif, y = Inf, label = "ENV > REP",
             color = 'black', size = 4, hjust = 0, vjust = 1) +
    annotate(geom = "label", x = max_pctdif, y = Inf, label = "ENV < REP",
             color = 'black', size = 4, hjust = 1, vjust = 1) +
    geom_vline(xintercept = 10, linetype = 'dashed', col = 'red', linewidth = 0.75) +
    geom_vline(xintercept = -10, linetype = 'dashed', col = 'red', linewidth = 0.75) +
    annotate(geom = 'label', x = 0, y = Inf, label = "Within 10%", fill = "white",
             size = 4, hjust = 0.5, vjust = 1, alpha = 0.8)

  assign(paste0("tbl_", param_env, "_10pct_rep"), pct_diff_kbl_rep, envir = .GlobalEnv)
  assign(paste0("pctdiff_", param_env, "_rep"), svl_diff_rep, envir = .GlobalEnv)

  QC_table <- rbind(QC_table,
                    QC_check(df = diff_10, meas_type = "Water Quality", tab = "ENV vs REP",
                             check = paste0(param_env, " REP values that are greater than 10% different than ENV values."),
                             chk_type = "check"))
  return(QC_table)
}

