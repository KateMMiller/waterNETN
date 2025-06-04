#----------------------------------------
# Conducts QC checks on CUVA wetland data. Output is reported in QC_VIBI_report.Rmd
#----------------------------------------
#
# Params to turn on when running within script. Otherwise set params in Rmd.
# #
# library(tidyverse)
# library(knitr) # for kable functions
# library(kableExtra) # for additional kable features
# #library(htmltools) # check what this is for before turning on
#
# library(waterNETN)
# importData()
#
# year_curr = 2024
# year_range = 2006:2024
# all_years = TRUE

#---- Sampling Matrix -----
samp_hist <- sumEvents(years = year_range) |>
  select(Park = UnitCode, SiteType, SiteName, param_type:Oct_cens) |>
  arrange(Park, SiteType, SiteName, param_type, Parameter)

samp_tab <-
  kable(samp_hist, format = 'html', align = c(rep("l", 5), rep("c", 13)),
        caption = "Sampling matrix by site, parameter, year, and month",
        col.names = c("Park", "Site Type", "Site Name", "Param. Type", "Parameter", "Year Range",
                      "# Years Sampled", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                      "May cens.", "Jun cens.", "Jul cens.", "Aug cens.", "Sep cens.", "Oct cens.")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'), full_width = FALSE,
                position = 'left', font_size = 11) |>
  collapse_rows(1:4, valign = 'top')

#---- Water Quality Checks ----
#------ WQual: Sonde Parameters -------
QC_table <- pct_check(param = "DO_mgL", meas_type = 'Water Quality', tab = 'Sonde Measurements', chk_type = "check")
QC_table <- pct_check(param = "pH", meas_type = 'Water Quality', tab = 'Sonde Measurements', chk_type = "check")

#+++ Add lines for remaining parameters. Just need to change the param name and assign it to QC_table.
#+++ For each line above 4 kables are returned, one for each percentage (eg., tbl_DO_mgL_99). Those
#+++ need to be added to the QC_NETN_report in the Sonde Parameters tab.

# check if Sonde checks returned at least 1 record to determine whether to include that tab in report
sonde_check <- QC_table |> filter(Data %in% "Sonde Measurements" & Num_Records > 0)
sonde_include <- tab_include(sonde_check)

#------ WQual: Lab Parameters -------
QC_table <- pct_check(param = "ANC_ueqL", meas_type = 'Water Quality', tab = 'Lab Measurements', chk_type = "check")

#+++ Add lines for remaining parameters. Just need to change the param name and assign it to QC_table.
#+++ For each line above 4 kables are returned, one for each percentage (eg., tbl_ANC_mgL_99). Those
#+++ need to be added to the QC_NETN_report in the Lab Parameters tab.

# check if Lab checks returned at least 1 record to determine whether to include that tab in report
lab_check <- QC_table |> filter(Data %in% "Lab Measurements" & Num_Records > 0)
lab_include <- tab_include(lab_check)

#------ WQual: Lab vs Sonde Parameters -------
QC_table <- svl_pct_check(param_sonde = "pH", param_lab = "pH_Lab")

# Output of svl_pct_check: tbl_pH_10 and pctdiff_pH

#+++ Add lines for remaining parameters comps. Just need to change the param name and assign it to QC_table.
#+++ For each line above a kable and a figure are returned (eg tbl_pH_10, pctdiff_pH). Those
#+++ need to be added to the QC_NETN_report in the Lab cs Sonde tab.

# check if Lab v Sonde checks returned at least 1 record to determine whether to include that tab in report
lab_v_sonde_check <- QC_table |> filter(Data %in% "Lab vs Sonde" & Num_Records > 0)
lab_v_sonde_include <- tab_include(lab_v_sonde_check)

# check if Water Quality checks returned at least 1 record to determine whether to include that tab in report
wqual_check <- QC_table |> filter(Type %in% "Water Quality" & Num_Records > 0)
wqual_include <- tab_include(wqual_check)

#---- Quality Control ----
#------ QC: DO 899 & 999 checks -------
dosat <- getSondeInSitu(years = year_range, parameter = "DOsat_pct", sample_depth = 'all', QC_type = c("899", "999")) |>
  filter(Value < 98 | Value > 102) |>
  select(UnitCode, SiteCode, SiteType, EventDate, year, month, QCtype, SampleDepth_m, Value) |>
  arrange(UnitCode, SiteCode, EventDate, QCtype)

QC_table <- rbind(QC_table,
                  QC_check(dosat, meas_type = "Quality Control", tab = "DO 899 and 999",
                           check = "DO saturation levels beyond accepted calibrartion of 98-102%",
                           chk_type = 'error'))

do_kbl <- make_kable(dosat, "DO saturation levels beyond accepted calibration of 98-102%.")

# check if DO 899/999 checks returned at least 1 record to determine whether to include that tab in report
do899_check <- QC_table |> filter(Data %in% "DO 899 and 999" & Num_Records > 0)
do899_include <- tab_include(do899_check)

#------ QC: QC samples vs ENV samples -------
# check if QC samples vs ENV checks returned at least 1 record to determine whether to include that tab in report
qcsamp_check <- QC_table |> filter(Data %in% "QC Samples" & Num_Records > 0)
qcsamp_include <- tab_include(qcsamp_check)

#------ QC: BLANK samples -------
blank <- getChemistry(years = year_range, QC_type = "BLANK") |> filter(!is.na(Value)) |> filter(Value > 0.05) |>
  select(UnitCode, SiteCode, EventDate, year, month, QCType, SampleType, Parameter, Value, SampleDepth_m, LabCode)

QC_table <- rbind(QC_table,
                  QC_check(blank, meas_type = "Quality Control", tab = "BLANK Samples",
                           check = "Blank samples > 0.05", chk_type = 'error'))

blank_kbl <- make_kable(blank, "Blank samples > 0.05")

# check if QC samples vs ENV checks returned at least 1 record to determine whether to include that tab in report
blank_check <- QC_table |> filter(Data %in% "BLANK Samples" & Num_Records > 0)
blank_include <- tab_include(blank_check)

#------ WQual: Secchi Depth -------
QC_table <- pct_check(param = "SDepth_m", meas_type = 'Water Quality', tab = 'Secchi Depth', chk_type = "check")

# check if Secchi checks returned at least 1 record to determine whether to include that tab in report
secchi_check <- QC_table |> filter(Data %in% "Secchi Depth" & Num_Records > 0)
secchi_include <- tab_include(secchi_check)

# check if Quality Control checks returned at least 1 record to determine whether to include that tab in report
qc_check <- QC_table |> filter(Type %in% "Quality Control" & Num_Records > 0)
qc_include <- tab_include(qc_check)

#---- Water Quantity Checks ----
QC_table <- pct_check(param = "WaterLevel_m", meas_type = 'Water Quantity', tab = 'Lake Level', chk_type = "check")
QC_table <- pct_check(param = "Discharge_cfs", meas_type = 'Water Quantity', tab = 'Lake Level', chk_type = "check")

# check if Lake level checks returned at least 1 record to determine whether to include that tab in report
lakelev_check <- QC_table |> filter(Data %in% "Lake Level" & Num_Records > 0)
lakelev_include <- tab_include(lakelev_check)

# check if discharge checks returned at least 1 record to determine whether to include that tab in report
disch_check <- QC_table |> filter(Data %in% "Discharge" & Num_Records > 0)
disch_include <- tab_include(disch_check)

# check if Water Quantity checks returned at least 1 record to determine whether to include that tab in report
wquant_check <- QC_table |> filter(Type %in% "Water Quantity" & Num_Records > 0)
wquant_include <- tab_include(wquant_check)

#+++++ Compile final QC Table
#+# revise for different color combos for checks (99 vs 90)?
QC_check_table <- kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
                         col.names = c("Type", "Data Tab", "Check Description", "Number of Records", "Check Type")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(3, width = "300px") |>
  column_spec(2:ncol(QC_table), background =
                ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "error", "#F2F2A0",
                       ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "check", "#b7d8ef", "#ffffff"))) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;')

