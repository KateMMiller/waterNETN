#----------------------------------------
# Conducts QC checks on CUVA wetland data. Output is reported in QC_VIBI_report.Rmd
#----------------------------------------
#
# Params to turn on when running within script. Otherwise set params in Rmd.
#
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

#---- Functions ----
# Summarize results of QC check
QC_check <- function(df, tab, meas_type, check, chk_type){
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
# DO <> 99% recorded
do <- getSondeInSitu(years = year_range, parameter = "DO_mgL")

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

# check if Sonde checks returned at least 1 record to determine whether to include that tab in report
sonde_check <- QC_table |> filter(tab %in% "Sonde Measurements" & Num_Records > 0)
sonde_include <- tab_include(sonde_check)

#------ WQual: Lab Parameters -------

# check if Lab checks returned at least 1 record to determine whether to include that tab in report
lab_check <- QC_table |> filter(tab %in% "Lab Measurements" & Num_Records > 0)
lab_include <- tab_include(lab_check)


#------ WQual: Lab vs Sonde Parameters -------


# check if Lab v Sond checks returned at least 1 record to determine whether to include that tab in report
lab_v_sonde_check <- QC_table |> filter(tab %in% "Lab vs Sonde" & Num_Records > 0)
lab_v_sond_include <- tab_include(lab_v_sond_check)

#------ WQual: DO 899 & 999 checks -------

# check if DO 899/999 checks returned at least 1 record to determine whether to include that tab in report
do899_check <- QC_table |> filter(tab %in% "DO 899 and 999" & Num_Records > 0)
do899_include <- tab_include(do899_check)

#------ WQual: QC samples vs ENV samples -------

# check if QC samples vs ENV checks returned at least 1 record to determine whether to include that tab in report
qcsamp_check <- QC_table |> filter(tab %in% "QC Samples" & Num_Records > 0)
qcsamp_include <- tab_include(qcsamp_check)

#------ WQual: BLANK samples -------

# check if QC samples vs ENV checks returned at least 1 record to determine whether to include that tab in report
blank_check <- QC_table |> filter(tab %in% "BLANK Samples" & Num_Records > 0)
blank_include <- tab_include(blank_check)

#------ WQual: Secchi Depth -------


# check if Secchi checks returned at least 1 record to determine whether to include that tab in report
secchi_check <- QC_table |> filter(tab %in% "Secchi Depth" & Num_Records > 0)
secchi_include <- tab_include(secchi_check)

# check if Water Quality checks returned at least 1 record to determine whether to include that tab in report
wqual_check <- QC_table |> filter(meas_type %in% "Water Quality" & Num_Records > 0)
wqual_include <- tab_include(wqual_check)

#---- Water Quantity Checks ----

#------ WQuant: Lake Level -------

#------ WQuant: Discharge -------


# check if Water Quantity checks returned at least 1 record to determine whether to include that tab in report
wquant_check <- QC_table |> filter(meas_type %in% "Water Quantity" & Num_Records > 0)
wquant_include <- tab_include(wquant_check)

#+++++ Compile final QC Table
QC_check_table <-  kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
                         col.names = c("Data Tab", "Check Description", "Number of Records", "Check Type")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(3, width = "150px") |>
  column_spec(2:ncol(QC_table), background =
                ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "error", "#F2F2A0",
                       ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "check", "#b7d8ef", "#ffffff"))) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;')

