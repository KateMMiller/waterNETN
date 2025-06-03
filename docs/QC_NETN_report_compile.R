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

#---- Functions ----
source("QC_NETN_report_functions.R")

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
# pH
pH_lab <- getChemistry(parameter = "pH_Lab", include_censored = TRUE) |>
  select(UnitCode, SiteCode, SiteName, EventDate, year, month, Parameter, Value, censored)
pH_sonde <- getSondeInSitu(parameter = "pH", sample_depth = "surface") |>
  mutate(censored = FALSE) |>
  select(UnitCode, SiteCode, SiteName, EventDate, year, month, Parameter, Value, censored)

pH_join <- full_join(pH_sonde, pH_lab,
                     by = c("UnitCode", "SiteCode", "SiteName", "EventDate", "year", "month"),
                     suffix = c("_sonde", "_lab"),
                     relationship = 'many-to-many') |>
  filter(!is.na(Value_sonde) & !is.na(Value_lab))


pH_join$diff <- pH_join$Value_sonde - pH_join$Value_lab
pH_join$pct_diff <- round(((pH_join$Value_lab - pH_join$Value_sonde)/pH_join$Value_sonde)*100, 2)

diff_10 <- pH_join |> filter(abs(pct_diff) > 10) |>
  select(UnitCode, SiteCode, EventDate, year, month, parameter = Parameter_sonde, Value_sonde, Value_lab, pct_diff)
head(diff_10)

pct_diff_kbl <-
  make_kable(diff_10, cap =
             paste0("Measurements that are more than 10% different between Sonde and lab.",
             "Negative values indicate the Sonde value was greater than the lab. ",
             "Positive values indicate the Sonde value was lower than the lab."))

max_pH = max(pH_join$Value_sonde, pH_join$Value_lab, na.rm = T)
max_pctdif = max(pH_join$pct_diff)

pH_comp_plot <-
  ggplot(pH_join, aes(x = Value_sonde, y = Value_lab)) +
    geom_point(col = "#696969", fill = '#CACACA', shape = 21) +
    geom_abline(slope = 1, intercept = 0, col = 'red', linewidth = 0.75) +
    geom_smooth(method = "lm", color = "black", se = F) +
    # geom_abline(slope = 1, intercept = 0.99, col = 'red', linewidth = 0.75) +
    # geom_abline(slope = 1, intercept = -0.99, col = 'red', linewidth = 0.75) +
    # geom_abline(slope = 1, intercept = 0.50, col = 'red', alpha = 0.6, linewidth = 0.5) +
    # geom_abline(slope = 1, intercept = -0.50, col = 'red', alpha = 0.6, linewidth = 0.5) +
    labs(x = "Sonde pH", y = "Lab pH") +
    coord_equal() +
    theme_WQ()

pH_diff <-
  ggplot(data = pH_join, aes(x = pct_diff)) +
    geom_density(alpha = 0.5, fill = "#95a1b9", color = "#747e91") +
    geom_vline(xintercept = 0, col = "#717171", linewidth = 0.75) +
    labs(y = "Density", x = "pH % Difference") +
    theme(legend.position = 'none', panel.border = element_blank(), panel.background = element_blank()) +
    annotate(geom = "label", x = -max_pctdif, y = Inf, label = "Sonde > Lab",
             color = 'black', size = 4, hjust = 0, vjust = 1) +
    annotate(geom = "label", x = max_pctdif, y = Inf, label = "Sonde < Lab",
             color = 'black', size = 4, hjust = 1, vjust = 1) +
    geom_vline(xintercept = 10, linetype = 'dashed', col = 'red', linewidth = 0.75) +
    geom_vline(xintercept = -10, linetype = 'dashed', col = 'red', linewidth = 0.75) +
    annotate(geom = 'label', x = 0, y = Inf, label = "Within 10%", fill = "white",
             size = 4, hjust = 0.5, vjust = 1, alpha = 0.8)

# Then make this into a function to do multiple params.

# check if Lab v Sond checks returned at least 1 record to determine whether to include that tab in report
lab_v_sonde_check <- QC_table |> filter(Data %in% "Lab vs Sonde" & Num_Records > 0)
lab_v_sond_include <- tab_include(lab_v_sond_check)

#------ WQual: DO 899 & 999 checks -------

# check if DO 899/999 checks returned at least 1 record to determine whether to include that tab in report
do899_check <- QC_table |> filter(Data %in% "DO 899 and 999" & Num_Records > 0)
do899_include <- tab_include(do899_check)

#------ WQual: QC samples vs ENV samples -------

# check if QC samples vs ENV checks returned at least 1 record to determine whether to include that tab in report
qcsamp_check <- QC_table |> filter(Data %in% "QC Samples" & Num_Records > 0)
qcsamp_include <- tab_include(qcsamp_check)

#------ WQual: BLANK samples -------

# check if QC samples vs ENV checks returned at least 1 record to determine whether to include that tab in report
blank_check <- QC_table |> filter(Data %in% "BLANK Samples" & Num_Records > 0)
blank_include <- tab_include(blank_check)

#------ WQual: Secchi Depth -------


# check if Secchi checks returned at least 1 record to determine whether to include that tab in report
secchi_check <- QC_table |> filter(Data %in% "Secchi Depth" & Num_Records > 0)
secchi_include <- tab_include(secchi_check)

# check if Water Quality checks returned at least 1 record to determine whether to include that tab in report
wqual_check <- QC_table |> filter(Type %in% "Water Quality" & Num_Records > 0)
wqual_include <- tab_include(wqual_check)

#---- Water Quantity Checks ----

#------ WQuant: Lake Level -------

#------ WQuant: Discharge -------


# check if Water Quantity checks returned at least 1 record to determine whether to include that tab in report
wquant_check <- QC_table |> filter(Type %in% "Water Quantity" & Num_Records > 0)
wquant_include <- tab_include(wquant_check)

#+++++ Compile final QC Table
#+# revise for different color combos for checks (99 vs 90)
QC_check_table <- kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
                         col.names = c("Type", "Data Tab", "Check Description", "Number of Records", "Check Type")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(3, width = "150px") |>
  column_spec(2:ncol(QC_table), background =
                ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "error", "#F2F2A0",
                       ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "check", "#b7d8ef", "#ffffff"))) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;')

