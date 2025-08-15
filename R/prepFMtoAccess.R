#' @title prepFMtoAccess: prepare exported FileMaker data for MS Access import.
#'
#' @description This function ingests the excel file that is exported from the FileMaker app for the NETN water monitoring program
#' and prepares data for importing in the the MS Access database. Users must specify the path and file name. The
#' function will determine whether the data is from a lake or stream based on column names. The function then exports
#' a csv file in the same filepath and with the same excel_name with '_cleaned_' and the date added to the filename, and
#' that is ready to be imported into MS Access. Note that function can only work with lake or stream data, not both
#' in the same function call. The output is intentionally a csv instead of xlsx because of the funky things Excel does to dates.
#'
#' @importFrom dplyr filter mutate
#'
#' @param filepath Quoted path where FileMaker export lives, and where the output from the function will be exported.
#'
#' @param excel_name Quoted name of the FileMaker export file, including the .xlsx
#'
#' @param keep_file Logical. If TRUE, will return a file to the global environment called cleaned_data. If FALSE (default),
#' will only export the file as an .xlsx.
#'
#' @return Saves an xlsx file with cleaned water data ready for MS Access import.
#'
#' @examples
#' \dontrun{
#'
#' library(waterNETN)
#'
#' # prep Lake data and keep output in R
#' prepFMtoAccess(filepath = "../data", excel_name = "ACAD_WQ_Lakes_2025-06.xlsx", keep_file = T)
#'
#' # prep Stream data
#' prepFMtoAccess(filepath = "../data", excel_name = "ACAD_WQ_Stream_2025-06.xlsx")
#'
#' }
#'
#' @export

prepFMtoAccess <- function(filepath = NA, excel_name = NA, keep_file = F){

    if(is.na(filepath)){stop(paste0("Must specify a filepath where xlsx lives."))
    } else if(!file.exists(filepath)){
      stop(paste0("Specified file path does not exist. ",
                  ifelse(grepl("sharepoint", filepath), " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))}
    if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")} # add / to end of filepath if doesn't exist

    if(!file.exists(paste0(filepath, excel_name)))stop(
      "Specified filepath and excel_name can't be found. Make sure your filepath and excel_name are spelled correctly.")

    stopifnot(class(keep_file) %in% 'logical')

    if(!requireNamespace("hms", quietly = TRUE)){
      stop("Package 'hms' needed for this function to work. Please install it by running install.packages('hms').",
           call. = FALSE)
    }

    if(!requireNamespace("readxl", quietly = TRUE)){
      stop("Package 'readxl' needed for this function to work. Please install it by running install.packages('readxl').",
           call. = FALSE)
    }

    # if(!requireNamespace("writexl", quietly = TRUE)){
    #   stop("Package 'writexl' needed for this function to work. Please install it by running install.packages('writexl').",
    #        call. = FALSE)
    # }

  options(scipen = 100)

  rawd <- readxl::read_excel(paste0(filepath, excel_name))

  site_type <-
  if(any(names(rawd) %in% "Stage_GageReading")){"lake"
  } else if(any(names(rawd) %in% "Flow_Discharge_cfs")){"stream"
  } else {'unknown'}

  if(site_type == "unknown"){stop("Can't determine whether the raw data is a lake or stream based on column names.")}

  cleand <-
    if(site_type == "lake"){
      rawd |>
        mutate(
          Stage_GageReading = ifelse(Stage_DatumName == "No measurement" & Stage_GageReading == 0, NA_real_, Stage_GageReading),
          Stage_Time = ifelse(hms::as_hms(Stage_Time) == hms::as_hms("00:00:00"), NA, Stage_Time),
          WaterSample_Time = ifelse(hms::as_hms(WaterSample_Time) == hms::as_hms("00:00:00"), NA, WaterSample_Time),
          WaterSample_QC_Time = ifelse(hms::as_hms(WaterSample_QC_Time) == hms::as_hms("00:00:00"), NA, WaterSample_QC_Time),
          WaterSample_QC_IBWExp = ifelse(WaterSample_QC_IBWExp == "1/1/0001", NA, WaterSample_QC_IBWExp),
        )
    } else if(site_type == "stream"){
      rawd |>
        mutate(
          Flow_Discharge_cfs = ifelse(Flow_Method_desc == "No measurement" & Flow_Discharge_cfs == 0, NA_real_, Flow_Discharge_cfs),
          Flow_Discharge_calc = ifelse(Flow_Method_desc == "No measurement" & Flow_Discharge_calc == 0, NA_real_, Flow_Discharge_calc),
          Flow_AvgVel = ifelse(Flow_Method_desc == "No measurement" & Flow_AvgVel == 0, NA_real_, Flow_AvgVel),
          Flow_TotalWidth = ifelse(Flow_Method_desc == "No measurement" & Flow_TotalWidth == 0, NA_real_, Flow_TotalWidth),
          Flow_TotalArea = ifelse(Flow_Method_desc == "No measurement" & Flow_TotalArea == 0, NA_real_, Flow_TotalArea),
          FlowTrackerTemp = ifelse(Flow_Method_desc == "No measurement" & FlowTrackerTemp == 0, NA_real_, FlowTrackerTemp),
          `tbl_Stage::Stage_Reading_1` = ifelse(`tbl_Stage::Stage_DatumName` == "No measurement" & `tbl_Stage::Stage_Reading_1` == 0, NA_real_, `tbl_Stage::Stage_Reading_1`),
          `tbl_Stage::Stage_Reading_2` = ifelse(`tbl_Stage::Stage_DatumName` == "No measurement" & `tbl_Stage::Stage_Reading_2` == 0, NA_real_, `tbl_Stage::Stage_Reading_2`),
          WaterSample_Time = ifelse(hms::as_hms(WaterSample_Time) == hms::as_hms("00:00:00"), NA_real_, WaterSample_Time),
          WaterSample_QC_Time = ifelse(hms::as_hms(WaterSample_QC_Time) == hms::as_hms("00:00:00"), NA_real_, WaterSample_QC_Time),
          WaterSample_QC_IBWExp = ifelse(WaterSample_QC_IBWExp == "1/1/0001", NA_real_, WaterSample_QC_IBWExp),
          `tbl_Stage::Stage_Time_1` = ifelse(hms::as_hms(`tbl_Stage::Stage_Time_1`) == hms::as_hms("00:00:00"), NA_real_, `tbl_Stage::Stage_Time_1`),
          `tbl_Stage::Stage_Time_2` = ifelse(hms::as_hms(`tbl_Stage::Stage_Time_2`) == hms::as_hms("00:00:00"), NA_real_, `tbl_Stage::Stage_Time_2`),
        )

    }

  cleaned_data <- cleand |> filter(!is.na(pk_EventCode))
  # change .. to :: if that's important for the import
  #names(cleaned_data) <- gsub("\\..", "::", names(cleaned_data))

  out_name <- paste0(filepath, substr(excel_name, 1, nchar(excel_name)-5), "_cleaned_",
                     format(Sys.Date(), "%Y%m%d"), ".csv") #.xlsx")

  if(keep_file == TRUE){assign("cleaned_data", cleaned_data, envir = .GlobalEnv)}

  print(paste0("Cleaned file saved to: ", out_name))

  #writexl::write_xlsx(cleaned_data, out_name)
  write.csv(cleaned_data, out_name, row.names = F)

  }
