#' @title exportData: Export data package views to .csv
#'
#' @description This function exports NETN water package views from the database that generates the views as
#' individual .csv files, or as a zip file with the dataset name (NETN_Water_Data_Package), and the date it
#' was zipped. The exported .csv or .zip files can then be imported via importData(). This function is primarily
#' for internal use to create flat files that users can import to run functions in this package without having
#' a connection having MS Access installed. Must first import the views in the current R session.
#'
#' @param filepath Quoted path to save files to. If not specified, will save to working directory.
#'
#' @param zip Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.
#'
#' @return NETN water database views exported to specified path
#'
#' @examples
#' \dontrun{
#' # RUN FIRST
#' library(waterNETN)
#' importData()
#'
#' # Export csvs to working directory
#' exportData()
#'
#' # Export a zip to the path specified
#' exportData(filepath = "C:/data", zip = TRUE)
#'
#' # Export views as .csvs to specified path
#' exportData(filepath = "C:/data")
#'}
#'
#' @export

exportData <- function(filepath = NA, zip = FALSE){

  # Check that suggested package required for this function are installed
  if(!requireNamespace("zip", quietly = TRUE) & zip == TRUE){
    stop("Package 'zip' needed to export to zip file. Please install it.", call. = FALSE)
  }

  # Make sure all the views are loaded. If anything is missing, function stops.
  view_list <- c("Chemistry_Data", "Discharge_Data", "Event_Info", "Light_Penetration_Data",
                 "Secchi_Data", "Sites_Lake", "Sites_Stream", "Sonde_InSitu_Data",
                 "StageDatum_Info", "StreamSite_Observations", "WaterLevel_Data")

  files <- if(exists("VIEWS_WQ")){ls(envir = VIEWS_WQ)} else {ls()}

  missing <- setdiff(view_list, files)

  if(length(missing) > 0 & length(missing) < length(view_list)){
    stop(paste0("Missing the following views: ", paste0(missing, collapse = ", ")))
  } else if (length(missing) == length(view_list)){
    stop("Views were not detected in your workspace. Please import the data first.")}

  # Error handling for path
  if(is.na(filepath)){filepath <- getwd()
  print(paste0("No filepath specified. Output saved to working directory: ", getwd()), quote = FALSE)
  if(is.na(filepath)){filepath = getwd()}
  if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")} # add / to end of filepath if doesn't exist


  } else if(!dir.exists(filepath)){
    stop("Specified directory does not exist.")
  } else{print(paste0("Output saving to ", filepath), quote = FALSE)}

  # Normalize filepath for zip
  pathn <- normalizePath(filepath)

  # Add / to end of path if it wasn't specified.
  pathn <- if(substr(pathn, nchar(pathn), nchar(pathn)) != "/"){
    paste0(pathn,"\\")} else {(paste0(pathn))}

  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  # Set up envir qualifier
  if(exists("VIEWS_WQ")){env = VIEWS_WQ} else {env = .GlobalEnv}

  # Export files
  if(zip == FALSE){
    #invisible(
      lapply(seq_along(view_list), function(x){
      setTxtProgressBar(pb, x)
      view_name = view_list[x]
      view = get(view_name, envir = env)
      write.csv(view,
                paste0(pathn, view_name, ".csv"),
                row.names = FALSE)
    })#)
  } else if(zip == TRUE){ #create tmp dir to export csvs, bundle to zip, then delete tmp folder

    dir.create(tmp <- tempfile())

    invisible(lapply(seq_along(view_list), function(x){
      setTxtProgressBar(pb, x)
      write.csv(get(view_list[[x]], envir = env),
                paste0(tmp, "\\", view_list[x], ".csv"),
                row.names = FALSE)
      }))

    close(pb)
    file_list <- list.files(tmp)

    zip_name = paste0("NETN_Water_Data_Package_", format(Sys.Date(), "%Y%m%d"), ".zip")

    zip::zipr(zipfile = paste0(pathn, zip_name),
              root = tmp,
              files = file_list)
    # csvs will be deleted as soon as R session is closed b/c tempfile
  }
  noquote(ifelse(zip == FALSE,
                 paste0('Export complete. Data package saved to: ', pathn),
                 paste0('Export complete. Data package saved to: ', pathn, zip_name)))
}


