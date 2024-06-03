#' @title importData: Imports NETN water data package
#'
#' @description This function imports views in the NETN water data package either as csv files or
#' queries in the NETN water data package. Each view is added to a VIEWS_WQ environment in
#' your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @param type Select how to import the data package.
#' \describe{
#' \item{"DSN"}{Default. DSN database. If odbc is not specified, will default to NETNWQ_DP.
#' Using this argument requires that you have a User DSN named NETNWQ_DP that points to the
#' database containing the data package queries. Note that the database that generates
#' the data package views must also be linked to the latest NETN WQ backend.}
#' \item{"dbfile"}{A specified database containing the data package queries for each view. If selected,
#' must provide the database filepath in the filepath argument.}
#' \item{"csv"}{Imports the csv version of the data package views. If selected, must provide the
#' filepath for the csvs in the filepath argument. This option that does not require MS Access and
#' ODBC driver on your computer.}
#' \item{"zip"}{Imports the csv versions of the data package views, as a zipped file. If selected,
#' must provide the filepath and name of the zip file. This option that does not require MS Access and
#' ODBC driver on your computer.}
#' }
#'
#' @param odbc DSN of the database when using type = DSN. If not specified will default to "NETNWQ_DP",
#' which should represent the database that generates the data package views.
#'
#' @param filepath Quoted filepath where data package database (if type = "dbfile") or the csvs
#' (if type = "csv" or type = "zip") live.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_WQ environment. If \code{FALSE}, stores views in global environment
#'
#' @examples
#' \dontrun{
#' # Import tables using default settings of type = "DSN" and odbc = "NETNWQ_DP"
#' importData()
#'
#' # Import views from specified database
#' importData(type = 'dbfile', filepath = "C:/NETN/R_Dev/Water/data/NETN_H2Ov4_DataPackage_202331115.accdb")
#'
#' # Import views from folder with csvs
#' importData(type = 'csv', filepath = "C:/NETN/R_Dev/Water/data/data_package/")
#'
#' # Import views from zip file of csvs
#' importData(type = 'zip',
#' filepath = "C:/NETN/R_Dev/Water/data/data_package/NETN_WQ_data_package_20240227.zip")
#'
#' }
#'
#' @return Assigns water csvs to specified environment
#' @export

importData <- function(type = c("DSN", "dbfile", "csv", "zip"),
                       odbc = "NETNWQ_DP", filepath = NA, new_env = TRUE){

  #-- Error handling --
  type <- match.arg(type)
  stopifnot(class(new_env) == 'logical')

  # check that filepath was specified for non-DSN options
  if(type %in% c("dbfile", "csv", "zip")){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when type = '",
                                type, "' option."))}
    }

  if(type == 'csv'){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when export = TRUE"))
    } else if(!file.exists(filepath)){
      stop(paste0("Specified file path does not exist. ",
                  ifelse(grepl("sharepoint", filepath), " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))}
    if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")}} # add / to end of filepath if doesn't exist

  # Check if type = 'csv' was specified, but .zip file is filepath
  if(type == 'csv' & grepl(".zip", filepath)){stop("Specified a zip file in filepath. Must use type = 'zip' instead of 'csv'.")}

  # check for required packages for certain arguments
  if(!requireNamespace("odbc", quietly = TRUE) & type %in% c('DSN', 'dbfile')){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE) & type %in% c('DSN', 'dbfile')){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }
  # if(!requireNamespace("zip", quietly = TRUE) & type == 'zip'){
  #   stop("Package 'zip' needed to export to zip file. Please install it.", call. = FALSE)
  # }

  # Create new environment if new_env = T or set env as Global
  if(new_env == TRUE){VIEWS_WQ <<- new.env()}
  env <- if(new_env == TRUE){VIEWS_WQ} else {.GlobalEnv}

  # Vector of file names in filepath that end in .csv (ie the data package views)
  wq_views <- c("Chemistry_Data_Long", "Discharge_Data", "Event_Info", "Light_Penetration_Data",
                "Secchi_Data_Long", "Sites_Lake", "Sites_Stream", "Sonde_InSitu_Data_Long",
                "StageDatum_Info", "StreamSite_Observations", "WaterLevel_Data")

  #-- Import from database --
  # make sure db is on dsn list if type == DSN
  if(type == "DSN"){
    dsn_list <- odbc::odbcListDataSources()
  if(!any(dsn_list$name %in% odbc)){
    stop(paste0("Specified DSN ", odbc, " is not a named database source." ))}
  }

  # Connect to database
  if(type == "DSN"){
    tryCatch(
      db <- DBI::dbConnect(drv = odbc::odbc(), dsn = odbc),
      error = function(e){stop(paste0("Unable to connect to specified DSN."))})
  }

  if(type == 'dbfile'){
    tryCatch(
     db <- DBI::dbConnect(drv = odbc::odbc(),
                           .connection_string =
                             paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", filepath)),
     error = function(e){stop(paste0("Unable to connect to specified database."))})
  }

  # Import database tables
  if(type %in% c("DSN", "dbfile")){

  pb = txtProgressBar(min = 0, max = length(wq_views), style = 3)

  tbl_import <- lapply(seq_along(wq_views),
                       function(x){
                         setTxtProgressBar(pb, x)
                         tab1 <- wq_views[x]
                         tab <- dplyr::tbl(db, tab1) |> dplyr::collect() |> as.data.frame()
                         return(tab)
                       })

  DBI::dbDisconnect(db)

  # Name tbl_import list of tables
  tbl_import <- setNames(tbl_import, wq_views)

  # Add list of tables in tbl_import to specified environment
  list2env(tbl_import, envir = env) # all tables into fxn env
  }

  #-- Import from csvs --
  if(type == "csv"){
  # List csvs in filepath folder
  dp_list <- list.files(filepath, pattern = ".csv")
  # Drop csvs that don't matching names in the wq_views
  dp_list <- dp_list[grepl(paste0(wq_views, collapse = "|"), dp_list)]
  # Drop date stamp (if it exists) from file name if exists in 2 steps
  dp_list_names <- gsub("[[:digit:]]+|.csv", "", dp_list)
  dp_list_names <- gsub("_$","", dp_list_names)

  miss_vws <- setdiff(dp_list_names, wq_views)

  # Check for missing views
  if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                paste0(miss_vws, collapse = ", "))}

  if(length(dp_list) > 11){
    stop(
    "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'Chemistry_Data'). Must specify a filepath that only contains 1 version of each view.")
    }

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(dp_list), style = 3)

  # Import the file names by applying read.csv to the dp_list of file names
  # This will return one list that includes all the datasets as individual elements
  # The na.string = NA converts "NA" in data to blanks. The check.names = F won't
  # replace invalid characters (eg "+") with "."
  dp_files <- lapply(seq_along(dp_list),
                function(x){
                  fname = dp_list[[x]]
                  setTxtProgressBar(pb, x)
                  read.csv(paste0(filepath, fname),
                           na.string = "NA",
                           tryLogical = TRUE,
                           check.names = FALSE)
         })

  # Set the names of dp_files as the shorter dp_list2 names
  dp_files <- setNames(dp_files, dp_list_names)

  # Takes every element of the dp_files list and saves it to the VIEWS_WQ or global
  # environment as separate, named objects.
  list2env(dp_files, envir = env)

  # Close progress bar
  close(pb)
  }

  if(type == "zip"){
    # Check if can read files within the zip file
    tryCatch(
        {zfiles = utils::unzip(filepath, list = T)$Name
        },
        error = function(e){stop(paste0("Unable to import specified zip file."))})

    z_list = zfiles[grepl(paste0(wq_views, collapse = "|"), zfiles)]

    # Drop date stamp (if it exists) from file name if exists in 2 steps
    z_list_names <- gsub("[[:digit:]]+|.csv", "", z_list)
    z_list_names <- gsub("./", "", z_list_names)
    z_list_names <- gsub("_$","", z_list_names)

    miss_vws <- setdiff(z_list_names, wq_views)

    # Check for missing views
    if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                  paste0(miss_vws, collapse = ", "))}

    if(length(z_list) > 11){
      stop(
        "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'Chemistry_Data'). Must specify a filepath that only contains 1 version of each view.")
    }

    # Since the missing test passed, clean up files so only includes names in view_list, but
    # maintain order in files

    # Import views now that all tests passed
    pb <- txtProgressBar(min = 0, max = length(z_list), style = 3)

    wqviews <- unzip(filepath, junkpaths = TRUE, exdir = tempdir())

    view_import <-
      lapply(seq_along(wqviews), function(x){
             setTxtProgressBar(pb,x)
             read.csv(wqviews[x], na.string = "NA", check.names = FALSE)})

    view_import <- setNames(view_import, z_list_names)
    list2env(view_import, envir = env)
     # Close progress bar
     close(pb)
   }

  # Print message in console
  print(ifelse(new_env == TRUE,
               paste0("Import complete. Views are located in VIEWS_WQ environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)
}
