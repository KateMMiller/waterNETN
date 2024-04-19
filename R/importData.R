#' @title importData: Imports NETN water data package
#'
#' @description This function imports flat files as csvs in the NETN water data package.
#' Each view is added to a VIEWS_WQ environment in your workspace, or to your global
#' environment based on whether new_env = TRUE or FALSE.
#'
#' @param path Quoted path where data package csvs live. Note that currently any csv in
#' the path provided will import. We will tighten this up once we make the data package into a zip file.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_WQ environment. If \code{FALSE}, stores views in global environment
#'
#' @examples
#' \dontrun{
#' # Import tables using default settings
#' importData(path = "C:/NETN/Water/data")
#'
#' # Import tables into global environment
#' importData(path = "C:/NETN/Water/data", new_env = FALSE)
#' }
#'
#' @return Assigns water csvs to specified environment
#' @export

importData <- function(path = NA, new_env = TRUE){

  # Error handling
  stopifnot(class(new_env) == 'logical')
  if(is.na(path)){stop("Must specify path of data package files.")}
  if(!dir.exists(path)){stop("Specified path does not exist.")}
  path <- if(!grepl("/$", path)){paste0(path, "/")}

  # create new environment if new_env = T or set env as Global
  if(new_env == TRUE){VIEWS_WQ <<- new.env()}
  env <- if(new_env == TRUE){VIEWS_WQ} else {.GlobalEnv}

  # Create vector of file names in path that end in .csv (ie the data package views)
  dp_list <- list.files(path, pattern = ".csv")

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(dp_list), style = 3)

  # Import the file names by applying read.csv to the dp_list of file names
  # This will return one list that includes all the datasets as individual elements
  dp_files <- lapply(seq_along(dp_list),
                function(x){
                  fname = dp_list[[x]]
                  setTxtProgressBar(pb, x)
                  read.csv(paste0(path, fname))
         })

  # Drop everything after the 2nd "_" in the file name
  dp_list2 <- sub("([A-Za-z]+_[A-Za-z]+).*", "\\1", dp_list)

  # Set the names of dp_files as the shorter dp_list2 names
  dp_files <- setNames(dp_files, dp_list2)

  # Takes every element of the dp_files list and saves it to the VIEWS_WQ or global
  # environment as separate, named objects.
  list2env(dp_files, envir = env)

  # Close progress bar
  close(pb)

  # Print message in console
  print(ifelse(new_env == TRUE,
               paste0("Import complete. Views are located in VIEWS_WQ environment."),
               paste0("Import complete. Views are located in global environment.")), quote = FALSE)
}
