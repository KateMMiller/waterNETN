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

  if(new_env == TRUE){VIEWS_WQ <<- new.env()}
  env <- if(new_env == TRUE){VIEWS_WQ} else {.GlobalEnv}

  dp_list <- list.files(path, pattern = ".csv")

  dp_files <- lapply(seq_along(dp_list),
                function(x){
                  fname = dp_list[[x]]
                  print(fname)
                  read.csv(paste0(path, fname))
         })

  dp_files <- setNames(dp_files, dp_list)
  list2env(dp_files, envir = env)

}
