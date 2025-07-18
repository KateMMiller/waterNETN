% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importData.R
\name{importData}
\alias{importData}
\title{importData: Imports NETN water data package}
\usage{
importData(type = c("DSN"), odbc = "NETNWQ_DP", filepath = NA, new_env = TRUE)
}
\arguments{
\item{type}{Select how to import the data package.
\describe{
\item{"DSN"}{Default. DSN database. If odbc is not specified, will default to NETNWQ_DP.
Using this argument requires that you have a User DSN named NETNWQ_DP that points to the
database containing the data package queries. Note that the database that generates
the data package views must also be linked to the latest NETN WQ backend.}
\item{"dbfile"}{A specified database containing the data package queries for each view. If selected,
must provide the database filepath in the filepath argument.}
\item{"csv"}{Imports the csv version of the data package views. If selected, must provide the
filepath for the csvs in the filepath argument. This option that does not require MS Access and
ODBC driver on your computer.}
\item{"zip"}{Imports the csv versions of the data package views, as a zipped file. If selected,
must provide the filepath and name of the zip file. This option that does not require MS Access and
ODBC driver on your computer.}
}}

\item{odbc}{DSN of the database when using type = DSN. If not specified will default to "NETNWQ_DP",
which should represent the database that generates the data package views.}

\item{filepath}{Quoted filepath where data package database (if type = "dbfile") or the csvs
(if type = "csv" or type = "zip") live.}

\item{new_env}{Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
views in VIEWS_WQ environment. If \code{FALSE}, stores views in global environment}
}
\value{
Assigns water csvs to specified environment
}
\description{
This function imports views in the NETN water data package either as csv files or
queries in the NETN water data package. Each view is added to a VIEWS_WQ environment in
your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
}
\examples{
\dontrun{
library(waterNETN)
# Import tables using default settings of type = "DSN" and odbc = "NETNWQ_DP"
importData()

# Import views from specified database
importData(type = 'dbfile', filepath = "C:/NETN/R_Dev/Water/data/NETN_H2Ov4_DataPackage_202331115.accdb")

# Import data package views
importData(type = 'zip', filepath = "C:/Users/KMMiller/OneDrive - DOI/NETN/R_Dev/Water/data/records-2313941.zip")

# Import views from folder with csvs
importData(type = 'csv', filepath = "C:/Users/KMMiller/OneDrive - DOI/NETN/R_Dev/Water/data/records-2313941")

# Import views from zip file of csvs
importData(type = 'zip',
filepath = "C:/NETN/R_Dev/Water/data/data_package/NETN_Water_data_package_20240927.zip")

}

}
