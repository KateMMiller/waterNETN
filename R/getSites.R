#' @title getSites: query NETN water data by site
#'
#' @description Queries NETN water data by site. Returned data frame can include lakes or streams, but only includes
#' columns that the Site_Lake and Site_Stream views have in common. For full site data, use the site-specific getSites()
#' functions (i.e., getSitesLakes()). This function is a useful building block within other package functions. Must
#' have water quality views imported in R session.
#'
#' @importFrom dplyr filter
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"LNETN"}{Includes all parks but ACAD}
#' \item{"ACAD"}{Acadia NP only}
#' \item{"MABI"}{Marsh-Billings-Rockefeller NHP only}
#' \item{"MIMA"}{Minute Man NHP only}
#' \item{"MORR"}{Morristown NHP only}
#' \item{"ROVA"}{Roosevelt-Vanderbilt NHS only}
#' \item{"SAGA"}{Saint-Gaudens NHP only}
#' \item{"SAIR"}{Saugus Iron Works NHS only}
#' \item{"SARA"}{Saratoga NHP only}
#' \item{"WEFA"}{Weir Farm NHP only}}
#'
#' @param site Filter on 6-letter SiteCode (e.g., "ACABIN", "MORRSA", etc.). Easiest way to pick a site. Defaults to "all".
#'
#' @param site_type Combine all site types, lakes or streams. Not needed if specifying particular sites.
#' \describe{
#' \item{"all"}{Default. Includes all site types, unless site or site_name select specific site types.}
#' \item{"lake"}{Include only lakes.}
#' \item{"stream"}{Include streams only.}
#' }
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @return Data frame of site info
#'
#'@examples
#' \dontrun{
#' importData()
#'
#' # get site info for all sites in MABI
#' mabi <- getSites(park = "MABI")
#'
#' # get site info for all streams in ACAD
#' ACAD_streams <- getSites(park = 'ACAD', site_type = 'stream')
#'
#' # get site info East Primrose Brook in MORR
#' east_prim <- getSites(site = "MORRSA")
#' }
#' @export

getSites <- function(park = "all", site = "all", site_type = c("all", "lake", "stream"),
                     active = TRUE,
                     output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
 if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}

  site_type <- match.arg(site_type)
  output <- match.arg(output)
  stopifnot(class(active) == "logical")

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({streams <- get("Sites_Stream", envir = env)
            streams$site_type = "stream"
           },
           error = function(e){stop("Water views not found. Please import data.")}
  )

  tryCatch({lakes <- get("Sites_Lake", envir = env)
            lakes$site_type = "lake"
           },
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Check that a valid site code was used
  sites <- rbind(streams[,c("SiteCode", "SiteName")], lakes[,c("SiteCode", "SiteName")])

  if(!all(site %in% c("all", unique(sites$SiteCode)))){
    stop(paste0("Specified site does not match an accepted site code. Acceptable codes are:",
                "\n",
                paste0(sort(sites$SiteCode), collapse = ", ")))
  }

  # site <- tryCatch(match.arg(site, several.ok = TRUE, c("all", unique(sites$SiteCode))),
  #                  error = function(e){stop(
  #                    paste0(
  #                    "Specified site does not match an accepted site code.
  #                    Acceptable codes are: ",
  #                    "\n",
  #                    paste0(sort(sites$SiteCode), collapse = "\n")))}
  #                  )

  #-- Filter site info --
  site_cols <- intersect(names(streams), names(lakes))

  # Start with largest dataset, based on which site type is chosen and select only
  # columns in common.
  wdata_full <-
    if(any(site_type == "all")){rbind(streams[ , site_cols], lakes[ , site_cols])
    } else if(any(site_type == "lake")){lakes[ , site_cols]
    } else if(any(site_type == "stream")){streams[ , site_cols]
    }

  # filter on site
  wdata1 <- if(any(site %in% "all")){wdata_full
    } else {filter(wdata_full, SiteCode %in% site)
    }

  # filter on park
  wdata2 <- if(any(park %in% "all")){wdata1
    } else {filter(wdata1, UnitCode %in% park)
    }

  # filter on active. Currently hard coded until data package includes it
  inactive = c("ACBUBO", "ACEGLO", "ACJRDO", "ACMOWB", "MORRSA", "MORRSC", "ROVASC", "SARASB", #streams
               "ACDKPD", "ACLPIH", "ACTARN", "ROVAPA") # lakes
  wdata3 <-
  if(active == TRUE){
    filter(wdata2, !SiteCode %in% inactive)
  } else {wdata2}

  if(nrow(wdata3) == 0){stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  # Clean up data, so columns are treated correctly
  # character fixes
  chr_cols <- c("SubUnitCode", "SubUnitName", "Datum", "XYAccuracy",
                "LegislativeClass")
  wdata3[,chr_cols][wdata3[,chr_cols] == "NA"] <- NA_character_

  # numeric fixes
  num_cols <- c("SiteLatitude", "SiteLongitude", "ContribWshedArea_km2")
  wdata3[,num_cols][wdata3[,num_cols] == "NA"] <- NA_real_
  wdata3[,num_cols] <- apply(wdata3[,num_cols], 2, function(x) as.numeric(x))

  # logic fixes
  wdata3$IsPointCUI <- as.logical(wdata3$IsPointCUI)

  return(data.frame(wdata3))

  }



