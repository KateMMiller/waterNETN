#' @title getSitesLake: query NETN lakes by site
#'
#' @description Queries NETN lakes by site.
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
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)
#'
#' @return Data frame of site info for lakes
#'
#'@examples
#' \dontrun{
#' importData()
#'
#' # get site info for all lakes in lowerNETN
#' lnetn <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")
#' lnetn_lks <- getSitesLake(park = lnetn)
#'
#' # get site info for all lakes in ACAD
#' ACAD_lakes <- getSitesLake(park = 'ACAD')
#'
#' }
#' @export

getSitesLake <- function(park = "all", site = "all", active = TRUE, output = c("short", "verbose")){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  park <- ifelse(park == "LNETN",
                 c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA"), park)
  output <- match.arg(output)
  stopifnot(class(active) == "logical")

  # Check if the views exist and stop if they don't
  env <- if(exists("VIEWS_WQ")){VIEWS_WQ} else {.GlobalEnv}

  tryCatch({lakes <- get("Sites_Lake", envir = env)},
           error = function(e){stop("Water views not found. Please import data.")}
  )

  # Check that a valid site code was used
  site <- tryCatch(match.arg(site, several.ok = TRUE, c("all", unique(lakes$SiteCode))),
                   error = function(e){stop(
                     paste0(
                     "Specified site does not match an accepted lake site code.
                     Acceptable codes are: ",
                     "\n",
                     paste0(sort(lakes$SiteCode), collapse = "\n")))}
                   )

  # Clean up data, so columns are treated correctly
  # character fixes
  chr_cols <- c("SubUnitCode", "SubUnitName", "Datum", "XYAccuracy",
                "Inlet", "Outlet", "TrophicType", "LegislativeClass", "FishMgtType")
  lakes[,chr_cols][lakes[,chr_cols] == "NA"] <- NA_character_

  # numeric fixes
  num_cols <- c("SiteLatitude", "SiteLongitude", "ContribWshedArea_km2", "MaxDepth_m")
  lakes[,num_cols][lakes[,num_cols] == "NA"] <- NA_real_
  lakes[,num_cols] <- apply(lakes[,num_cols], 2, function(x) as.numeric(x))

  # logic fixes
  lakes$IsPointCUI <- as.logical(lakes$IsPointCUI)

  #-- Filter site info --
  # filter on site
  wdata1 <- if(any(site %in% "all")){lakes
    } else {filter(lakes, SiteCode %in% site)
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

  wdata4 <- if(output == "short"){
    wdata3[,c("SiteCode", "SiteName", "UnitCode", "SubUnitCode", "HUC12", "ContribWshedArea_km2",
              "Inlet", "Outlet", "MaxDepth_m", "TrophicType", "LegislativeClass", "FishMgtType",
              "SiteDescription", "Notes")]
  } else {wdata3}

  if(nrow(wdata4) == 0){stop("Returned data frame with no records. Check your park, site, and site_type arguments.")}

  return(data.frame(wdata4))

  }



