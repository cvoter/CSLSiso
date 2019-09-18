#' Monthly Lake Surface Temperature
#'
#' This function takes output from the Wisconsin Department of Natural
#' Resources Surface Water Integrated Monitoring System (SWIMS),
#'
#' @references \url{https://dnr.wi.gov/topic/surfacewater/swims/}
#'
#' @param lake name of lake to analyze (capitalized, e.g. "Pleasant")
#' @param SWIMS_file name of SWIMS csv file which includes the following fields:
#' \itemize{
#'   \item DNR Parameter - unique DNR number for parameter. Assume lake
#'         surface temperatures are stored as "10" for "Temperature Field"
#'   \item Result - numeric value of measurement
#'   \item Units - units of result
#'   \item Result Depth - depth at which measurement taken, asume is "0
#'         Meters" for lake surface temperatures
#'   \item WBIC - Wisconsin Water Body Identification Code
#' }
#' @param site_file filname of site dictionary with the following columns
#' \itemize{
#' \item lake - name of lake associated with the measurement site. Corresponds
#'              to lake name argument in this function.
#' \item obs_type - type of observation (LK = lake, GW = groundwater
#'                  monitoring well)
#' \item site_id - unique site id for measurement site (e.g., LL-01).
#'                 Corresponds to site_id in "isotopes" data frame.
#' \item SWIMS_station_id - SWIMS station id, if exists for this site.
#' \item USGS_id - USGS site number. Corresponds to site_no in "water_levels"
#'                 data frame.
#' \item WBIC - water body identification code (WBIC), for lake sites only.
#' }
#' @param filedir directory in which daily weather csv file resides, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#'
#' @return monthly_lst, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{ltmp}{mean monthly lake surface temperature (degrees K)}
#' }
#'
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom lubridate month year
#'
#' @export

get_monthly_lst <- function(lake,
                            SWIMS_file,
                            site_file = "csls_site_dictionary.csv",
                            filedir = 'system.file') {
  # Load all SWIMS data
  if (filedir == 'system.file') {
    SWIMS <- read.csv(system.file("extdata",
                                  SWIMS_file,
                                  package = "isoH2Obudget",
                                  mustWork = TRUE))
    site_dictionary <- read.csv(system.file("extdata",
                                            site_file,
                                            package = "isoH2Obudget",
                                            mustWork = TRUE))
  } else {
    SWIMS <- read.csv(sprintf('%s/%s', filedir, SWIMS_file))
    site_dictionary <- read.csv(sprintf("%s/%s", filedir, site_file))
  }

  # Get WBIC for lake
  WBIC <- site_dictionary %>%
          filter(.data$lake == !!lake,
                 .data$obs_type == "LK") %>%
          select(.data$WBIC) %>%
          as.integer()

  # Subset to get just surface temps for just this lake
  submonthly_lst           <- SWIMS %>%
                              filter(.data$DNR.Parameter == 10,
                                     .data$Result.Depth == "0 Meters",
                                     .data$WBIC == !!WBIC) %>%
                              select(.data$Start.Date.Time,
                                     .data$Result,
                                     .data$Units)
  colnames(submonthly_lst) <- c("date","ltmp","units")
  submonthly_lst$date      <- as.Date(submonthly_lst$date, format = "%m/%d/%Y")
  submonthly_lst$ltmp      <- as.numeric(as.character(submonthly_lst$ltmp))

  # Make sure data are in expected units of degrees C
  if (length(unique(submonthly_lst$units)) != 1) {
    warning("Warning: Temperature units are not all the same in SWIMS subset")
  } else if (unique(submonthly_lst$units) != "DEGREES C") {
    warning("Warning: Temperature units are not 'DEGREES C'")
  }

  # Extract monthly lake surface temperatures
  month_info <- start_n_months(submonthly_lst$date, all_days = FALSE)
  monthly_lst <- NULL
  for (i in 1:month_info$nmonths) {
    this_month <- month_info$start_date + months(i-1)
    m          <- month(this_month)
    y          <- year(this_month)

    these_lst  <- submonthly_lst[which(month(submonthly_lst$date) == m &
                                         year(submonthly_lst$date) == y),]
    monthly_lst$date[[i]] <- this_month
    monthly_lst$ltmp_K[i] <- mean(these_lst$ltmp, na.rm = TRUE) + 273.15
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_lst$date <- as.Date(monthly_lst$date, origin = "1970-01-01")

  return(as.data.frame(monthly_lst))
}
