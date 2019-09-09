#' Monthly Lake Surface Temperature
#'
#' This function takes output from the Wisconsin Department of Natural
#' Resources Surface Water Integrated Monitoring System (SWIMS),
#'
#' @references \url{https://dnr.wi.gov/topic/surfacewater/swims/}
#'
#' @param filename name of SWIMS csv file which includes the following fields:
#' \itemize{
#'   \item DNR Parameter - unique DNR number for parameter. Assume lake
#'         surface temperatures are stored as "10" for "Temperature Field"
#'   \item Result - numeric value of measurement
#'   \item Units - units of result
#'   \item Result Depth - depth at which measurement taken, asume is "0
#'         Meters" for lake surface temperatures
#'   \item WBIC - Wisconsin Water Body Identification Code
#' }
#' @param filedir directory in which daily weather csv file resides, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#' @param wbic Wisconsin Water Body Identification Code for lake of interest
#'
#' @return monthly_lst, a data frame with the following columns:
#' \describe{
#' \item{date}{first of the month for each monthly observation (Date)}
#' \item{ltmp}{mean monthly lake surface temperature (degrees C)}
#' }
#'
#' @importFrom lubridate day month year
#' @importFrom utils read.csv
#' @importFrom rlang .data
#'
#' @export

get_monthly_lst <- function(filename,
                            filedir = 'system.file',
                            wbic = 106900) {
  # Load all SWIMS data
  if (filedir == 'system.file') {
    SWIMS <- read.csv(system.file("extdata",
                                  filename,
                                  package = "isoH2Obudget",
                                  mustWork = TRUE))
  } else {
    SWIMS <- read.csv(sprintf('%s/%s', filedir, filename))
  }

  # Subset to get just surface temps for just this lake
  submonthly_lst <- subset(SWIMS,
                           select = c(.data$Start.Date.Time,
                                      .data$Result,
                                      .data$Units),
                           .data$DNR.Parameter == 10 &
                             .data$Result.Depth == "0 Meters" &
                             .data$WBIC == wbic)
  colnames(submonthly_lst) <- c("date","ltmp","units")
  submonthly_lst$date <- as.Date(submonthly_lst$date, format = "%m/%d/%Y")
  submonthly_lst$ltmp <- as.numeric(as.character(submonthly_lst$ltmp))

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

    these_lst <- submonthly_lst[which(month(submonthly_lst$date) == m &
                                        year(submonthly_lst$date) == y),]
    monthly_lst$date[[i]] <- this_month
    monthly_lst$ltmp[i] <- mean(these_lst$ltmp, na.rm = TRUE)
  }

  # R bizzarly looses the class of date objects in for loops, fix here
  monthly_lst$date <- as.Date(monthly_lst$date, origin = "1970-01-01")

  return(as.data.frame(monthly_lst))
}
