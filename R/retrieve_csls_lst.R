#' Retrieve Central Sands Lake Surface Temperature
#'
#' This function retrieves every-other-week measurements of lake surface
#' temperatures associated with CSLS from SWIMS. Extracts only DNR Parameter 10
#' with Result Depth of "0 Meters".
#'
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
#' @param filedir directory in which weather csv file resides, defaults to
#'                'system.file' to instruct it to look within installed package
#'                files.
#'
#' @return lst, a data frame with the following columns:
#' \describe{
#' \item{WBIC}{Water Body Identification Code for lake}
#' \item{date}{date and time of observation}
#' \item{ltmp}{lake surface temperature (degrees C)}
#' \item{units}{units of lake surface temperature, should all be "DEGREES C"}
#' }
#'
#' @importFrom lubridate mdy_hm
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#'
#' @export

retrieve_csls_lst <- function(filename, filedir = "system.file") {
  # Load all SWIMS data
  SWIMS         <- load_pkg_csv(filename, filedir)

  # Subset lake surface temperature
  lst           <- SWIMS %>%
                   filter(.data$DNR.Parameter == 10,
                          .data$Result.Depth == "0 Meters") %>%
                   select(.data$WBIC,
                          .data$Start.Date.Time,
                          .data$Result,
                          .data$Units)
  colnames(lst) <- c("WBIC", "date", "ltmp","units")

  lst$date      <- mdy_hm(lst$date)
  lst$ltmp      <- as.numeric(as.character(lst$ltmp))

  # Data QC
  if (length(unique(lst$units)) != 1) {
    warning("Warning: Temperature units for lake surface temperature from SWIMS are not all the same")
  } else if (unique(lst$units) != "DEGREES C") {
    warning("Warning: Temperature units for lake surface temperature from SWIMS are not 'DEGREES C'")
  }

  return(lst)
}
