#' CSLS Lake Surface Temperature Measurements
#'
#' Every-other-week measurments of lake surface temperature at the CSLS lakes
#' (Plainfield Lake, Long Lake, and Pleasant Lake).
#'
#' The raw data is sourced from the Wisconsin Department of Natural Resources
#' (DNR) Surface Water Integrated Management System (SWIMS). The raw csv file
#' contains all measurements associated with the project "Central Sands Lake
#' Study" or "csls".
#'
#' Raw csv data is processed with the function \code{\link{retrieve_csls_lst}}
#' from the isoH2Obudget package. This function subsets only those SWIMS
#' measurements with the DNR Parameter "10" for"Temperature Field" with Result
#' Depth of "0 Meters".
#'
#' @seealso \code{\link{retrieve_csls_lst}}
#'
#' @docType data
#'
#' @usage data(lst)
#'
#' @format A data frame with the 78 observations of 4 variables:
#' \describe{
#' \item{WBIC}{Water Body Identification Code for lake}
#' \item{date}{date and time of observation}
#' \item{ltmp}{lake surface temperature (degrees C)}
#' \item{units}{units of lake surface temperature, should all be "DEGREES C"}
#' }
#'
#' @source \url{https://dnr.wi.gov/topic/surfacewater/swims/}
"lst"
