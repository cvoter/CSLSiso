#' CSLS Sites Dictionary
#'
#' Dictionary for looking up alternate names and identification numbers for
#' sample sites in the CSLS.
#'
#' Raw csv data is processed with the function
#' \code{\link{retrieve_csls_site_dictionary}} from the isoH2Obudget package. No
#' changes are made to the raw csv file in this function.
#'
#' @seealso \code{\link{retrieve_csls_site_dictionary}}
#'
#' @docType data
#'
#' @usage data(site_dictionary)
#'
#' @format A data frame with the following columns:
#' \describe{
#' \item{lake}{name of lake associated with the measurement site. Corresponds
#'             to lake name argument in this function.}
#' \item{obs_type}{type of observation (LK = lake, GW = groundwater
#'                 monitoring well)}
#' \item{site_id}{unique site id for measurement site (e.g., LL-01).
#'                Corresponds to site_id in "isotopes" data frame.}
#' \item{SWIMS_station_id}{SWIMS station id, if exists for this site.}
#' \item{USGS_id}{USGS site number. Corresponds to site_no in "water_levels"
#'                 data frame.}
#' \item{WBIC}{water body identification code (WBIC), for lake sites only.}
#' \item{static_iso_class}{static site type for isotope site classification
#'                         (upgradient, downgradient, precipitation, lake, or
#'                         NA)}
#' }
#'
"site_dictionary"
