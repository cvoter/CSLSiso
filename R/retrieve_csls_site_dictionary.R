#' Retrieve Central Sands Sites Dictionary
#'
#' This function loads the site dictionary for CSLS.
#'
#' @param filename filname of site dictionary with the following columns
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
#' @param filedir name of directory with both csv files, defaults to
#'                'system.file' to instruct it to look within package
#'                "inst/extdata" directory.
#'
#' @return site_dictionary, a data frame with the following columns:
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
#' }
#'
#' @export

retrieve_csls_site_dictionary <- function(filename, filedir = 'system.file') {
  site_dictionary <- load_pkg_csv(filename, filedir)

  return(site_dictionary)
}
